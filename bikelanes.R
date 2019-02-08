library(rvest)
library(RCurl)
library(httr)
library(xml2)
library(tidyverse)
library(lubridate)

baseurls <- paste0("https://311.boston.gov/?page=",c(1:20),"&q=bike+lane+park&utf8=%E2%9C%93")

all_reqs <- NULL
for(i in 1:length(baseurls)){
	get_object <- try(GET(baseurls[i], timeout(10), user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/600.5.17 (KHTML, like Gecko) Version/8.0.5 Safari/600.5.17")))
	if(class(get_object)[1] == 'try-error'){
		timeouts <- c(timeouts,i)
		next()
	}
	if(get_object$status_code == 490 | get_object$status_code == 0 | get_object$status_code == 550){
		timeouts <- c(timeouts,i)
		next()
	}

	html_page <- try(read_html(get_object))

	# extract the links after the "a href" tags
	nodes <- html_page %>%
		html_nodes(".report") 
	
	links <- nodes %>% # 
		html_attr("onclick") %>%
		gsub("location.href=\'(.*)\'.*","\\1",.) %>%
		paste0("https://311.boston.gov",.)
	
	# extract the link text after the "a href" tags
	titles <- nodes %>%
		html_nodes(".report-title") %>%
		html_text(trim = T) 
	
	# For individual cases, follow link and gather info
	for(j in 1:length(links)){
		get_object <- try(GET(links[j], timeout(10), user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/600.5.17 (KHTML, like Gecko) Version/8.0.5 Safari/600.5.17")))
		
		html_req <- try(read_html(get_object))
		
		req_nodes <- html_req %>%
			html_nodes("#content p")
		
		address <- req_nodes %>%
			html_text() %>%
			grep("address",.,value=T) %>%
			gsub("address: (.*)$","\\1",.)
		
		lat <- req_nodes %>%
			html_text() %>%
			grep("coordinates lat,lng:",.,value=T) %>%
			gsub("coordinates lat,lng: (.*?), (.*)$","\\1",.) %>%
			str_trim()
		
		long <- req_nodes %>%
			html_text() %>%
			grep("coordinates lat,lng:",.,value=T) %>%
			gsub("coordinates lat,lng: (.*?), (.*)$","\\2",.) %>%
			str_trim()
		
		descr <- html_req %>%
			html_nodes("blockquote") %>%
			html_text() %>%
			str_trim()

		status_table <- html_req %>%
			html_nodes(css ="#notes-tab > table") %>%
			# html_attrs() %>%
			# html_text()
			html_table(fill=T)
			
		status_table <- status_table[[1]]
		
		date_opened <- as_datetime(status_table$Timestamp[grep("Opened",status_table$Description)],
															 format="%a %b %d, %Y %I:%M%p")
		if(length(date_opened)<1){
			date_opened <- as_datetime(status_table$Timestamp[grep("Submitted",status_table$Description)],
																 format="%a %b %d, %Y %I:%M%p")
		}
		if(length(date_opened)<1){
			date_opened <- NA
		}

		date_closed <- as_datetime(status_table$Timestamp[grep("Closed",status_table$Description)],
															 format="%a %b %d, %Y %I:%M%p")
		if(length(date_closed)<1){
			date_closed <- NA
		}
		
		closed_status <- gsub("Closed with status: (.*)","\\1",grep("Closed with status: ",status_table$Description,value=T))
		if(length(closed_status)<1){
			closed_status <- NA
		}
		
		this_req <- as.tibble(data.frame(title=titles[j],link=links[j],address,lat,long,date_opened,date_closed,descr,closed_status))
		
		all_reqs <- bind_rows(all_reqs,this_req)
	}

}

all_reqs$lat <- as.numeric(all_reqs$lat)
all_reqs$long <- as.numeric(all_reqs$long)

write.csv(all_reqs,paste0("bikelane-requests/bikelane_311_",Sys.Date(),".csv"))

# combine with all other scraped files:
all_files <- list.files("bikelane-requests/",full.names = T)

all_reqs_dates <- NULL
for(k in 1:length(all_files)){
	this_reqs <- read_csv(all_files[k])
	all_reqs_dates <- bind_rows(all_reqs_dates,all_reqs_dates)
}
all_reqs_dates <- unique(all_reqs_dates)

## Mapping these requests:
library(RgoogleMaps) # googlemaps API tool
library(ggmap) 
api_key <- readLines("google.api")
ggmap::register_google(key=api_key)
library(sf)


bike_reqs <- st_as_sf(all_reqs,coords = c("long","lat"))
st_crs(bike_reqs) <- "+proj=longlat +datum=WGS84"

st_bbox(bike_reqs)
boston_bbox <- c(st_bbox(bike_reqs)[1],
								 st_bbox(bike_reqs)[2],
								 st_bbox(bike_reqs)[3],
								 st_bbox(bike_reqs)[4])
names(boston_bbox) <- c("left","bottom","right","top")

### nicer plots with basemaps:
boston_base <- get_map(location="Boston Latin Academy, Boston, MA",zoom=12, color="color",maptype="roadmap",crop=T,source="google")
b0 <- ggmap(boston_base, extent="device",legend="none",maprange=T)

boston_base_zoom <- get_map(location="Berklee School of Music, Boston, MA",zoom=16, color="color",maptype="roadmap",crop=T,source="google")
b0_zoom <- ggmap(boston_base_zoom, extent="device",legend="none",maprange=T)

boston_base_stamen <- get_stamenmap(bbox = boston_bbox,zoom=14, maptype="terrain-lines",crop=T)
b0_stamen <- ggmap(boston_base_stamen, extent="device",legend="none",maprange=T)

(b1_zoom <- b0_zoom + 
		geom_sf(data=bike_reqs,color = "red",inherit.aes = F)
)	
ggsave(b1_zoom,filename = "Figures/blocked_bikelanes_berklee_190207.pdf",height=8,width=8)

(b1_all <- b0_stamen + 
		geom_sf(data=bike_reqs,color = "red",inherit.aes = F,size=0.8) + 
		theme(axis.line=element_blank(),axis.text.x=element_blank(),
					axis.text.y=element_blank(),axis.ticks=element_blank(),
					axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					panel.background=element_blank(),panel.border=element_blank(),panel.grid.major = element_line(colour = 'transparent'),
					panel.grid.minor=element_blank(),plot.background=element_blank())
)

ggsave(b1_all,filename = "Figures/blocked_bikelanes_190207.pdf",dpi = 600)
