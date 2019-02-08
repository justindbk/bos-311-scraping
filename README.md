# bos-311-scraping

The City of Boston doesn't let people easily see the user-entered text on 311 requests via their (Open Data portal)[http://data.boston.gov]. I discovered that the user page for viewing completed requests allows for public access via an unintended search feature. This (backdoor page)[https://311.boston.gov/] only lets viewer see last 200 requests or max of 200 search results, but it's still something.

So far, just using this to try and track people blocking bike lanes with their cars across Boston. Drivers do this a lot and it's a huge safety problem. Used search terms "bike + lane + park" (here)[https://311.boston.gov/?q=bike+lane+park&utf8=%E2%9C%93] and scraped all pages of results. Should automate this to run periodically but that's a task for another time.
