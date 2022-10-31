# 0-L2M-download-archive-updated:
#  Download all the archived L2M reports in raw form to then evaluate:
#  https://official.nba.com/nba-officiating-last-two-minute-reports-archive/
  
  # ---- start --------------------------------------------------------------

library(httr)
library(rvest)
library(tidyverse)

# Create a directory for the data
local_dir   <- "0-data/L2M/archived-pdf"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

url <- paste0("https://official.nba.com/",
              "nba-officiating-last-two-minute-reports-archive/")

# read in url from above, then extract the links that comply with:
links <- read_html(url) %>% 
  html_nodes("#primary a") %>%
  html_attr("href") %>% 
  # Missing some games of Spurs-Clippers, Memphis-Houston, and Indiana-Chicago
  append(c(paste0("http://official.nba.com/wp-content/uploads/",
                  "sites/4/2015/03/L2M-MEM-HOU-3-4-15.pdf"),
           paste0("https://ak-static.cms.nba.com/wp-content/uploads/",
                  c("sites/4/2015/04/L2M-SAS-LAC-4-28-2015.pdf",
                    "sites/4/2015/11/L2M-IND-CHI-11-16-151.pdf",
                    "sites/4/2016/02/L2M-IND-@-MIA-2-22-16-1.pdf",
                    "sites/4/2016/05/L2M-GSW-OKC-5-28-16.pdf"))))

# Remove nonexistent SAS-LAC gme
links <- links[-grep("L2M-SAS-LAC-4-28-15.pdf", links)]

# Hack correction for url link which includes "http:// https://"
links <- str_remove(links, "http://: ")

# Hack correction for urls that have %0d after gameId= 
links <- str_remove(links, "%0d")

# ---- pdf-format ---------------------------------------------------------


links_pdf <- links[grepl(pattern = "*.pdf", links)]

files  <- paste(data_source, basename(links_pdf), sep = "/")

# For each url and file, check to see if it exists then try to download.
#  need to keep a record of those which fail with a 404 error
pdf_games <- map(links_pdf,  function(x) {
  file_x <- paste(data_source, basename(x), sep = "/")
  if (!file.exists(file_x)) {
    Sys.sleep(runif(1, 3, 5))
    results = tryCatch(download.file(x, file_x, method = "libcurl"),
                       warning = function(w) {
                         "bad"
                       })
    return(data.frame(file = file_x, url = x, result = results))
  } else return(data.frame(file = file_x, url = x, result = "exists"))
})
