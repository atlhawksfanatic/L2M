# 0-L2M-download-archive:
#  Download all the archived L2M reports in raw form to then evaluate:
#  http://official.nba.com/nba-last-two-minute-reports-archive/

# ---- start --------------------------------------------------------------


library("httr")
library("rvest")
library("tidyverse")

# Create a directory for the data
local_dir   <- "0-data/L2M/archive"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

url <- "http://official.nba.com/nba-last-two-minute-reports-archive/"

# read in url from above, then extract the links that comply with:
links <- read_html(url) %>% 
  html_nodes("#main a+ a , strong+ a") %>%
  html_attr("href") %>% 
  # Missing one game of Spurs-Clippers, Memphis-Houston, and Indiana-Chicago
  append(paste0("https://ak-static.cms.nba.com/wp-content/uploads/sites/",
                "4/2015/04/L2M-SAS-LAC-4-28-2015.pdf")) %>% 
  append(paste0("http://official.nba.com/wp-content/uploads/",
                "sites/4/2015/03/L2M-MEM-HOU-3-4-15.pdf"),
         paste0("https://ak-static.cms.nba.com/wp-content/uploads/",
                "sites/4/2015/11/L2M-IND-CHI-11-16-151.pdf"),
         paste0("https://ak-static.cms.nba.com/wp-content/uploads/",
                "sites/4/2016/02/L2M-IND-@-MIA-2-22-16-1.pdf"),
         paste0("https://ak-static.cms.nba.com/wp-content/uploads/",
                "sites/4/2016/05/L2M-GSW-OKC-5-28-16.pdf"))

# Remove nonexistent SAS-LAC gme
links <- links[-grep("L2M-SAS-LAC-4-28-15.pdf", links)]

files  <- paste(data_source, basename(links), sep = "/")

# For each url and file, check to see if it exists then try to download.
#  need to keep a record of those which fail with a 404 error
pdf_games <- map(links,  function(x) {
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


# ---- blogs --------------------------------------------------------------

#  Cross checking the blog posts for missing games with the structure of:
#  http://official.nba.com/
#  nba-officiating-last-two-minute-report-february-27-2017/

# base <- "http://official.nba.com/nba-officiating-last-two-minute-report"
# 
# dates <- c(seq(as.Date("2015/03/01"), as.Date("2015/06/16"), by = "day"),
#            seq(as.Date("2015/10/27"), as.Date("2016/06/19"), by = "day"),
#            seq(as.Date("2016/10/25"), as.Date("2017/06/07"), by = "day"))
# 
# blog_urls <- paste(base, tolower(format(dates, "%B")),
#                    as.numeric(format(dates, "%d")),
#                    format(dates, "%Y"), sep = "-")
# 
# game_links <- map(blog_urls, function(x){
#   Sys.sleep(runif(1, 2, 4)) # should take no more than 34 minutes
#   temp <- GET(x)
#   if (temp$status_code > 400) return()
#   temp <- temp %>% 
#     read_html() %>% 
#     html_nodes("#main p+ p a") %>%
#     html_attr("href")
#   
#   temp <- temp[!(temp == "http://official.nba.com/wp-content/uploads/sites/4/2015/11/L2M-IND-CHI-11-16-151.pdf")]
#   files <- paste(data_source, basename(temp), sep = "/")
#   
#   map(temp, function(i){
#     file_i <- paste(data_source, basename(i), sep = "/")
#     if (!file.exists(file_i)) download.file(i, file_i, method = "libcurl")
#   })
# })
