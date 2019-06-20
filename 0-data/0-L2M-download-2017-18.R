# 0-NBA-L2M-download-2017-18:
#  Download all the archived L2M reports in raw form to then evaluate:
#  https://official.nba.com/2017-18-nba-officiating-last-two-minute-reports/

library("rvest")
library("tidyverse")

# Create a directory for the data
local_dir     <- "0-data/L2M/2017-18"
data_source   <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

url <- "https://official.nba.com/2017-18-nba-officiating-last-two-minute-reports/"

# read in url from above, then extract the links that comply with:
links <- read_html(url) %>% 
  html_nodes("#main a+ a , strong+ a") %>%
  html_attr("href")

links <- links[!grepl(pattern = "http://official.nba.com/nba-last-two-minute-reports-archive/", links)]

# These are the "missing" files that I don't have:
# "L2M-CHI-MIL-12-15-2017.pdf" - not in files
# "L2M-DEN-OKC-03-30-2018.pdf" - not in files
# "L2M-GSW-ATL-03-02-2018.pdf" - not in files
# "L2M-IND-BKN-02-14-2018.pdf" - it is "L2M-IND-BKN-02-14-2018-1.pdf" 
# "L2M-NYK-NOP-12-30-2017.pdf" - not in files
# "L2M-PHI-WAS-10-18-2017.pdf" - it's there!
# "L2M-SAS-POR-12-20-2017.pdf" - not in files
# need to add in some missing ones:
missed <- paste0("https://ak-static.cms.nba.com/wp-content/uploads/sites/4/",
                 c("2017/12/L2M-CHI-MIL-12-15-2017.pdf",
                   "2018/03/L2M-DEN-OKC-03-30-2018.pdf",
                   "2018/03/L2M-GSW-ATL-03-02-2018.pdf",
                   #"2018/02/L2M-IND-BKN-02-14-2018.pdf",
                   "2017/12/L2M-NYK-NOP-12-30-2017.pdf",
                   "2017/12/L2M-SAS-POR-12-20-2017.pdf"))
links  <- append(links, missed)


files  <- paste(data_source, basename(links), sep = "/")

# For each url and file, check to see if it exists then try to download.
#  need to keep a record of those which fail with a 404 error

pdf_games <- map(links,  function(x) {
  file_x <- paste(data_source, basename(x), sep = "/")
  if (!file.exists(file_x)) {
    Sys.sleep(runif(1, 3, 5))
    tryCatch(download.file(x, file_x, method = "libcurl"),
             warning = function(w) {
               "bad"
             })
  } else "exists"
})

