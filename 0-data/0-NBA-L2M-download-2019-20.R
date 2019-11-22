# 0-L2M-download-2019-20:
#  Download all the archived L2M reports in raw form to then evaluate:
#  https://official.nba.com/2019-20-nba-officiating-last-two-minute-reports/ 

# ---- start --------------------------------------------------------------

library("httr")
library("rvest")
library("tidyverse")
library("splashr")

# Create a directory for the data
local_dir     <- "0-data/L2M/2019-20"
data_source   <- paste0(local_dir, "/raw")
scrape_source <- paste0(data_source, "/scraped_splashr")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(scrape_source)) dir.create(scrape_source, recursive = T)

url <- paste0("https://official.nba.com/",
              "2019-20-nba-officiating-last-two-minute-reports/")

# read in url from above, then extract the links that comply with:
links <- read_html(url) %>% 
  html_nodes("h2~ p a") %>%
  html_attr("href")

# ---- url-format ---------------------------------------------------------

# Adjust the links so it defaults to no video
links_url <- paste0(links, ifelse(grepl(pattern = "&noVideo=true$", links),
                                  "", "&noVideo=true"))

# Only get the links from games not scraped
scraped_files <- dir(scrape_source, pattern = ".csv", full.names = T)

links_url <- links_url[!(gsub(".*\\?|&.*", "", links_url) %in%
                           tools::file_path_sans_ext(basename(scraped_files)))]

# scraped_data <- read_rds("0-data/L2M/scraped_201819.rds")
# bad_data     <- filter(scraped_data, grepl("error", period))
# good_data    <- filter(scraped_data, !grepl("error", period))
# 
# links_url    <- links_url[!(gsub(".*\\?|&.*", "", links_url) %in%
#                               good_data$game_id)]

# ---- splashr-start -------------------------------------------------------

# system("sudo docker run -it -p 8050:8050 scrapinghub/splash")
system("docker run -p 8050:8050 scrapinghub/splash", wait = FALSE)

splash_active() # This needs to be TRUE to work...

# ---- map-links ----------------------------------------------------------

scrape_site <- map(links_url, function(x) {
  game_id <- gsub(".*\\?|&.*", "", x)
  print(paste0(game_id, " at ", Sys.time()))
  
  Sys.sleep(runif(1, 10, 15))
  
  l2m_raw <- render_html(url = x, wait = 7)
  
  game_details <- l2m_raw %>% 
    xml_nodes(".gamedetails") %>% 
    html_text()
  game_date <- l2m_raw %>% 
    xml_nodes(".gamedate") %>% 
    html_text()
  
  l2m_site <- l2m_raw  %>% 
    html_table(fill = T) %>% 
    .[[1]]
  
  # Did the scrape give us a functional table?
  
  if (is.data.frame(l2m_site)) {
    print("is a data.frame")
    l2m_site <- l2m_site[, !is.na(names(l2m_site)) & !(names(l2m_site) == "")]
    
    names(l2m_site) <- tolower(str_replace(names(l2m_site), " ", "_"))
    
    data1 <- l2m_site %>% 
      mutate(comments = if_else(grepl(pattern = "^Comment", period),
                                time, NA_character_),
             comments = lead(comments),
             stint = if_else(period == "", time, NA_character_)) %>% 
      fill(stint)
    
    data2 <- data1 %>% 
      filter(grepl(pattern = "^Period", period)) %>% 
      mutate(period = str_remove(period, "Period:"),
             time = str_remove(time, "Time:"),
             call_type = str_remove(call_type, "Call Type:"),
             committing = str_remove(committing_player,
                                     "Committing Player:"),
             disadvantaged = str_remove(disadvantaged_player,
                                        "Disadvantaged Player:"),
             decision = str_remove(review_decision,
                                   "Review Decision:")) %>% 
      mutate_all(str_trim) %>% 
      select(period, time, call_type, committing, disadvantaged, decision,
             comments, stint)
    
    
    # Check to see that these are dataframes with more than 1 row
    n1 = nrow(data2)
    if (n1 > 0) {
      print("it worked!")
      j5 <- data2
      j5$game_id      <- game_id
      j5$game_details <- game_details
      j5$game_date    <- game_date
      j5$scrape_time  <- Sys.time()
      
      return(j5)
    } else if (n1 == 0) {
      print("didn't work, data were 0 length")
      j5 <- data.frame(Period = "error - n1 == 0",
                       game_id = game_id,
                       game_details = game_details,
                       game_date = game_date,
                       scrape_time = Sys.time())
      
      return(j5)
    } else {
      print("didn't work, not sure")
      j5 <- data.frame(Period = "error - huh",
                       game_id = game_id,
                       game_details = game_details,
                       game_date = game_date,
                       scrape_time = Sys.time())
      
      return(j5)
    }
    
    
  } else {
    print("no")
    j5 <- data.frame(Period = "error",
                     game_id = game_id,
                     game_details = game_details,
                     game_date = game_date,
                     scrape_time = Sys.time())
    
    return(j5)
  }
  
})

# Now, write the full data set and also write individual csv files!
if (is_empty(scrape_site)) {
  scrape_data <- data.frame(scrape_time = NA)
  
} else {
  scrape_data  <- bind_rows(scrape_site) %>% 
    mutate(scrape_time = as.character(scrape_time))
  
}

scraped_data <- map(scraped_files, read_csv, col_types = cols(.default = "c"))

# Individual games
ind_games_csv <- map(scrape_site, function(x) {
  game_id <- x$game_id[1]
  if (nrow(x) > 1) {
    write_csv(x, paste0(scrape_source, "/", game_id, ".csv"))
    return(data.frame(game_id, status = "good"))
  } else {
    return(data.frame(game_id, status = "bad"))
  }
})

corrections <- scrape_data %>% 
  bind_rows(scraped_data) %>%
  arrange(scrape_time) %>% 
  filter(!is.na(scrape_time))

# Enter in the home and away teams plus final scores
corrections <- corrections %>% 
  mutate(away1 = str_trim(str_remove(game_details, "@.*$")),
         away_score = str_extract(away1,  "(?<=\\().+?(?=\\))"),
         away_team = str_trim(str_remove(away1, "\\(.*\\)")),
         home1 = str_trim(str_remove(game_details, ".*@")),
         home_score = str_extract(home1,  "(?<=\\().+?(?=\\))"),
         home_team = str_trim(str_remove(home1, "\\(.*\\)"))) %>% 
  select(-away1, -home1)


write_csv(corrections, paste0(local_dir, "/scraped_201920.csv"))
write_rds(corrections, paste0(local_dir, "/scraped_201920.rds"))


