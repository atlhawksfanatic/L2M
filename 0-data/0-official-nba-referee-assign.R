# 0-official-nba-referee-assign.R
# Referee Assignments

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/official_nba"
data_source <- paste0(local_dir, "/raw")
ref_source  <- paste0(data_source, "/referee")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(ref_source)) dir.create(ref_source, recursive = T)

# ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
if (file.exists("0-data/stats_nba/nba_game_schedule.csv")) {
  id_list <- read_csv("0-data/stats_nba/nba_game_schedule.csv")
} else {
  print("Please download game schedule from 0-stats-nba-box-data.R")
  id_list <- data.frame(gid = NA_character_) 
}

# Read the list of already downloaded referee assignments
queried_ref <- dir(ref_source, pattern = ".csv", full.names = T) %>% 
  as_tibble() %>% 
  mutate(date = as.Date(tools::file_path_sans_ext(basename(value))))

new_game_dates <- id_list %>% 
  # L2Ms began on 2015-03-01, so only use those dates onward in the id_list
  filter(date > "2015-02-28") %>% 
  # And don't download for a date that has already occurred
  filter(!date %in% queried_ref$date)

# ---- nba-official-api ------------------------------------------------------

# https://official.nba.com/wp-json/api/v1/get-game-officials?&date=2022-01-31

# Set up headers to make requests to API
official_nba_headers <- c(
  "Host" = "official.nba.com",
  "User-Agent" = "libcurl/7.68.0 r-curl/4.3.2 httr/1.4.2",
  "Accept" = "application/json, text/javascript, */*; q=0.01",
  "Accept-Language" = "en-US,en;q=0.9",
  "Accept-Encoding" = "gzip, deflate, br",
  # "x-nba-official-origin" = "official",
  # "x-nba-official-token" = "true",
  "Connection" = "keep-alive",
  "Referer" = "https://official.nba.com/referee-assignments/"#,
  # "Pragma" = "no-cache",
  # "Cache-Control" = "no-cache"
)

# ---- query --------------------------------------------------------------

# Take the list of nba_game_dates that are missing ref, subset prior to today,
#  then query API

map_dates <-new_game_dates %>% 
  filter(date < Sys.Date() + 1) %>% 
  select(date) %>% 
  distinct()

ref_mapped <- purrr::map(map_dates$date, function(x) {
  Sys.sleep(runif(1, 2.5, 5.5))
  print(paste(x, "at", Sys.time()))
  
  # ref v1 api
  x_url <- paste0("https://official.nba.com/wp-json/api/v1/",
                  "get-game-officials?&date=",
                  x)
  
  res <- GET(x_url, add_headers(official_nba_headers))
  
  # Exit out if the query is invalid
  if (res$status_code == 404) {
    ref_info <- data.frame(table = NA)
    return(ref_info)
  } else {
    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    ref_info      <- json$nba$Table$rows
    ref_info$date <- x
    return(ref_info)
  }
})


# ---- save ---------------------------------------------------------------

# Save individual games based on nba_game_id
ind_games_csv <- purrr::map(ref_mapped, function(x) {
  temp <- as.data.frame(x)
  game_date <- temp$date[1]
  
  # If the data.frame in the list only has one observation it's an error
  if (nrow(temp) > 1) {
    write_csv(temp, paste0(ref_source, "/", game_date, ".csv"))
    return(data.frame(game_date, status = "good"))
  } else {
    write_csv(temp, paste0(ref_source, "/", game_date, ".csv"))
    return(data.frame(game_date, status = "bad"))
  }
})

ref_assigned <- map(queried_ref$value,
                    read_csv, col_types = cols(.default = "c")) %>% 
  bind_rows() %>% 
  # Get rid of reports that don't exist and only have game_id non-NA
  filter_at(vars(-date),
            any_vars(!is.na(.))) %>% 
  arrange(date, game_id)

# Join the mapped with the previously mapped, save for use
ref_all <- ref_mapped %>% 
  bind_rows() %>% 
  # Convert to characters
  mutate(across(everything(), as.character)) %>% 
  # Then join with already read in
  bind_rows(ref_assigned) %>% 
  # For some reason there are duplicated dates
  select(-date) %>% 
  distinct() %>% 
  filter(!is.na(game_date)) %>% 
  arrange(game_code, game_id)

write_csv(ref_all, paste0(local_dir, "/referee_assignments.csv"))
write_rds(ref_all, paste0(local_dir, "/referee_assignments.rds"))
