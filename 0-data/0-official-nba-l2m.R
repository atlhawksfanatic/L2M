# 0-official-nba-l2m.R
# Official NBA L2M Reports through their API

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/official_nba"
data_source <- paste0(local_dir, "/raw")
l2m_source  <- paste0(data_source, "/l2m-api")
l2m_bad     <- paste0(l2m_source, "/bad")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(l2m_source)) dir.create(l2m_source, recursive = T)
if (!file.exists(l2m_bad)) dir.create(l2m_bad, recursive = T)

# # ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
# Based on: 0-stats-nba-game-ids.R
if (file.exists("0-data/stats_nba/nba_game_schedule.csv")) {
  id_list <- read_csv("0-data/stats_nba/nba_game_schedule.csv")
} else {
  print("Please download game schedule from 0-stats-nba-game-ids.R")
  id_list <- data.frame(gid = NA_character_) 
}

# # Read the list of already queried L2Ms
# queried_l2m <- dir(l2m_source, pattern = ".csv",
#                    recursive = T, full.names = T) |> 
#   as_tibble() |> 
#   mutate(game_id = tools::file_path_sans_ext(basename(value)))

# Read in previous L2M
if (file.exists(paste0(local_dir, "/official_nba_l2m_api.csv"))) {
  l2m_old <- read_csv(paste0(local_dir, "/official_nba_l2m_api.csv"),
                      col_types = cols(.default = "c"))
} else {
  l2m_old <- data.frame(game_id = NA_character_) 
}

new_game_ids <- id_list |> 
  # L2Ms began on 2015-03-01, so only use those dates onward in the id_list
  filter(date > "2015-02-28", date < Sys.Date() - 1) |> 
  # And don't download for a date that has already occurred
  filter(!gid %in% l2m_old$game_id)


# ---- nba-official-api ------------------------------------------------------

# https://official.nba.com/l2m/json/0022100747.json

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
  "Referer" = "https://official.nba.com/"#,
  # "Pragma" = "no-cache",
  # "Cache-Control" = "no-cache"
)


# ---- query --------------------------------------------------------------

# Take the list of nba_game_ids that are missing ref, then query API
l2m_mapped <- purrr::map(new_game_ids$gid[1:min(50, length(new_game_ids$gid))], function(x) {
  Sys.sleep(runif(1, 2.5, 5.5))
  print(paste(x, "at", Sys.time()))
  
  # l2m api
  # x = "0022100747"
  x_url <- paste0("https://official.nba.com/l2m/json/",
                  x,
                  ".json")
  
  res <- GET(x_url, add_headers(official_nba_headers))
  
  # Exit out if the query is invalid
  if (res$status_code > 399) {
    l2m <- data.frame(game_id = x)
    return(l2m)
  } else {
    json <-
      res$content |>
      rawToChar() |>
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    l2m       <- json$l2m
    game_info <- json$game
    results   <- bind_cols(l2m, game_info) |> 
      mutate(game_id = x)
    return(results)
  }
  
})


# ---- save ---------------------------------------------------------------

# Save individual games based on nba_game_id
ind_games_csv <- purrr::map(l2m_mapped, function(x) {
  temp <- as.data.frame(x)
  game_id <- temp$game_id[1]
  
  # If the data.frame in the list only has one observation it's an error
  if (nrow(temp) > 1) {
    write_csv(temp, paste0(l2m_source, "/", game_id, ".csv"))
    return(data.frame(game_id, status = "good"))
  } else {
    write_csv(temp, paste0(l2m_bad, "/", game_id, ".csv"))
    return(data.frame(game_id, status = "bad"))
  }
})

# Combine with previously queried in L2Ms
l2m_all <- l2m_mapped |> 
  bind_rows() |> 
  mutate(across(everything(), as.character)) |> 
  bind_rows(l2m_old) |> 
  arrange(GameId, PeriodName, desc(PCTime))

# Minor corrections
l2m_games_corrected <- l2m_all |> 
  # https://official.nba.com/l2m/L2MReport.html?gameId=0021800788 shows a play
  # occurs at "00:96" for a Westbrook play but video shows 00:56.9
  mutate(PCTime = ifelse(PCTime == "00:96" & game_id == "0021800788",
                         "00:56.9", PCTime)) |> 
  # https://official.nba.com/l2m/L2MReport.html?gameId=0021801148 has two plays
  #  at the bottom with PCTime of "00:18." which are missing the milliseconds
  mutate(PCTime = ifelse(PCTime == "00:18." & game_id == "0021801148",
                         "00:18.1", PCTime)) |> 
  # https://official.nba.com/l2m/L2MReport.html?gameId=0022000029 has a play
  #  missing the millisecond in overtime at "01:16."
  mutate(PCTime = ifelse(PCTime == "01:16." & game_id == "0022000029",
                         "01:16.5", PCTime))

write_csv(l2m_games_corrected, paste0(local_dir, "/official_nba_l2m_api.csv"))
write_rds(l2m_games_corrected, paste0(local_dir, "/official_nba_l2m_api.rds"))