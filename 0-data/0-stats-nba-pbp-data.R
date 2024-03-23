# 0-stats-nba-box-data.R

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/stats_nba"
data_source <- paste0(local_dir, "/raw")
pbp_source  <- paste0(data_source, "/pbp")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(pbp_source)) dir.create(pbp_source, recursive = T)

# ---- game-calls ---------------------------------------------------------

l2m_games <- read_csv("1-tidy/L2M/L2M_raw_api.csv")

# ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
if (file.exists(paste0(local_dir, "/nba_game_schedule.csv"))) {
  id_list <- read_csv(paste0(local_dir, "/nba_game_schedule.csv")) |> 
    select(-national_tv)
} else {
  print("Please download game schedule from 0-stats-nba-game-ids.R")
  id_list <- data.frame(gid = NA_character_) 
}


# Games in the L2M
stats_nba_games <- l2m_games |> 
  select(date, home, away) |> 
  distinct()

stats_nba_game_ids <- stats_nba_games |> 
  left_join(id_list) |> 
  arrange(date)

# Read the list of already downloaded box scores
queried_pbp <- dir(pbp_source, pattern = ".csv", full.names = T)

# Only download the missing pbp scores:
new_game_ids <- stats_nba_game_ids |> 
  filter(!is.na(gid),
         !(gid %in% 
             tools::file_path_sans_ext(basename(queried_pbp))))

# All games instead of just L2Ms
new_game_ids <- id_list |> 
  filter(date < Sys.Date(),
         # date > "2022-10-01",
         !is.na(home_score)) |> 
  filter(!is.na(gid),
         !(gid %in% 
             tools::file_path_sans_ext(basename(queried_pbp))))

# ---- nba-stats-api ------------------------------------------------------

# Set up headers to make requests to API
stats_nba_headers <- c(
  "Host" = "stats.nba.com",
  # "User-Agent" = "libcurl/7.68.0 r-curl/4.3.2 httr/1.4.2",
  "User-Agent" = paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) ",
                        "AppleWebKit/537.36 (KHTML, like Gecko) ",
                        "Chrome/79.0.3945.130 Safari/537.36"),
  "Accept" = "application/json, text/plain, */*",
  "Accept-Language" = "en-US,en;q=0.5",
  "Accept-Encoding" = "gzip, deflate, br",
  "x-nba-stats-origin" = "stats",
  "x-nba-stats-token" = "true",
  "Connection" = "keep-alive",
  "Referer" = "https =//stats.nba.com/",
  "Pragma" = "no-cache",
  "Cache-Control" = "no-cache"
)

# ---- query --------------------------------------------------------------

# Take the list of nba_game_ids that are missing pbp, then query API
games_to_map <- new_game_ids$gid[1:(min(length(new_game_ids$gid), 25))]

pbp_mapped <- purrr::map(games_to_map, function(x) {
  Sys.sleep(runif(1, 2.5, 5.5))
  print(paste(x, "at", Sys.time()))
  
  # PBP v2 info:
  # Endpoint URL
  # https://stats.nba.com/stats/playbyplayv2
  # 
  # Valid URL
  # https://stats.nba.com/stats/playbyplayv2?
  #  EndPeriod=1&GameID=0021700807&StartPeriod=1
  x_url <- paste0("https://stats.nba.com/stats/playbyplayv2?",
                  "EndPeriod=4&GameID=",
                  x,
                  "&StartPeriod=1")
  
  res <- GET(x_url, add_headers(stats_nba_headers))
  
  # Exit out if the query is invalid
  if (res$status_code == 404) {
    pbp_score_info <- data.frame(table = NA)
    return(pbp_score_info)
  } else {
    json <-
      res$content |>
      rawToChar() |>
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    if (is_empty(json$resultSets$rowSet[[1]])) {
      pbp_score_info <- data.frame(table = NA)
      return(pbp_score_info)
    } else {
      json_map <- pmap(json$resultSets, function(name, headers, rowSet) {
        results <- data.frame(rowSet)
        names(results) <- headers
        results$table <- name
        return(results)
      })
    }
    
    pbp_score_info <- json_map[[1]]
    return(pbp_score_info)
  }
})


# ---- save ---------------------------------------------------------------

# Save individual games based on nba_game_id
ind_games_csv <- purrr::map(pbp_mapped, function(x) {
  game_id <- x$GAME_ID[1]
  
  # If the data.frame in the list only has one observation it's an error
  if (nrow(x) > 1) {
    write_csv(x, paste0(pbp_source, "/", game_id, ".csv"))
    return(data.frame(game_id, status = "good"))
  } else if (is_null(game_id)) {
    return(data.frame(game_id = NA_character_, status = "bad"))
  } else {
    return(data.frame(game_id, status = "bad"))
  }
})

