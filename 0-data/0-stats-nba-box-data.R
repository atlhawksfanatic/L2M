# 0-stats-nba-box-data.R

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/stats_nba"
data_source <- paste0(local_dir, "/raw")
box_source  <- paste0(data_source, "/box_scores")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(box_source)) dir.create(box_source, recursive = T)

# ---- game-calls ---------------------------------------------------------

l2m_games <- read_csv("1-tidy/L2M/L2M_raw_api.csv")

# ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
if (file.exists(paste0(local_dir, "/nba_game_schedule.csv"))) {
  id_list <- read_csv(paste0(local_dir, "/nba_game_schedule.csv")) %>% 
    select(-networks)
} else {
  print("Please download game schedule from 0-stats-nba-game-ids.R")
  id_list <- data.frame(gid = NA_character_) 
}

# Games in the L2M
stats_nba_games <- l2m_games %>% 
  select(date, home, away) %>% 
  distinct()

stats_nba_game_ids <- stats_nba_games %>% 
  left_join(id_list) %>% 
  arrange(date)

if (file.exists("0-data/stats_nba/stats_nba_box.csv")) {
  old_box <- read_csv("0-data/stats_nba/stats_nba_box.csv",
                      col_types = cols(.default = "c")) %>% 
    mutate(MIN = as.numeric(MIN),
           date = as.Date(date)) %>% 
    select(-any_of(c("home_score", "away_score", "networks", "national_tv"))) %>% 
    left_join(stats_nba_game_ids)
} else {
  old_box <- data.frame(gid = NA)
  # In case you need to reread in the box scores
  # Read the list of already downloaded box scores
  # queried_box <- dir(box_source, pattern = ".csv", full.names = T)
  # old_map <- map(queried_box, ~read_csv(., col_types = cols(.default = "c")))
  # old_box <- old_map %>% 
  #   bind_rows() %>% 
  #   mutate(MIN = as.numeric(MIN),
  #          date = as.Date(date))
}

# Only download the missing box scores:
missing_ids <- stats_nba_game_ids %>% 
  filter(!is.na(gid), !(gid %in% unique(old_box$gid)))

# ---- nba-stats-api ------------------------------------------------------

# Set up headers to make requests to API
stats_nba_headers <- c(
  "Host" = "stats.nba.com",
  # "User-Agent" = "libcurl/7.68.0 r-curl/4.3.2 httr/1.4.2",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36",
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

# Take the list of nba_game_ids that are missing box scores, then query API
#  for information on players, referees, and attendance

box_mapped <- purrr::map(missing_ids$gid, function(x) {
  Sys.sleep(runif(1, 0.5, 2.5))
  print(paste(x, "at", Sys.time()))
  
  # Box Score Info:
  # EndPeriod=1&EndRange=0&GameID=0021700807&
  # RangeType=0&StartPeriod=1&StartRange=0
  x_url <- paste0("https://stats.nba.com/stats/boxscoretraditionalv2?",
                  "EndPeriod=1&EndRange=0&GameID=",
                  x,
                  "&RangeType=0&StartPeriod=1&StartRange=0")
  
  
  res <- GET(x_url, add_headers(stats_nba_headers))
  
  # Exit out if the query is invalid
  if (res$status_code == 404) {
    box_score_info <- data.frame(table = NA)
    return(box_score_info)
  } else {
    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    json_map <- pmap(json$resultSets, function(name, headers, rowSet) {
      results <- data.frame(rowSet)
      names(results) <- headers
      results$table <- name
      return(results)
    })
    
    box_score_info <- json_map %>% 
      bind_rows() %>% 
      filter(table == "PlayerStats") %>% 
      select(any_of(c("GAME_ID", "TEAM_ID", "TEAM_ABBREVIATION",
                      "PLAYER_ID", "PLAYER_NAME", "NICKNAME", "MIN")))
    
    return(box_score_info)
  }
})

info_mapped <- purrr::map(missing_ids$gid, function(x) {
  Sys.sleep(runif(1, 0.5, 2.5))
  print(paste(x, "at", Sys.time()))
  
  # Info on officials and attendance for a game
  x_url <- paste0("https://stats.nba.com/stats/boxscoresummaryv2?GameID=", x)
  
  
  res <- GET(x_url, add_headers(stats_nba_headers))
  
  # Exit out if the query is invalid
  if (res$status_code == 404) {
    game_output <- data.frame(table = NA)
    return(game_output)
  } else {
    json <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    json_map <- pmap(json$resultSets, function(name, headers, rowSet) {
      results <- data.frame(rowSet)
      # Sometimes this is empty, so ignore it if it is empty
      if (!length(results)) {
        results <- data.frame(table = name)
        return(results)
      }
      
      names(results) <- headers
      results$table <- name
      return(results)
    })
    
    game_info <- json_map %>% 
      bind_rows() %>% 
      filter(table == "GameInfo") %>% 
      select_if(~ !any(is.na(.))) %>% 
      select(-table)
    
    officials <- json_map %>% 
      bind_rows() %>% 
      filter(table == "Officials") %>% 
      select_if(~ !any(is.na(.))) %>% 
      select(-table)
    
    officials_wide <- officials %>% 
      arrange(OFFICIAL_ID) %>% 
      mutate(OFFICIAL = paste(FIRST_NAME, LAST_NAME),
             num = 1:n()) %>% 
      select(-FIRST_NAME, -LAST_NAME) %>% 
      pivot_wider(names_from = num,
                  values_from = c(OFFICIAL, OFFICIAL_ID, JERSEY_NUM),
                  names_glue = "{.value}_{num}")
    
    game_output <- bind_cols(game_info, officials_wide) %>% 
      mutate(GAME_ID = json$parameters$GameID)
    
    return(game_output)
  }
})



# ---- manipulate ---------------------------------------------------------


# Take new box score info and combine along with ID information to determine
# home/away players
new_box <- box_mapped %>% 
  bind_rows()

new_info <- info_mapped %>% 
  bind_rows()


# If there are no box scores downloaded, make the new box_info NA
if (is_empty(new_box) & is_empty(new_info)) {
  new_box_info <- data.frame(gid = NA)
} else {
  # get player's side and convert the minutes into a numeric
  new_box_info <- new_box %>% 
    left_join(new_info) %>% 
    mutate(gid = GAME_ID) %>% 
    left_join(stats_nba_game_ids) %>% 
    mutate(PLAYER_SIDE = ifelse(home == TEAM_ABBREVIATION, "home", "away"),
           MIN = as.numeric(str_extract(MIN, ".+?(?=:)")) +
             as.numeric(str_remove(MIN, ".*:")) / 60)
}

# Save individual games based on gid
ind_games_csv <- purrr::map(unique(new_box_info$gid), function(x) {
  temp <- filter(new_box_info, gid == x)
  
  # If the data.frame in the list only has one observation it's an error
  if (nrow(temp) > 1) {
    write_csv(temp, paste0(box_source, "/", x, ".csv"))
    return(data.frame(x, status = "good"))
  } else {
    return(data.frame(x, status = "bad"))
  }
})

# Add in the already scrapped box score info, then arrange by ID
fresh_box_info <- bind_rows(old_box, new_box_info) %>% 
  filter(!is.na(gid)) %>% 
  arrange(date, gid)


# Save output for continual updates
write_csv(fresh_box_info, paste0(local_dir, "/stats_nba_box.csv"))
write_rds(fresh_box_info, paste0(local_dir, "/stats_nba_box.rds"))
