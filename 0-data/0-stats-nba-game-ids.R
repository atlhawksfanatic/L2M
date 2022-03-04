# 0-stats-nba-game-ids.R

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/stats_nba"
data_source <- paste0(local_dir, "/raw")
id_source   <- paste0(data_source, "/game_ids")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
if (!file.exists(id_source)) dir.create(id_source, recursive = T)

# ---- nba-stats-api ------------------------------------------------------

# Set up headers to make requests to API
stats_nba_headers <- c(
  "Host" = "stats.nba.com",
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

# ---- pre-2015-game-ids ---------------------------------------------------

# https://github.com/gmf05/nba
# The NBA's Game ID, 0021400001, is a 10-digit code: XXXYYGGGGG, where:
#  XXX refers to a season prefix,
#  YY is the season year (e.g. 14 for 2014-15),
#  and GGGGG refers to the game number (1-1230 for 30-team regular season).
# 
# Season prefixes are:# 
# 001 : Pre Season
# 002 : Regular Season
# 003 : All-Star
# 004 : Post Season
# 005 : Play-In
# https://raw.githubusercontent.com/gmf05/nba/master/data/csv/games_96-14.csv

# Pre 2015-16 gameids
if (file.exists(paste0(id_source, "/game_ids_pre2015.csv"))) {
  pre2015_ids <- read_csv(paste0(id_source, "/game_ids_pre2015.csv"))
} else {
  pre2015_ids <- paste0("https://raw.githubusercontent.com/gmf05/",
                      "nba/master/data/csv/games_96-14.csv") %>% 
    read_csv(col_types = cols(.default = "c")) %>% 
    janitor::clean_names()
  
  pre2015_ids <- pre2015_ids %>% 
    mutate(date = as.Date(str_sub(game_code, 1, 8), format = "%Y%m%d")) %>% 
    select(gid = game_id, gcode = game_code, date, home, away)
  
  write_csv(pre2015_ids, paste0(id_source, "/game_ids_pre2015.csv"))
}

# These are missing the 2015 Playoffs, need to add those in by guessing the
#  range of values that the game id can take on
# start: 0041400101
# end:   0041400406

yoffs_id_range <- str_pad(seq(0041400101, 0041400406), 10, "left", "0")

# ---- queries ------------------------------------------------------------

# The 2015 Playoffs Query
if (file.exists(paste0(id_source, "/game_ids_2015_playoffs.csv"))) {
  yoffs <- read_csv(paste0(id_source, "/game_ids_2015_playoffs.csv"))
} else {
  yoffs_mapped <- purrr::map(yoffs_id_range, function(x) {
    Sys.sleep(runif(1, 5, 15))
    print(paste(x, "at", Sys.time()))
    
    # Info on officials and attendance for a game
    x_url <- paste0("https://stats.nba.com/stats/boxscoresummaryv2?GameID=", x)
    
    
    res <- GET(x_url, add_headers(stats_nba_headers))
    
    # Exit out if the query is invalid
    if (res$status_code > 400) {
      id_info <- data.frame(gid = x)
      print(paste("Game ", x, " does not exist"))
      return(id_info)
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
      }) %>% 
        bind_rows()
      
      # Is this just a summary of the series? Then exit.
      if (json_map$WH_STATUS[1] == "0") {
        id_info <- data.frame(gid = x)
        print(paste("Series with game ", x, " does not exist"))
      } else {
        team_ids <- json_map %>% 
          select(TEAM_ID, TEAM_ABBREVIATION) %>% 
          filter(!is.na(TEAM_ID)) %>% 
          distinct() %>% 
          column_to_rownames("TEAM_ID")
        
        id_info <- json_map %>% 
          filter(table == "GameSummary") %>% 
          select_if(~ !any(is.na(.))) %>% 
          mutate(date = as.Date(str_sub(GAME_DATE_EST, 1, 10)),
                 home = team_ids[HOME_TEAM_ID,],
                 away = team_ids[VISITOR_TEAM_ID,]) %>% 
          select(gid = GAME_ID, gcode = GAMECODE,
                 date, home, away)
      }
      
      return(id_info)
    }
  })
  
  
  yoffs <- bind_rows(yoffs_mapped) %>% 
    filter(!is.na(gcode))
  
  write_csv(yoffs, paste0(id_source, "/game_ids_2015_playoffs.csv"))
}


# Query for all of the existing schedules for NBA game ids, not sure what to do
#  when a season starts to add on playoff games
years <- seq(2015, format(Sys.Date(), "%Y"))

ids_map <- map(years, function(x) {
  Sys.sleep(runif(1, 5, 10))
  print(paste(x, "at", Sys.time()))
  # https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/2021/league/00_full_schedule.json
  x_url <- paste0("https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/",
                  x,
                  "/league/00_full_schedule.json")
  
  res <- GET(x_url)
  
  # Exit out if the query is invalid
  if (res$status_code > 400) {
    id_info <- data.frame(gid = NA)
    print(paste("Season ", x, " does not exist"))
  } else {
    id_info <-
      res$content %>%
      rawToChar() %>%
      jsonlite::fromJSON(flatten = T) %>% 
      .$lscd %>% 
      .$mscd.g %>% 
      bind_rows() %>% 
      mutate(date = as.Date(gdte)) %>% 
      # Unravel the broadcast variables for each game
      unnest(bd.b, keep_empty = T, names_sep = "_") %>%
      group_by(gid, gcode, date, home = h.ta, away = v.ta) %>%
      # Extract only the times a game was on national TV, which could be null
      summarise(national_tv = list(bd.b_disp[bd.b_scope == "natl" &
                                               bd.b_type == "tv"])) %>%
      # home_score = h.s[1],
      # away_score = v.s[1]
      # Replace the possible nulls with NA for national TV and unravel
      mutate(national_tv = replace_na(national_tv)) %>%
      unnest(national_tv) %>%
      # select(gid, gcode, date, home = h.ta, away = v.ta) %>%
      arrange(date, gid)
    
    # Save each season game schedule as csv
    write_csv(id_info, paste0(id_source, "/game_ids_", x, ".csv"))
  }
  
  return(id_info)
})

# Bring them all together
game_list <- bind_rows(ids_map) %>% 
  replace_na(list(national_tv = "no")) %>% 
  bind_rows(pre2015_ids) %>% 
  bind_rows(yoffs) %>% 
  filter(!is.na(gid)) %>% 
  arrange(date, gid)

# Save as csv
write_csv(game_list, paste0(local_dir, "/nba_game_schedule.csv"))
