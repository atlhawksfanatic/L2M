# 0-stats-nba-game-ids-pre-2015.R

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

# Pre 2015-16 gameids, note no scores are present.
if (file.exists(paste0(id_source, "/game_ids_pre2015.csv"))) {
  pre2015_ids <- read_csv(paste0(id_source, "/game_ids_pre2015.csv"))
} else {
  # Make the franchise names consistent for pre-2015, bad on left good on right
  team_cross <- c(#"ATL" = "",
    # "BKN" = "",
    # "BOS" = "",
    # "CHA" = "",
    "CHH" = "NOP",
    # "CHI" = "",
    # "CLE" = "",
    # "DAL" = "",
    # "DEN" = "",
    # "DET" = "",
    # "GSW" = "",
    # "HOU" = "",
    # "IND" = "",
    # "LAC" = "",
    # "LAL" = "",
    # "MEM" = "",
    # "MIA" = "",
    # "MIL" = "",
    # "MIN" = "",
    "NJN" = "BKN",
    "NOH" = "NOP",
    "NOK" = "NOP",
    # "NOP" = "",
    # "NYK" = "",
    # "OKC" = "",
    # "ORL" = "",
    # "PHI" = "",
    # "PHX" = "",
    # "POR" = "",
    # "SAC" = "",
    # "SAS" = "",
    "SEA" = "OKC",
    # "TOR" = "",
    # "UTA" = "",
    # "WAS" = "",
    "VAN" = "MEM")
  
  pre2015_ids <- paste0("https://raw.githubusercontent.com/gmf05/",
                        "nba/master/data/csv/games_96-14.csv") %>% 
    read_csv(col_types = cols(.default = "c")) %>% 
    janitor::clean_names()
  
  pre2015_ids <- pre2015_ids %>% 
    mutate(date = as.Date(str_sub(game_code, 1, 8), format = "%Y%m%d"),
           home = ifelse(is.na(team_cross[home]),
                         home,
                         team_cross[home]),
           away = ifelse(is.na(team_cross[away]),
                         away,
                         team_cross[away])) %>% 
    select(gid = game_id, gcode = game_code, date, home, away)
  
  write_csv(pre2015_ids, paste0(id_source, "/game_ids_pre2015.csv"))
}

# ---- read ---------------------------------------------------------------

# Scores
if (file.exists(paste0(id_source, "/game_ids_pre2015_scores.csv"))) {
  pre2015_scores <- read_csv(paste0(id_source, "/game_ids_pre2015_scores.csv"),
                             col_types = cols(.default = "c"))
  
} else {
  pre2015_scores <- data_frame(gid = NA_character_)
  }


missing_ids <- pre2015_ids %>% 
  mutate(home_score = if("home_score" %in% colnames(.)) home_score else NA_integer_,
         away_score = if("away_score" %in% colnames(.)) away_score else NA_integer_) %>% 
  # Let's only get post 2014 games
  filter(!(gid %in% pre2015_scores$gid))


# ---- query --------------------------------------------------------------

# x = "0022100747"
missing_games <- top_n(missing_ids, 75, date) %>% 
  pull(gid)

info_mapped <- purrr::map(missing_games, function(x) {
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
    json <- tryCatch({
      res$content %>%
        rawToChar() %>%
        jsonlite::fromJSON(simplifyVector = T, flatten = T)
    }, error = function(e) {
      message(paste("Looks like an error for game: ", x))
      message("Here's the original error message:")
      message(e)
      # Choose a return value in case of error
      return(NA)
    })
    
    if (length(json)<=1) return(data.frame(gid = x))
    
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
    
    score <- json_map %>% 
      filter(table == "LineScore") %>% 
      select_if(~ !any(is.na(.))) %>% 
      mutate(date = as.Date(str_sub(GAME_DATE_EST, 1, 10)),
             team_abr = TEAM_ABBREVIATION,
             side = if_else(team_abr == id_info$home, "home", "away"),
             score = PTS) %>% 
      select(gid = GAME_ID, date, side, team_abr, score) %>% 
      pivot_wider( c("gid", "date"),
                   names_from = "side",
                   names_glue = "{side}_{.value}",
                   values_from = c(team_abr, score)) %>% 
      select(gid, date, home = home_team_abr, home_score,
             away = away_team_abr, away_score)
    
    game_info <- left_join(id_info, score)
    
    return(game_info)
  }
})

info_scores <- info_mapped %>% 
  bind_rows()

# Then add in info_scores
info_scores %>% 
  mutate_all(as.character) %>% 
  bind_rows(pre2015_scores) %>% 
  arrange(gid) %>% 
  distinct() %>% 
  filter(!is.na(gid), !is.na(gcode)) %>% 
  write_csv(paste0(id_source, "/game_ids_pre2015_scores.csv"))

# 
# if (file.exists(paste0(id_source, "/game_ids_pre2015_scores.csv"))) {
#   pre2015_scores <- read_csv(paste0(id_source, "/game_ids_pre2015_scores.csv"),
#                              col_types = cols(.default = "c"))
#   
#   
# } else {
#   # Save first batch
#   write_csv(info_scores,
#             paste0(id_source, "/game_ids_pre2015_scores.csv"))
# }
