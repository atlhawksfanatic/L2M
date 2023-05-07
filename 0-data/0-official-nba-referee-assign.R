# 0-official-nba-referee-assign.R
# Referee Assignments

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

dir_create <- function(x) {
  if (!file.exists(x)) dir.create(x, recursive = T)
  return(x)
}

local_dir   <- dir_create("0-data/official_nba")
data_source <- dir_create(paste0(local_dir, "/raw"))
ref_source  <- dir_create(paste0(data_source, "/referee"))
map(paste(ref_source, c("nba", "gl", "wnba"), sep = "/"), dir_create)


# ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
if (file.exists("0-data/stats_nba/nba_game_schedule.csv")) {
  id_list <- read_csv("0-data/stats_nba/nba_game_schedule.csv")
} else {
  print("Please download game schedule from 0-stats-nba-game-ids.R")
  id_list <- data.frame(gid = NA_character_) 
}

# Read the list of already downloaded referee assignments
queried_ref <- dir(ref_source, pattern = ".csv", full.names = T,
                   recursive = T) |> 
  as_tibble() |> 
  mutate(date = as.Date(tools::file_path_sans_ext(basename(value))))

new_game_dates <- id_list |> 
  # L2Ms began on 2015-03-01, so only use those dates onward in the id_list
  filter(date > "2015-02-28") |> 
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

# Take the list of nba_game_dates that are missing ref assignments, filter to
#  anytime before 10AM Eastern of current day when assignments are typically
#  released, then query API

map_dates <- new_game_dates |> 
  # Check if it is after 10AM in New York or not
  filter(date < floor_date(now(tz = "America/New_York") - 60*60*10) + 1) |> 
  # filter(date < ifelse(format(Sys.time(), tz = "America/New_York") >
  #                        as.POSIXct(paste(Sys.Date(), "10:00"),
  #                                   tz = "America/New_York"),
  #                      Sys.Date() + 1,
  #                      Sys.Date())) |> 
  select(date) |> 
  distinct()

ref_mapped <- purrr::map(map_dates$date[1:min(100, length(map_dates$date))],
                         function(x) {
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
      res$content |>
      rawToChar() |>
      jsonlite::fromJSON(simplifyVector = T, flatten = T)
    
    if (is_empty(json$nba$Table$rows)) {
      leagues <- list(nba = data.frame(date = x),
                      gl = data.frame(date = x),
                      wnba = data.frame(date = x))
    } else {
      leagues <- purrr::map(json, function(y) {
        league_games      <- y$Table$rows
        if (is_empty(y$Table1$rows)) {
          league_replay <- data.frame(date = x)
        } else {
          league_replay <- y$Table1$rows |>
            distinct() |> 
            mutate(n = row_number()) |>
            rename(replaycenter_official_code = official_code) |> 
            pivot_wider(game_date,
                        names_from = "n",
                        names_sep = "",
                        values_from = c("replaycenter_official_code",
                                        "replaycenter_official"))
        }
        
        if (is_empty(league_games)) {
          league_date <- tibble(date = x)
        } else {
          league_date <- league_games |> 
            mutate(date = x) |> 
            left_join(league_replay)
        }
        return(league_date)
      })
      
    }
    # ref_info      <- json$nba$Table$rows
    # ref_info$date <- x
    # return(ref_info)
    
    return(leagues)
  }
})


# ---- save ---------------------------------------------------------------

# Save individual games based on nba_game_id
ind_games_csv <- purrr::map(ref_mapped, function(x) {
  purrr::map2(x, names(x), function(y, yy) {
    temp <- as.data.frame(y)
    game_date <- temp$date[1]
    
    write_csv(temp, paste0(ref_source, "/", yy, "/", game_date, ".csv"))
    
    return(data.frame(temp, league = yy))
  }) |> 
    bind_rows()
})

# ---- aggregate ----------------------------------------------------------

new_leagues <- bind_rows(ind_games_csv)

split(new_leagues, f = as.factor(new_leagues$league)) |> 
  list2env(globalenv())


# NBA Data
if (!file.exists(paste0(local_dir, "/nba_referee_assignments.csv"))) {
  nba |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/nba_referee_assignments.csv"))
} else {
  nba_old <- read_csv(paste0(local_dir, "/nba_referee_assignments.csv"),
                      col_types = cols(.default = "c"))
  nba |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    bind_rows(nba_old) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/nba_referee_assignments.csv"))
}

# G League
if (!file.exists(paste0(local_dir, "/gl_referee_assignments.csv"))) {
  gl |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/gl_referee_assignments.csv"))
} else {
  gl_old <- read_csv(paste0(local_dir, "/gl_referee_assignments.csv"),
                      col_types = cols(.default = "c"))
  gl |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    bind_rows(gl_old) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/gl_referee_assignments.csv"))
}

# WNBA Data
if (!file.exists(paste0(local_dir, "/wnba_referee_assignments.csv"))) {
  wnba |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/wnba_referee_assignments.csv"))
} else {
  wnba_old <- read_csv(paste0(local_dir, "/wnba_referee_assignments.csv"),
                      col_types = cols(.default = "c"))
  wnba |> 
    mutate(across(everything(), as.character)) |> 
    # For some reason there are duplicated dates
    select(-date, -league) |> 
    bind_rows(wnba_old) |> 
    distinct() |> 
    filter(!is.na(game_date)) |> 
    relocate(contains("replaycenter_official_code"), .after = last_col()) |> 
    arrange(game_code, game_id) |> 
    write_csv(paste0(local_dir, "/wnba_referee_assignments.csv"))
}
