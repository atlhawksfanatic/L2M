# 1-L2M-raw.R
# Raw L2M data

# ---- start --------------------------------------------------------------

library(lubridate)
library(tidyverse)

local_dir   <- "1-tidy/L2M"
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)

# ---- team-list ----------------------------------------------------------

# Only download the bkref team id list once and save it

if (file.exists(paste0(local_dir, "/team_ids.csv"))) {
  team_list <- read_csv(paste0(local_dir, "/team_ids.csv"))
} else {
  team_list <- paste0("https://raw.githubusercontent.com/bttmly/",
                      "nba/master/data/teams.json") %>% 
    httr::GET() %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% 
    mutate_all(as.character)
  
  write_csv(team_list, paste0(local_dir, "/team_ids.csv"))
}

team_dictionary        <- team_list$abbreviation
names(team_dictionary) <- team_list$simpleName

# ---- game-ids -----------------------------------------------------------

# Download all NBA game_id and schedule information
if (file.exists("0-data/stats_nba/nba_game_schedule.csv")) {
  id_list <- read_csv("0-data/stats_nba/nba_game_schedule.csv")
} else {
  print("Please download game schedule from 0-stats-nba-game-ids.R")
  id_list <- data.frame(gid = NA_character_) 
}

# ---- game-calls ---------------------------------------------------------

# Archived
archived <- read_csv("0-data/L2M/archive/pdftools_L2M_archive_all.csv",
                     col_types = cols(.default = "c")) %>% 
  mutate(call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         date = mdy(game_date),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team]) %>% 
  left_join(select(id_list, nba_game_id = gid, date, home, away))

# 2017-18
l2m_2018 <- read_csv("0-data/L2M/2017-18/pdftools_L2M_201718.csv",
                     col_types = cols(.default = "c")) %>% 
  mutate(date = mdy(game_date),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team]) %>% 
  left_join(select(id_list, nba_game_id = gid, date, home, away))

# 2018-19 pdfs
l2mpdf_2019 <- read_csv("0-data/L2M/2018-19/pdftools_L2M_201819_all.csv",
                        col_types = cols(.default = "c")) %>% 
  mutate(date = mdy(game_date),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team]) %>% 
  left_join(select(id_list, nba_game_id = gid, date, home, away))

# 2018-19 scraped
scraped_2019 <- read_csv("0-data/L2M/2018-19/scraped_201819.csv",
                         col_types = cols(.default = "c")) %>% 
  # select(-game_id, -away_score, -home_score) %>% 
  mutate(date = mdy(game_date),
         decision = case_when(decision == "NCC" ~ "CNC",
                              decision == "NCI" ~ "INC",
                              decision == "Undetectable" ~ "",
                              T ~ decision),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2019-20 pdfs
l2mpdf_2020 <- read_csv("0-data/L2M/2019-20/pdftools_L2M_201920_all.csv",
                        col_types = cols(.default = "c")) %>% 
  mutate(date = mdy(game_date),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team]) %>% 
  left_join(select(id_list, nba_game_id = gid, date, home, away))

# 2019-20 scraped
scraped_2020 <- read_csv("0-data/L2M/2019-20/scraped_201920.csv",
                         col_types = cols(.default = "c")) %>% 
  # select(-game_id, -away_score, -home_score) %>% 
  mutate(date = mdy(game_date),
         decision = case_when(decision == "NCC" ~ "CNC",
                              decision == "NCI" ~ "INC",
                              decision == "Undetectable" ~ "",
                              T ~ decision),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2020-21 scraped
scraped_2021 <- read_csv("0-data/L2M/2020-21/scraped_202021.csv",
                         col_types = cols(.default = "c")) %>% 
  # select(-game_id, -away_score, -home_score) %>% 
  mutate(date = mdy(game_date),
         decision = case_when(decision == "NCC" ~ "CNC",
                              decision == "NCI" ~ "INC",
                              decision == "Undetectable" ~ "",
                              T ~ decision),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2021-22 scraped
scraped_2022 <- read_csv("0-data/L2M/2021-22/scraped_202122.csv",
                         col_types = cols(.default = "c")) %>% 
  # select(-game_id, -away_score, -home_score) %>% 
  mutate(date = mdy(game_date),
         decision = case_when(decision == "NCC" ~ "CNC",
                              decision == "NCI" ~ "INC",
                              decision == "Undetectable" ~ "",
                              T ~ decision),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2022-23 scraped
scraped_2023 <- read_csv("0-data/L2M/2022-23/scraped_202223.csv",
                         col_types = cols(.default = "c")) %>% 
  # select(-game_id, -away_score, -home_score) %>% 
  mutate(date = mdy(game_date),
         decision = case_when(decision == "NCC" ~ "CNC",
                              decision == "NCI" ~ "INC",
                              decision == "Undetectable" ~ "",
                              T ~ decision),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# L2M are not consistent with the names of teams
team_cross <- c("BRK" = "BKN", "PHO" = "PHX")

bkref_cross <- c("BKN" = "BRK", "PHX" = "PHO", "CHA" = "CHO")

l2m_games <- archived %>% 
  bind_rows(l2m_2018) %>% 
  bind_rows(l2mpdf_2019) %>% 
  bind_rows(scraped_2019) %>% 
  bind_rows(l2mpdf_2020) %>% 
  bind_rows(scraped_2020) %>% 
  bind_rows(scraped_2021) %>% 
  bind_rows(scraped_2022) %>% 
  bind_rows(scraped_2023) %>% 
  # select(-away_score, -home_score) %>% 
  mutate(home = ifelse(is.na(team_cross[home]),
                       home, team_cross[home]),
         away = ifelse(is.na(team_cross[away]),
                       away, team_cross[away]),
         
         away_score = as.numeric(away_score),
         home_score = as.numeric(home_score),
         
         home_team = ifelse(is.na(team_dictionary[home]),
                            home, team_dictionary[home]),
         away_team = ifelse(is.na(team_dictionary[away]),
                            away, team_dictionary[away]),
         home_bkref = ifelse(is.na(bkref_cross[home_team]),
                             home_team, bkref_cross[home_team]),
         
         bkref_id = paste0(str_remove_all(date, "-"), "0",
                           home_bkref),
         nba_game_id = ifelse(is.na(nba_game_id),
                              str_remove(game_id, "gameId="),
                              nba_game_id)) %>% 
  arrange(date, game_id, period, desc(time), scrape_time) %>% 
  select(period:away, scrape_time:bkref_id, nba_game_id)

# Minor corrections
l2m_games_corrected <- l2m_games %>% 
  # https://official.nba.com/l2m/L2MReport.html?gameId=0021801148 has two plays
  #  at the bottom with time of "00:18." which are missing the milliseconds
  mutate(time = ifelse(time == "00:18." & nba_game_id == "0021801148",
                       "00:18.1", time)) %>% 
  # https://official.nba.com/l2m/L2MReport.html?gameId=0022000029 has a play
  #  missing the millisecond in overtime at "01:16."
  mutate(time = ifelse(time == "01:16." & nba_game_id == "0022000029",
                       "01:16.5", time))

write_csv(l2m_games_corrected, paste0(local_dir, "/L2M_raw.csv"))
write_rds(l2m_games_corrected, paste0(local_dir, "/L2M_raw.rds"))
