# 1-L2M-raw.R

# ---- start --------------------------------------------------------------

library("lubridate")
library("tidyverse")

local_dir   <- "1-tidy/L2M"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)


# ---- game-calls ---------------------------------------------------------

team_list <- read_rds("0-data/bkref/team_ids.rds")
team_dictionary <- team_list$abbreviation
names(team_dictionary) <- team_list$simpleName

# Archived
archived <- read_rds("0-data/L2M/archive/pdftools_L2M_archive_all.rds") %>% 
  mutate(call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         date = mdy(game_date),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2017-18
l2m_2018 <- read_rds("0-data/L2M/2017-18/pdftools_L2M_201718.rds") %>% 
  mutate(date = mdy(game_date),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2018-19 pdfs
l2mpdf_2019 <- read_rds("0-data/L2M/2018-19/pdftools_L2M_201819_all.rds") %>% 
  mutate(date = mdy(game_date),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team])

# 2018-19 scraped
scraped_2019 <- read_rds("0-data/L2M/2018-19/scraped_201819.rds") %>% 
  select(-game_id, -away_score, -home_score) %>% 
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
  select(-away_score, -home_score) %>% 
  mutate(home = ifelse(is.na(team_cross[home]),
                       home, team_cross[home]),
         away = ifelse(is.na(team_cross[away]),
                       away, team_cross[away]),
         
         home_team = ifelse(is.na(team_dictionary[home]),
                            home, team_dictionary[home]),
         away_team = ifelse(is.na(team_dictionary[away]),
                            away, team_dictionary[away]),
         home_bkref = ifelse(is.na(bkref_cross[home_team]),
                             home_team, bkref_cross[home_team]),
         
         bkref_id = paste0(str_remove_all(date, "-"), "0",
                           home_bkref))

write_csv(l2m_games, paste0(local_dir, "/L2M_raw.csv"))
write_rds(l2m_games, paste0(local_dir, "/L2M_raw.rds"))