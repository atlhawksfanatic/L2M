# 1-L2M-raw-stats-nba.R
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

# Archived PDFs
pdf_archived <- "0-data/L2M/archived-pdf/pdftools_L2M_archive_all.csv" %>% 
  read_csv(col_types = cols(.default = "c")) %>% 
  mutate(date = mdy(game_date),
         #PCTime = parse_time(time),
         call_type = ifelse(call_type == "N/A" | call_type == "Other",
                            NA_character_, call_type),
         call_type = str_squish(call_type),
         call = str_remove(call_type, ":.*"),
         type = str_trim(str_remove(call_type, ".*:")),
         home = team_dictionary[home_team],
         away = team_dictionary[away_team],
         away_score = as.numeric(away_score),
         home_score = as.numeric(home_score))

l2m_pdfs <- pdf_archived %>% 
  select(-home_score, -away_score) %>% 
  left_join(id_list)

# API names: left is API right is PDF
api_cross <- c("PeriodName" = "period",
               #"PCTime" = "time",
               # "ImposibleIndicator" = "",
               "Comment" = "comments",
               "CallRatingName" = "decision",
               "CallType" = "call_type",
               "CP" =  "committing",
               "DP" =  "disadvantaged",
               # "Difficulty" =  "",
               # "VideolLink" =  "",
               # "Qualifier" =  "",
               # "posID" =  "",
               # "posStart" =  "",
               # "posEnd" =  "",
               # "posTeamId" =  "",
               # "teamIdInFavor" =  "",
               # "errorInFavor" =  "",
               # "imgChart" =  "",
               "Home_team" =  "home_team",
               "Away_team" =  "away_team",
               # "GameId" =  "",
               "HomeTeamScore" =  "home_score",
               "VisitorTeamScore" =  "away_score",
               # "GameDate" =  "",
               # "HomeTeamId" =  "",
               # "AwayTeamId" =  "",
               "Home_team_abbr" =  "home",
               "Away_team_abbr" =  "away",
               # "L2M_Comments" =  "",
               "GameDateOut" =  "game_date")


# L2M API
l2m_api <- read_csv("0-data/official_nba/official_nba_l2m_api.csv",
                    col_types = cols(.default = "c")) %>% 
  # Get rid of reports that don't exist and only have game_id non-NA
  filter_at(vars(-game_id),
            any_vars(!is.na(.))) %>% 
  mutate(CallRatingName = case_when(CallRatingName == "NCC" ~ "CNC",
                                    CallRatingName == "NCI" ~ "INC",
                                    CallRatingName == "Undetectable" ~ "",
                                    T ~ CallRatingName),
         CallType = ifelse(CallType == "N/A" | CallType == "Other",
                            NA_character_, str_squish(CallType)),
         call = str_remove(CallType, ":.*"),
         Comment = str_trim(Comment),
         type = str_trim(str_remove(CallType, ".*:")),
         date = as.Date(str_sub(GameDate, 1, 10)),
         file = paste0(game_id, ".csv"),
         time = str_replace(PCTime, "(\\:.*?)\\:", "\\1.")) %>% 
  mutate_at(vars(HomeTeamScore, VisitorTeamScore), as.numeric) %>% 
  # rename uses the opposite convention
  rename(setNames(names(api_cross), api_cross))%>% 
  left_join(select(id_list, -away_score, -home_score))

# If a game has both a PDF and API set of calls, then only take the API version
l2m_games <- l2m_pdfs %>% 
  filter(!(gid %in% unique(l2m_api$GameId))) %>% 
  bind_rows(l2m_api) %>% 
  arrange(date, gid, period, desc(time))

write_csv(l2m_games, paste0(local_dir, "/L2M_raw_api.csv"))
write_rds(l2m_games, paste0(local_dir, "/L2M_raw_api.rds"))
