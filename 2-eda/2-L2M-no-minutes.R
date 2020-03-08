# 2-L2M-no-minutes.R
# Which calls involve players that didn't play any minutes in a game?

# ---- start --------------------------------------------------------------

library("lubridate")
library("tidyverse")

local_dir   <- "2-eda/L2M"
figures     <- paste0(local_dir, "/figures")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(figures)) dir.create(figures, recursive = T)

l2m <- read_rds("1-tidy/L2M/L2M.rds") %>% 
  mutate(decision_2 = ifelse(is.na(decision) | decision == "",
                             "INC", decision),
         decision_3 = ifelse(is.na(decision) | decision == "",
                             "inc", decision),
         correct = ifelse(decision_2 %in% c("CC", "CNC"),
                          "correct", "incorrect"),
         correct_num = 1*(correct == "correct"),
         foul = ifelse(decision_2 %in% c("CC", "INC"),
                       "foul", "clean"),
         foul_num = 1*(foul == "foul"),
         temp_year = ifelse(month(date) < 10, 2016, 2015),
         temp_date = as.Date(paste(temp_year, month(date),
                                   day(date), sep = "/"), "%Y/%m/%d"))

l2m %>%
  filter(date > "2019-12-10") %>%
  View()

# Some of the odd player names in L2M which need a corresponding team
odd_bkref <- tribble(
  ~player_name, ~player_team,
  "", "",
  "76ers", "PHI",
  "Alvin Gentry", "NOP",
  "Atkinson Kenny", "BKN",
  "Atkinson, Kenny", "BKN",
  "Blatt David", "CLE",
  "Blatt, David", "CLE",
  "Brett Brown", "PHI",
  "Bucks", "MIL",
  "Bulls", "CHI",
  "Cavaliers", "CLE",
  "Celtics", "BOS",
  "Clifford Steve", "CHA",
  "Clifford, Steve", "CHA",
  "Clippers", "LAC",
  "Doc Rivers", "LAC",
  "Dwane Casey", "TOR",
  "Grizzlies", "MEM",
  "Hawks", "ATL",
  "Heat", "MIA",
  "Hoiberg Fred", "CHI",
  "Hoiberg, Fred", "CHI",
  "Hornets", "CHA",
  "Igor Kokoskov", "PHX",
  "Jazz", "UTA",
  "Kidd Jason", "MIL",
  "Kidd, Jason", "MIL",
  "Kings", "SAC",
  "Knicks", "NYK",
  "Lakers", "LAL",
  "Magic", "ORL",
  "Mavericks", "DAL",
  "Michael Malone", "DEN",
  "Mike Woodson", "LAC",
  "Nets", "BKN",
  "Nuggets", "DEN",
  "Pacers", "IND",
  "Pelicans", "NOP",
  "Pistons", "DET",
  "Raptors", "TOR",
  "Rockets", "HOU",
  "Saunders, Flip", "MIN",
  "Spurs", "SAS",
  "Steve Clifford", "CHA",
  "Suns", "PHX",
  "Timberwolves", "MIN",
  "Thunder", "OKC",
  "Trail Blazers", "POR",
  "Tyronn Lue", "CLE",
  "Warriors", "GSW",
  "Wizards", "WAS")

# ---- no-minutes ---------------------------------------------------------


l2m %>% 
  filter((is.na(committing_min) | is.na(disadvantaged_min)),
         !(committing %in% odd_bkref$player_name),
         !(disadvantaged %in% odd_bkref$player_name),
         !is.na(disadvantaged)) %>%
  write_csv(paste0(figures, "/no_minutes.csv"))

l2m %>% 
  filter((is.na(committing_min) | is.na(disadvantaged_min)),
         !(committing %in% odd_bkref$player_name),
         !(disadvantaged %in% odd_bkref$player_name),
         !is.na(disadvantaged)) %>%
  arrange(desc(date)) %>% 
  select(date, away_team, home_team, period, time,
         committing, committing_min, disadvantaged, disadvantaged_min)
