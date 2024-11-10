# 1-L2M-stats-nba.R
# Boxscore and referee data from stats.nba or official.nba

# ---- start --------------------------------------------------------------

library(lubridate)
library(tidyverse)

local_dir   <- "1-tidy/L2M"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)


# ---- team-list ----------------------------------------------------------

# Only download the bkref team id list once and save it

if (file.exists(paste0(local_dir, "/team_ids.csv"))) {
  team_list <- read_csv(paste0(local_dir, "/team_ids.csv"))
} else {
  team_list <- paste0("https://raw.githubusercontent.com/bttmly/",
                      "nba/master/data/teams.json") |> 
    httr::GET() |> 
    httr::content(as = "text", encoding = "UTF-8") |> 
    jsonlite::fromJSON() |> 
    mutate_all(as.character)
  
  write_csv(team_list, paste0(local_dir, "/team_ids.csv"))
}

team_dictionary        <- team_list$abbreviation
names(team_dictionary) <- team_list$simpleName

# L2M are not consistent with the names of teams
team_cross <- c("BRK" = "BKN", "PHO" = "PHX")
bkref_cross <- c("BKN" = "BRK", "PHX" = "PHO", "CHA" = "CHO")

# ---- l2m-games ----------------------------------------------------------

l2m_raw   <- read_csv("1-tidy/L2M/L2M_raw_api.csv",
                      col_types = cols(.default = "c")) |> 
  mutate(date = as.Date(date))

stats_box <- read_csv("0-data/stats_nba/stats_nba_box.csv",
                      col_types = cols(.default = "c")) |> 
  mutate(MIN = as.numeric(MIN),
         date = as.Date(date))

# ---- season-dates -------------------------------------------------------


l2m_raw_szns <-
  l2m_raw |> 
  mutate(season = case_when(date < as.Date("2015-10-01") ~ 2015,
                            date < as.Date("2016-10-01") ~ 2016,
                            date < as.Date("2017-10-01") ~ 2017,
                            date < as.Date("2018-10-01") ~ 2018,
                            date < as.Date("2019-10-01") ~ 2019,
                            date < as.Date("2020-11-01") ~ 2020,
                            date < as.Date("2021-10-01") ~ 2021,
                            date < as.Date("2022-10-01") ~ 2022,
                            date < as.Date("2023-10-01") ~ 2023,
                            date < as.Date("2024-10-01") ~ 2024,
                            date < as.Date("2025-10-01") ~ 2025,
                            T ~ NA_real_),
         # Last day of the regular season
         # April 15, 2015
         # April 13, 2016
         # April 12, 2017
         # April 11, 2018
         # April 10, 2019
         # April 15, 2020
         playoff = case_when(date > as.Date("2015-04-16") &
                               date < as.Date("2015-10-01") ~ T,
                             date > as.Date("2016-04-14") &
                               date < as.Date("2016-10-01") ~ T,
                             date > as.Date("2017-04-13") &
                               date < as.Date("2017-10-01") ~ T,
                             date > as.Date("2018-04-12") &
                               date < as.Date("2018-10-01") ~ T,
                             date > as.Date("2019-04-11") &
                               date < as.Date("2019-10-01") ~ T,
                             date > as.Date("2020-8-15") &
                               date < as.Date("2020-10-13") ~ T,
                             date > as.Date("2021-5-18") &
                               date < as.Date("2021-7-30") ~ T,
                             date > as.Date("2022-4-11") &
                               date < as.Date("2022-6-20") ~ T,
                             date > as.Date("2023-4-11") &
                               date < as.Date("2023-6-20") ~ T,
                             date > as.Date("2024-4-16") &
                               date < as.Date("2024-6-21") ~ T,
                             date > as.Date("2025-4-15") &
                               date < as.Date("2025-6-22") ~ T,
                             T ~ F))

# ---- correct-players ----------------------------------------------------

# These are the bad L2M player names that need to be converted to the correct
#  name. Sometimes it is due to a player altering their name and the NBA not
#  updating their L2Ms: bad name on the left, converted good right
bad_players <- c("Alfonso Burke" = "Trey Burke",
                 "Alfred Horford" = "Al Horford",
                 "CJ Watson" = "C.J. Watson",
                 "CJ Williams" = "C.J. Williams",
                 "Danuel House" = "Danuel House Jr.",
                 "Danuel House, Jr." = "Danuel House Jr.",
                 "Darryl Augustin" = "D.J. Augustin",
                 "DJ Augustin" = "D.J. Augustin",
                 "Demar DeRozan" = "DeMar DeRozan",
                 "Edrice Adebayo" = "Bam Adebayo",
                 "Forcier Chad" = "Evan Fournier",
                 "Forcier, Chad" = "Evan Fournier",
                 "Glenn Robinson III" = "Glenn Robinson",
                 "Grant Anthony" = "Jerami Grant",
                 "Grant, Anthony" = "Jerami Grant",
                 "Guillermo Hernangomez" = "Willy Hernangomez",
                 "Harrington Adam" = "Joe Harris",
                 "Harrington, Adam" = "Joe Harris",
                 "Ishmael Smith" = "Ish Smith",
                 "James Ennis III" = "James Ennis",
                 "Jeff Taylor" = "Jeffery Taylor",
                 "Jeffrey Teague" = "Jeff Teague",
                 "Johnny O'Bryant" = "Johnny O'Bryant III",
                 "Joseph Ingles" = "Joe Ingles",
                 "Joshua Hart" = "Josh Hart",
                 "Joshua Richardson" = "Josh Richardson",
                 "J.R. Smith" = "JR Smith",
                 "Jose Juan Barea" = "J.J. Barea",
                 "Juan Hernangomez" = "Juancho Hernangomez",
                 "Kelly Oubre Jr." = "Kelly Oubre",
                 "Louis Williams" = "Lou Williams",
                 "Monty Williams" = "Marvin Williams",
                 "Nene Hilario" = "Nene", 
                 "Otto Porter" = "Otto Porter Jr.",
                 "Patrick Mills" = "Patty Mills",
                 "Robert Williams III" = "Robert Williams",
                 "Ronald Baker" = "Ron Baker",
                 "Shaivonte Gilgeous-Alexander" = "Shai Gilgeous-Alexander",
                 "Steph Curry" = "Stephen Curry",
                 "TJ McConnell" = "T.J. McConnell",
                 "TJ Warren" = "T.J. Warren",
                 "Wes Matthews" = "Wesley Matthews",
                 "William Barton" = "Will Barton",
                 
                 # New problems for 2019-20
                 "P.J. Tucker" = "PJ Tucker",
                 "Kevin Knox II" = "Kevin Knox",
                 "Marcus Morris Sr." = "Marcus Morris",
                 "Cameron Reddish" = "Cam Reddish",
                 # 2020-21
                 "P.J. Dozier" = "PJ Dozier",
                 "Michael Frazier" = "Michael Frazier II",
                 "Nic Claxton" = "Nicolas Claxton",
                 # 2021-22
                 "Enes Kanter" = "Enes Freedom",
                 # 2023-24
                 "O.G. Anunoby" = "OG Anunoby",
                 "Craig Porter" = "Craig Porter Jr.")

l2m_games <- l2m_raw_szns |> 
  mutate(committing = ifelse(is.na(bad_players[committing]),
                             committing,
                             bad_players[committing]),
         disadvantaged = ifelse(is.na(bad_players[disadvantaged]),
                                disadvantaged,
                                bad_players[disadvantaged]))

# ---- stats-nba-players ------------------------------------------------------

# Game specific box score parts
stats_box_base <- stats_box |> 
  select(GAME_ID, contains("OFFICIAL"), ATTENDANCE, date, home, away) |> 
  distinct()


# Discrepancies from how stats.nba handles their player names versus the L2M
#  "bad" on left (stats.nba), "good" on right (L2M values) ie we want to keep
#  what the L2M shows for someone's name consistent throughout even if someone
#  changed their name like Enes Kanter->Freedom
bad_stat_names <- c("P.J. Tucker" = "PJ Tucker", # NBA is inconsistent in L2Ms
                    "Marcus Morris Sr." = "Marcus Morris",
                    "Kelly Oubre Jr." = "Kelly Oubre",
                    "James Ennis III" = "James Ennis",
                    "Robert Williams III" = "Robert Williams",
                    "Glenn Robinson III" = "Glenn Robinson",
                    "P.J. Dozier" = "PJ Dozier",
                    "Nic Claxton" = "Nicolas Claxton",
                    "Kevin Knox II" = "Kevin Knox",
                    "Frank Mason III" = "Frank Mason",
                    "T.J. Leaf" = "TJ Leaf",
                    "Walt Lemon Jr." = "Walter Lemon Jr.",
                    "O.G. Anunoby" = "OG Anunoby")
# Since the committing/disadvantaged can be from either roster, create 
#  situations where it could be either
stats_games <- stats_box |> 
  mutate(player_name = ifelse(is.na(bad_stat_names[PLAYER_NAME]),
                              PLAYER_NAME,
                              bad_stat_names[PLAYER_NAME]))

committing_box <- stats_games |> 
  select(GAME_ID,
         committing = player_name,
         committing_min = MIN,
         committing_team = TEAM_ABBREVIATION,
         committing_side = PLAYER_SIDE)
disadvantaged_box <- stats_games |> 
  select(GAME_ID,
         disadvantaged = player_name,
         disadvantaged_min = MIN,
         disadvantaged_team = TEAM_ABBREVIATION,
         disadvantaged_side = PLAYER_SIDE)


# Some of the odd player names in L2M that are coaches, need a map
still <- 2024
coach_stats <- tribble(
  ~player_name, ~player_team, ~season,
  "Alvin Gentry", "NOP", 2016:2020,
  "Alvin Gentry", "SAC", 2022,
  "Atkinson Kenny", "BKN", 2017:2020,
  "Atkinson, Kenny", "BKN", 2017:2020,
  "Blatt David", "CLE", 2015:2016,
  "Blatt, David", "CLE", 2015:2016,
  "Brett Brown", "PHI", 2015:2020,
  "Clifford Steve", "CHA", 2015:2018,
  "Clifford, Steve", "CHA", 2015:2018,
  "Doc Rivers", "LAC", 2015:2020,
  "Dwane Casey", "TOR", 2015:2018,
  "Hoiberg Fred", "CHI", 2016:2019,
  "Hoiberg, Fred", "CHI", 2016:2019,
  "Igor Kokoskov", "PHX", 2019,
  "Kidd Jason", "MIL", 2015:2018,
  "Kidd, Jason", "MIL", 2015:2018,
  "Michael Malone", "DEN", 2016:still,
  "Mike Budenholzer", "MIL", 2019:2023,
  "Mike Woodson", "LAC", 2015:2018,
  "Nick Nurse", "TOR", 2019:2023,
  "Nick Nurse", "PHI", still,
  "Saunders, Flip", "MIN", 2015,
  "Stan Van Gundy", "NOP", 2021,
  "Stephen Silas", "HOU", 2021:2023,
  "Steve Clifford", "CHA", 2015:2018,
  "Tyronn Lue", "CLE",  2016:2019) |> 
  unnest(season)

# Coach could be committing or disadvantaged
coach_stats_c <- rename(coach_stats,
                        committing = player_name,
                        coach_committing = player_team)
coach_stats_d <- rename(coach_stats,
                        disadvantaged = player_name,
                        coach_disadvantaged = player_team)

# Combine all the elements to the L2M calls

l2m_games_stats <- 
  l2m_games |> 
  left_join(stats_box_base) |> 
  left_join(committing_box) |> 
  left_join(disadvantaged_box) |>
  left_join(coach_stats_c) |> 
  left_join(coach_stats_d) |> 
  mutate(committing_team = ifelse(is.na(team_dictionary[committing]),
                                  committing_team, team_dictionary[committing]),
         disadvantaged_team = ifelse(is.na(team_dictionary[disadvantaged]),
                                     disadvantaged_team,
                                     team_dictionary[disadvantaged])) |> 
  # Was a coach involved?
  mutate(committing_team = ifelse(is.na(committing_team),
                                  coach_committing,
                                  committing_team),
         disadvantaged_team = ifelse(is.na(disadvantaged_team),
                                     coach_disadvantaged,
                                     disadvantaged_team)) |> 
  mutate(call = str_to_upper(call), type = str_to_upper(type),
         # Committing/Disadvantaged side to teams if NA
         committing_side = case_when(is.na(committing_side) &
                                       (committing_team == home) ~ "home",
                                     is.na(committing_side) &
                                       (committing_team == away) ~ "away",
                                     T ~ committing_side),
         disadvantaged_side = case_when(is.na(disadvantaged_side) &
                                          (disadvantaged_team == home) ~ "home",
                                        is.na(disadvantaged_side) &
                                          (disadvantaged_team == away) ~ "away",
                                        T ~ disadvantaged_side)) |> 
  select(-contains("coach"))
  

# Make corrections to types and calls
# Bad on left, good on right
type_correct <- c("10 SECOND" = "10 SECOND VIOLATION",
                  "24 SECOND" = "24 SECOND VIOLATION",
                  "3 SECOND" = "3 SECOND VIOLATION",
                  "3 SECONDS" = "3 SECOND VIOLATION",
                  "5 SECOND" = "5 SECOND VIOLATION",
                  "8 SECOND" = "8 SECOND VIOLATION",
                  "BAD PASS" = "OUT OF BOUNDS",
                  "DEFENSIVE 3 SECOND" = "DEFENSE 3 SECOND",
                  "DISCONTINUED DRIBBLE" = "DISCONTINUE DRIBBLE",
                  "DOUBLE LANE" = "LANE VIOLATION",
                  "INBOUND TURNOVER" = "INBOUND",
                  "JUMP BALL" = "JUMP BALL VIOLATION",
                  "JUMPBALL" = "JUMP BALL VIOLATION",
                  "KICKED BALL" = "KICKED BALL VIOLATION",
                  "LANE" = "LANE VIOLATION",
                  "LOOSE-BALL" = "LOOSE BALL",
                  "LOST BALL OUT OF BOUNDS" = "OUT OF BOUNDS",
                  "LOST BALL POSSESSION" = "OUT OF BOUNDS",
                  "OFFENSIVE CHARGE" = "OFFENSIVE",
                  "OUT-OF-BOUNDS" = "OUT OF BOUNDS",
                  "OUT OF BOUNDS - BAD PASS TURN" = "OUT OF BOUNDS",
                  "PERSONAL BLOCK" = "PERSONAL",
                  "SHOOTING BLOCK" = "SHOOTING",
                  "SHOOTING FOUL" = "SHOOTING",
                  "STEPPED OUT OF BOUNDS" = "OUT OF BOUNDS",
                  "SECOND TECHNICAL" = "TECHNICAL",
                  "DELAY TECHNICAL" = "TECHNICAL",
                  "DOUBLE TECHNICAL" = "TECHNICAL")

l2m_games_stats2 <-
  l2m_games_stats |> 
  mutate(type2 = ifelse(is.na(type_correct[type]), type, type_correct[type]),
         time = ifelse(time == "00:96", "00:56", time),
         # Time Remaining is problematic here
         time_min = parse_number(str_extract(time, ".+?(?=:)")),
         time_sec = parse_number(str_remove(time, ".+?(?=:)")),
         time2 = time_min + time_sec / 60) |> 
  arrange(gcode)


write_csv(l2m_games_stats2, paste0(local_dir, "/L2M_stats_nba.csv"))
write_rds(l2m_games_stats2, paste0(local_dir, "/L2M_stats_nba.rds"))


# ---- ref-shiny --------------------------------------------------------------

shiny_vrbls <- c("season", "gid", "period", "time", "call", "type",
                 "committing", "disadvantaged",
                 "decision", "decision_3", "comments", 
                 "OFFICIAL_1", "OFFICIAL_2", "OFFICIAL_3")

l2m_shiny <-
  l2m_games_stats2 |> 
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
                                   day(date), sep = "/"), "%Y/%m/%d")) |> 
  select(all_of(shiny_vrbls))

if (file.exists("3-shiny/ref-shiny/L2M_stats_nba.csv")) {
  write_csv(l2m_shiny, "3-shiny/ref-shiny/L2M_stats_nba.csv")
  write_rds(l2m_shiny, "3-shiny/ref-shiny/L2M_stats_nba.rds")
}
