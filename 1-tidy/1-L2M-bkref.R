# 1-L2M-bkref.R
# Boxscore and referee data from bkref instead of the NBA API

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

l2m_games <- read_csv("1-tidy/L2M/L2M_raw.csv",
                      col_types = cols(.default = "c"))

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
                 "Guillermo Hernangomez" = "Willy Hernangomez",
                 "Johnny O'Bryant" = "Johnny O'Bryant III",
                 "James Ennis III" = "James Ennis",
                 "Glenn Robinson III" = "Glenn Robinson",
                 "Robert Williams III" = "Robert Williams",
                 "Joseph Ingles" = "Joe Ingles",
                 "Joshua Richardson" = "Josh Richardson",
                 "J.R. Smith" = "JR Smith",
                 "Jose Juan Barea" = "J.J. Barea",
                 "Juan Hernangomez" = "Juancho Hernangomez",
                 "Louis Williams" = "Lou Williams",
                 "Nene Hilario" = "Nene", 
                 "Patrick Mills" = "Patty Mills",
                 "Ronald Baker" = "Ron Baker",
                 "Shaivonte Gilgeous-Alexander" = "Shai Gilgeous-Alexander",
                 "Steph Curry" = "Stephen Curry",
                 "TJ McConnell" = "T.J. McConnell",
                 "TJ Warren" = "T.J. Warren",
                 "William Barton" = "Will Barton",
                 "Ishmael Smith" = "Ish Smith",
                 "Jeffrey Teague" = "Jeff Teague",
                 "Joshua Hart" = "Josh Hart",
                 "Otto Porter" = "Otto Porter Jr.",
                 "Grant Anthony" = "Jerami Grant",
                 "Grant, Anthony" = "Jerami Grant",
                 "Wes Matthews" = "Wesley Matthews",
                 "Forcier Chad" = "Evan Fournier",
                 "Forcier, Chad" = "Evan Fournier",
                 "Harrington Adam" = "Joe Harris",
                 "Harrington, Adam" = "Joe Harris",
                 "Monty Williams" = "Marvin Williams",
                 "Kelly Oubre Jr." = "Kelly Oubre",
                 # New problems for 2019-20
                 "P.J. Tucker" = "PJ Tucker",
                 "Kevin Knox II" = "Kevin Knox",
                 "Marcus Morris Sr." = "Marcus Morris",
                 "Cameron Reddish" = "Cam Reddish",
                 # 2020-21
                 "P.J. Dozier" = "PJ Dozier",
                 "Nic Claxton" = "Nicolas Claxton",
                 # 2022-23
                 "OG Anunoby" = "O.G. Anunoby",
                 "Jeff Dowtin" = "Jeff Dowtin Jr.")

l2m_games <- l2m_games |> 
  mutate(committing = ifelse(is.na(bad_players[committing]),
                             committing,
                             bad_players[committing]),
         disadvantaged = ifelse(is.na(bad_players[disadvantaged]),
                                disadvantaged,
                                bad_players[disadvantaged]))

# ---- bkref-players ------------------------------------------------------


# Discrepancies from how bkref handles their player names versus the NBA
#  bad on left (bkref), good on right (L2M values)
bad_bkref <- c(# "Glenn Robinson" = "Glenn Robinson III",
               # "James Ennis" = "James Ennis III",
               # "Kelly Oubre" = "Kelly Oubre Jr.",
               "C.J. Miles" = "CJ Miles",
               "Nene Hilario" = "Nene",
               "Otto Porter" = "Otto Porter Jr.",
               "Tim Hardaway" = "Tim Hardaway Jr.",
               "J.R. Smith" = "JR Smith",
               "Jeffery Taylor" = "Jeff Taylor",
               "J.J. Redick" = "JJ Redick",
               "P.J. Tucker" = "PJ Tucker", # NBA is inconsistent in L2Ms
               "B.J. Johnson" = "BJ Johnson",
               "Danuel House" = "Danuel House Jr.",
               "Dennis Smith" = "Dennis Smith Jr.",
               "Derrick Jones" = "Derrick Jones Jr.",
               "Derrick Walton" = "Derrick Walton Jr.",
               "Gary Payton" = "Gary Payton II",
               "Gary Trent" = "Gary Trent Jr.",
               "Harry Giles" = "Harry Giles III",
               "Jaren Jackson" = "Jaren Jackson Jr.",
               "J.J. Hickson" = "JJ Hickson",
               "Johnny O'Bryant" = "Johnny O'Bryant III",
               "Juan Hernangomez" = "Juancho Hernangomez",
               "K.J. McDaniels" = "KJ McDaniels",
               "Larry Nance" = "Larry Nance Jr.",
               "Marvin Bagley" = "Marvin Bagley III",
               "Matt Williams" = "Matt Williams Jr.",
               "Mohamed Bamba" = "Mo Bamba",
               "P.J. Hairston" = "PJ Hairston",
               "Taurean Waller-Prince" = "Taurean Prince",
               "T.J. Leaf" = "TJ Leaf",
               "Troy Brown" = "Troy Brown Jr.",
               "Wade Baldwin" = "Wade Baldwin IV",
               "Walt Lemon" = "Walter Lemon Jr.",
               "Wendell Carter" = "Wendell Carter Jr.",
               "Wesley Iwundu" = "Wes Iwundu",
               # Problems with 2019-20
               "PJ Washington" = "P.J. Washington",
               # "Cam Reddish" = "Cameron Reddish",
               "Lonnie Walker" = "Lonnie Walker IV",
               "Jakob Pöltl" = "Jakob Poeltl",
               "Jakob Poltl" = "Jakob Poeltl",
               "Kevin Porter" = "Kevin Porter Jr.",
               "Anžejs Pasečņiks" = "Anzejs Pasecniks",
               "Sviatoslav Mykhailiuk" = "Svi Mykhailiuk",
               "Michael Porter" = "Michael Porter Jr.",
               "Melvin Frazier" = "Melvin Frazier Jr.",
               # 2020-21
               "Kenyon Martin" = "Kenyon Martin Jr.",
               "Kira Lewis" = "Kira Lewis Jr.",
               "Nic Claxton" = "Nicolas Claxton",
               # 2021-22
               "Herb Jones" = "Herbert Jones",
               "Trey Murphy" = "Trey Murphy III",
               "Cameron Thomas" = "Cam Thomas",
               "David Duke" = "David Duke Jr.",
               "Brandon Boston" = "Brandon Boston Jr.",
               "Duane Washington" = "Duane Washington Jr.",
               "Kelly Oubre Jr." = "Kelly Oubre",
               # 2022-23
               "OG Anunoby" = "O.G. Anunoby",
               "Jeff Dowtin" = "Jeff Dowtin Jr.",
               "Xavier Tillman Sr." = "Xavier Tillman")
# # 2019-20 bkref now has accents on names
# "Nicolò Melli" = "Nicolo Melli",
# "Nikola Vučević" = "Nikola Vucevic"

# Some of the odd player names in L2M which need a corresponding team
odd_bkref <- tribble(
  ~player_name, ~player_team,
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
  "Nick Nurse", "TOR",
  "Nuggets", "DEN",
  "Pacers", "IND",
  "Pelicans", "NOP",
  "Pistons", "DET",
  "Raptors", "TOR",
  "Rockets", "HOU",
  "Saunders, Flip", "MIN",
  "Spurs", "SAS",
  "Stephen Silas", "HOU",
  "Steve Clifford", "CHA",
  "Suns", "PHX",
  "Timberwolves", "MIN",
  "Thunder", "OKC",
  "Trail Blazers", "POR",
  "Tyronn Lue", "CLE",
  "Warriors", "GSW",
  "Wizards", "WAS")

odd_bkref_dic <- odd_bkref$player_team
names(odd_bkref_dic) <- odd_bkref$player_name

# Box scores from bkref
bkref_games <- read_csv("0-data/bkref/bkref_box.csv",
                        col_types = cols(.default = "c")) |> 
  # 2019-20 bkref now has accents on names
  mutate(player_name = stringi::stri_trans_general(player_name,
                                                   id = "Latin-ASCII")) |> 
  
  mutate(home = ifelse(is.na(team_cross[home]),
                       home, team_cross[home]),
         away = ifelse(is.na(team_cross[away]),
                       away, team_cross[away]),
         player_name = ifelse(is.na(bad_bkref[player_name]),
                              player_name, bad_bkref[player_name]),
         player_team = ifelse(is.na(team_dictionary[player_team]),
                              player_team, team_dictionary[player_team])) |> 
  bind_rows(odd_bkref)

# Game specific box score parts from bkref
bkref_game_base <- bkref_games |> 
  select(bkref_id, ref_1, ref_2, ref_3, attendance, date, home, away) |> 
  distinct()

# Since the committing/disadvantaged can be from either roster, create 
#  situations where it could be either
committing_bkref <- bkref_games |> 
  select(bkref_id, committing = player_name, committing_min = player_min,
         committing_team = player_team, committing_side = player_side)
disadvantaged_bkref <- bkref_games |> 
  select(bkref_id, disadvantaged = player_name, disadvantaged_min = player_min,
         disadvantaged_team = player_team, disadvantaged_side = player_side)

odd_bkref_c <- rename(odd_bkref,
                      committing = player_name,
                      committing_team = player_team)
odd_bkref_d <- rename(odd_bkref,
                      disadvantaged = player_name,
                      disadvantaged_team = player_team)

l2m_games_bkref <- l2m_games |> 
  left_join(bkref_game_base) |> 
  left_join(committing_bkref) |> 
  left_join(disadvantaged_bkref) |> 
  mutate(committing_team = ifelse(is.na(odd_bkref_dic[committing]),
                                  committing_team, odd_bkref_dic[committing]),
         disadvantaged_team = ifelse(is.na(odd_bkref_dic[disadvantaged]),
                                     disadvantaged_team,
                                     odd_bkref_dic[disadvantaged]),
         call = str_to_upper(call), type = str_to_upper(type),
         # Committing/Disadvantaged side to teams if NA
         committing_side = case_when(is.na(committing_side) &
                                       (committing_team == home_team) ~ "home",
                                     is.na(committing_side) &
                                       (committing_team == away_team) ~ "away",
                                     T ~ committing_side),
         disadvantaged_side = case_when(is.na(disadvantaged_side) &
                                       (disadvantaged_team == home_team) ~ "home",
                                     is.na(disadvantaged_side) &
                                       (disadvantaged_team == away_team) ~ "away",
                                     T ~ disadvantaged_side))

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
l2m_games_bkref <- l2m_games_bkref |> 
  mutate(type2 = ifelse(is.na(type_correct[type]), type, type_correct[type]),
         time = ifelse(time == "00:96", "00:56", time),
         # Time Remaining is problematic here
         time_min = parse_number(str_extract(time, ".+?(?=:)")),
         time_sec = parse_number(str_remove(time, ".+?(?=:)")),
         time2 = time_min + time_sec / 60) |> 
  arrange(bkref_id)

# new_season <- filter(l2m_games_bkref, date > "2019-10-10")
# new_season |> 
#   filter(is.na(committing_min))
# new_season |> 
#   filter(is.na(disadvantaged_min))

# ---- season-dates -------------------------------------------------------

l2m_games_bkref <-
  l2m_games_bkref |> 
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
                             T ~ F))

write_csv(l2m_games_bkref, paste0(local_dir, "/L2M.csv"))
write_rds(l2m_games_bkref, paste0(local_dir, "/L2M.rds"))


# ---- ref-shiny --------------------------------------------------------------

l2m_shiny <-
  l2m_games_bkref |> 
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

if (file.exists("3-shiny/ref-shiny/L2M.csv")) {
  write_csv(l2m_shiny, "3-shiny/ref-shiny/L2M.csv")
  write_rds(l2m_shiny, "3-shiny/ref-shiny/L2M.rds")
}
