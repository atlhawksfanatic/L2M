# 1-L2M-bkref.R
# Boxscore and referee data from bkref instead of the NBA API

# ---- start --------------------------------------------------------------

library("lubridate")
library("tidyverse")

local_dir   <- "1-tidy/L2M"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

team_list <- read_rds("0-data/bkref/team_ids.rds") %>% 
  rename(team_id = teamId)

# Which team names need to be adjusted to this?
team_dictionary <- team_list$abbreviation
names(team_dictionary) <- team_list$simpleName

team_names <- team_list$simpleName
names(team_names) <- team_list$abbreviation

bkref_dictionary <- team_list$simpleName
names(bkref_dictionary) <- team_list$abbreviation


# ---- l2m-games ----------------------------------------------------------

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

# 2019-20 scraped
scraped_2020 <- read_rds("0-data/L2M/2019-20/scraped_201920.rds") %>% 
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
  bind_rows(scraped_2020) %>% 
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

# ---- correct-players ----------------------------------------------------

# These are the bad L2M player names that need to be converted to the
#  correct name: bad name on the left, converted good right
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
                 "Marcus Morris Sr." = "Marcus Morris")

l2m_games <- l2m_games %>% 
  mutate(committing = ifelse(is.na(bad_players[committing]),
                             committing,
                             bad_players[committing]),
         disadvantaged = ifelse(is.na(bad_players[disadvantaged]),
                                disadvantaged,
                                bad_players[disadvantaged]))

# ---- bkref-players ------------------------------------------------------


# Discrepancies from how bkref handles their player names versus the NBA
#  bad on left, good on right
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
               "Cam Reddish" = "Cameron Reddish")
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

odd_bkref_dic <- odd_bkref$player_team
names(odd_bkref_dic) <- odd_bkref$player_name

# Box scores from bkref
bkref_games <- read_rds("0-data/bkref/bkref_box.rds") %>% 
  mutate(home = ifelse(is.na(team_cross[home]),
                       home, team_cross[home]),
         away = ifelse(is.na(team_cross[away]),
                       away, team_cross[away]),
         player_name = ifelse(is.na(bad_bkref[player_name]),
                              player_name, bad_bkref[player_name]),
         player_team = ifelse(is.na(team_dictionary[player_team]),
                              player_team, team_dictionary[player_team])) %>% 
  # 2019-20 bkref now has accents on names
  mutate(player_name = stringi::stri_trans_general(player_name,
                                                   id = "Latin-ASCII")) %>% 
  bind_rows(odd_bkref)

# Game specific box score parts from bkref
bkref_game_base <- bkref_games %>% 
  select(bkref_id, ref_1, ref_2, ref_3, attendance, date, home, away) %>% 
  distinct()

# Since the committing/disadvantaged can be from either roster, create 
#  situations where it could be either
committing_bkref <- bkref_games %>% 
  select(bkref_id, committing = player_name, committing_min = player_min,
         committing_team = player_team, committing_side = player_side)
disadvantaged_bkref <- bkref_games %>% 
  select(bkref_id, disadvantaged = player_name, disadvantaged_min = player_min,
         disadvantaged_team = player_team, disadvantaged_side = player_side)

odd_bkref_c <- rename(odd_bkref,
                      committing = player_name,
                      committing_team = player_team)
odd_bkref_d <- rename(odd_bkref,
                      disadvantaged = player_name,
                      disadvantaged_team = player_team)

l2m_games_bkref <- l2m_games %>% 
  left_join(bkref_game_base) %>% 
  left_join(committing_bkref) %>% 
  left_join(disadvantaged_bkref) %>% 
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
l2m_games_bkref <- l2m_games_bkref %>% 
  mutate(type2 = ifelse(is.na(type_correct[type]), type, type_correct[type]),
         time = ifelse(time == "00:96", "00:56", time),
         # Time Remaining is problematic here
         time_min = parse_number(str_extract(time, ".+?(?=:)")),
         time_sec = parse_number(str_remove(time, ".+?(?=:)")),
         time2 = time_min + time_sec / 60) %>% 
  arrange(bkref_id)


write_csv(l2m_games_bkref, paste0(local_dir, "/L2M.csv"))
write_rds(l2m_games_bkref, paste0(local_dir, "/L2M.rds"))

