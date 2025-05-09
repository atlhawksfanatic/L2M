
# ---- start --------------------------------------------------------------

library(tidyverse)

local_dir   <- "2-eda/L2M"
figures     <- paste0(local_dir, "/figures")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(figures)) dir.create(figures, recursive = T)

git_hub <- "https://github.com/atlhawksfanatic/L2M/raw/master/"
# Or if you want the local data, uncomment
# git_hub <- ""

games <- paste0(git_hub, "0-data/stats_nba/nba_game_schedule.csv") |> 
  read_csv() |> 
  rename(game_id = gid, game_code = gcode)

refs  <- paste0(git_hub, "0-data/official_nba/nba_referee_assignments.csv") |> 
  read_csv() |> 
  # Season variable is XYYYY where X indicates the portion of the season
  #  (shown below) and YYYY is the year that the season starts
  # 001 : Pre Season
  # 002 : Regular Season
  # 003 : All-Star
  # 004 : Post Season
  # 005 : Play-In
  mutate(szn_type = case_when(str_sub(season, 1, 1) == "1" ~ "pre-season",
                              str_sub(season, 1, 1) == "2" ~ "regular season",
                              str_sub(season, 1, 1) == "3" ~ "all-star",
                              str_sub(season, 1, 1) == "4" ~ "playoffs",
                              str_sub(season, 1, 1) == "5" ~ "play-in",
                              T ~ NA_character_),
         szn = as.numeric(str_sub(season, 2)) + 1) |> 
  left_join(games)

# ---- rate-refs ----------------------------------------------------------

ref_rate_szn <- refs |> 
  select(game_id, szn, szn_type, official1, official2, official3) |> 
  pivot_longer(c(-game_id, -szn, -szn_type)) |> 
  filter(!is.na(value)) |> 
  # Assign points based on crew chief, referee, and umpire
  mutate(ref_points = case_when(name == "official1" ~ 5,
                                name == "official2" ~ 3,
                                name == "official3" ~ 1,
                                T ~ 0),
         # HACK Correct for the wrong official names that occur
         official = case_when(grepl("sha('?)rae mitchell", value,
                                    ignore.case = T) ~ "Sha'Rae Mitchell",
                              grepl("pat o('?)connell", value,
                                    ignore.case = T) ~ "Pat O'Connell",
                              T ~ value)) |> 
  # For each season a ref has been involved in, sum up their total assignment
  #  points and how many games they were involved in to get their season rating
  group_by(szn, szn_type, official) |> 
  summarise(ref_points = sum(ref_points),
            games = n(),
            ref_rating = ref_points / games)

# Join the ref's season rating with the list of assigned games
rated_refs <- refs |> 
  select(game_id, home_team_abbr, away_team_abbr,
         szn, szn_type, national_tv,
         official1, official2, official3) |> 
  pivot_longer(c(official1, official2, official3),
               names_to = "position", values_to = "official") |> 
  filter(!is.na(official)) |> 
  left_join(ref_rate_szn)

# ---- team-calc ----------------------------------------------------------

team_calc <- rated_refs |> 
  pivot_longer(c(home_team_abbr, away_team_abbr),
               names_to = "loc", values_to = "team") |> 
  mutate(nat_tv_indicator = national_tv %in% c("ABC", "ESPN", "TNT",
                                               "TNT/TBS", "ABC/ESPN",
                                               "TNT OT",
                                               "ESPN & ABC Simulcast")) |> 
  group_by(szn, szn_type, team) |> 
  summarise(ref_rating = mean(ref_rating, na.rm = T),
            games = n_distinct(game_id),
            total_officials = n_distinct(game_id, official),
            tv_games = n_distinct(game_id[nat_tv_indicator]))


season_calc <- team_calc |> 
  ungroup() |> 
  # filter(szn_type == "regular season") |> 
  select(szn, szn_type, team, ref_rating, tv_games, games)

season_calc |> 
  filter(szn == 2024, szn_type == "regular season") |> 
  arrange(desc(ref_rating)) |> 
  knitr::kable()

write_csv(season_calc, "2-eda/L2M/figures/haberstroh_team_ratings.csv")
write_csv(ref_rate_szn, "2-eda/L2M/figures/haberstroh_ref_ratings.csv")


# ---- playoffs -----------------------------------------------------------

# Bad/NBRA on left, good/NBA on right
ref_cross <- c("Joshua Tiven" = "Josh Tiven",
               "J.T. Orr" = "JT Orr",
               "Lauren Holtkamp-Sterling" = "Lauren Holtkamp",
               "Matthew Boland" = "Matt Boland",
               "Suyash Metha" = "Suyash Mehta")

ref_bios <- read_csv("0-data/NBRA/bios/ref_bios_recent.csv") |> 
  select(ref_name, ref_number, nba_exp) |> 
  mutate(official = ifelse(is.na(ref_cross[ref_name]),
                           ref_name,
                           ref_cross[ref_name]))

# https://official.nba.com/nba-announces-36-officials-selected-for-2022-nba-playoffs-presented-by-google-pixel/
# p_off <- paste0("Brent Barnaky, Curtis Blair, Tony Brothers, Nick Buchert, ",
#                 "James Capers, Kevin Cutler, Eric Dalen, Marc Davis, JB DeRosa,",
#                 " Mitchell Ervin, Kane Fitzgerald, Tyler Ford, Brian Forte, ",
#                 "Scott Foster, Pat Fraher, Jacyn Goble, John Goble, ",
#                 "David Guthrie, Bill Kennedy, Courtney Kirkland, Karl Lane, ",
#                 "Eric Lewis, Mark Lindsay, Tre Maddox, Ed Malloy, Rodney Mott,",
#                 " Gediminas Petraitis, Michael Smith, Ben Taylor, Josh Tiven, ",
#                 "Scott Twardoski, Justin Van Duyne, Tom Washington, ",
#                 "James Williams, Sean Wright, Zach Zarba")
# p_alt <- paste0("Ray Acosta, Matt Boland, Derrick Collins, Lauren Holtkamp, ",
#                 "Brett Nansel, Aaron Smith, Dedric Taylor, Leon Wood")

# # https://official.nba.com/nba-announces-36-officials-selected-for-2023-nba-playoffs-presented-by-google-pixel/
# p_off <- paste0("Ray Acosta, Brent Barnaky, Curtis Blair, Tony Brothers, ",
#                 "Nick Buchert, Sean Corbin, Kevin Cutler, Eric Dalen, ",
#                 "Marc Davis, JB DeRosa, Mitchell Ervin, Tyler Ford, ",
#                 "Brian Forte, Scott Foster, Pat Fraher, Jacyn Goble, ",
#                 "John Goble, David Guthrie, Bill Kennedy, Courtney Kirkland, ",
#                 "Karl Lane, Eric Lewis, Mark Lindsay, Tre Maddox, Ed Malloy, ",
#                 "Rodney Mott, Gediminas Petraitis, Kevin Scott, Aaron Smith, ",
#                 "Michael Smith, Ben Taylor, Josh Tiven, Justin Van Duyne, ",
#                 "James Williams, Sean Wright, Zach Zarba")
# p_alt <- paste0("Lauren Holtkamp, Ashley Moyer-Gleich, Natalie Sago, ",
#                 "Dedric Taylor, Scott Twardoski")
# 
# # https://twitter.com/NBAOfficial/status/1652803157527855104
# p2_off <- paste0("Curtis Blair, Tony Brothers, Nick Buchert, Sean Corbin, ",
#                  "Marc Davis, Mitchell Ervin, Tyler Ford, ",
#                  "Brian Forte, Scott Foster, Pat Fraher, Jacyn Goble, ",
#                  "John Goble, David Guthrie, Bill Kennedy, Courtney Kirkland, ",
#                  "Karl Lane, Eric Lewis, Mark Lindsay, Tre Maddox, Ed Malloy, ",
#                  "Rodney Mott, Gediminas Petraitis, Kevin Scott, Ben Taylor, ",
#                  "Josh Tiven, James Williams, Sean Wright, Zach Zarba")
# p2_alt <- paste0("Brent Barnaky, JB DeRosa, Aaron Smith, Justin Van Duyne")
# 
# # https://twitter.com/NBAOfficial/status/1658527767150096386
# p3_off <- paste0("Curtis Blair, Tony Brothers, Marc Davis, Tyler Ford, ",
#                  "Brian Forte, Scott Foster, John Goble, David Guthrie, ",
#                  "Bill Kennedy, Courtney Kirkland, Eric Lewis, Mark Lindsay, ",
#                  "Tre Maddox, Ed Malloy, Rodney Mott, Kevin Scott, Ben Taylor, ",
#                  "Josh Tiven, James Williams, Zach Zarba")
# p3_alt <- paste0("Nick Buchert, Mitchell Ervin, Pat Fraher, Jacyn Goble")


# https://official.nba.com/nba-announces-36-officials-selected-for-2024-nba-playoffs-presented-by-google-pixel/
p_off <- paste0("Ray Acosta, Brent Barnaky, Curtis Blair, Tony Brothers, ",
                "Nick Buchert, James Capers, Sean Corbin, Kevin Cutler, ",
                "Marc Davis, JB DeRosa, Mitchell Ervin, Tyler Ford, ",
                "Brian Forte, Scott Foster, Pat Fraher, Jacyn Goble, ",
                "John Goble, David Guthrie, Bill Kennedy, Courtney Kirkland, ",
                "Marat Kogut, Karl Lane, Mark Lindsay, Ed Malloy, ",
                "Ashley Moyer-Gleich, JT Orr, Gediminas Petraitis, ",
                "Kevin Scott, Aaron Smith, Ben Taylor, Dedric Taylor, ",
                "Josh Tiven, Justin Van Duyne, James Williams, ",
                "Sean Wright, Zach Zarba")
p_alt <- paste0("John Butler, Derrick Collins, Eric Dalen, Scott Twardoski")





parse_officials <- function(ref_string, ref_status) {
  ref_string |> 
    str_split(",", simplify = T) |> 
    t() |> 
    data.frame(official = _) |> 
    mutate(official = str_trim(official),
           status = ref_status)
}

first_round <- bind_rows(parse_officials(p_off, "Selected"),
                          parse_officials(p_alt, "Alternate")) |> 
  rename(first_round = status)

# second_round <- bind_rows(parse_officials(p2_off, "Selected"),
#                           parse_officials(p2_alt, "Alternate")) |> 
#   rename(second_round = status)
# 
# third_round <- bind_rows(parse_officials(p3_off, "Selected"),
#                          parse_officials(p3_alt, "Alternate")) |> 
#   rename(third_round = status)

poffs_2024 <- ref_rate_szn |> 
  filter(szn == 2024, szn_type == "regular season") |> 
  left_join(first_round) |> 
  # left_join(second_round) |> 
  # left_join(third_round) |> 
  left_join(ref_bios)

poffs_2024 |> 
  arrange(desc(nba_exp)) |> 
  select(szn, szn_type, official, nba_exp,
         ref_points, games, ref_rating, first_round) |> 
  View()
