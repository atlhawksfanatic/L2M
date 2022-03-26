
# ---- start --------------------------------------------------------------

library(tidyverse)

local_dir   <- "2-eda/L2M"
figures     <- paste0(local_dir, "/figures")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(figures)) dir.create(figures, recursive = T)

git_hub <- "https://github.com/atlhawksfanatic/L2M/raw/master/"
# Or if you want the local data, uncomment
# git_hub <- ""

games <- paste0(git_hub, "0-data/stats_nba/nba_game_schedule.csv") %>% 
  read_csv() %>% 
  rename(game_id = gid, game_code = gcode)

refs  <- paste0(git_hub, "0-data/official_nba/nba_referee_assignments.csv") %>% 
  read_csv() %>% 
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
         szn = as.numeric(str_sub(season, 2)) + 1) %>% 
  left_join(games)

# ---- rate-refs ----------------------------------------------------------

ref_rate_szn <- refs %>% 
  select(game_id, szn, szn_type, official1, official2, official3) %>% 
  pivot_longer(c(-game_id, -szn, -szn_type)) %>% 
  filter(!is.na(value)) %>% 
  # Assign points based on crew chief, referee, and umpire
  mutate(ref_points = case_when(name == "official1" ~ 5,
                                name == "official2" ~ 3,
                                name == "official3" ~ 1,
                                T ~ 0)) %>% 
  # For each season a ref has been involved in, sum up their total assignment
  #  points and how many games they were involved in to get their season rating
  group_by(szn, szn_type, official = value) %>% 
  summarise(ref_points = sum(ref_points),
            games = n(),
            ref_rating = ref_points / games)

# Join the ref's season rating with the list of assigned games
rated_refs <- refs %>% 
  select(game_id, home_team_abbr, away_team_abbr,
         szn, szn_type, national_tv,
         official1, official2, official3) %>% 
  pivot_longer(c(official1, official2, official3),
               names_to = "position", values_to = "official") %>% 
  filter(!is.na(official)) %>% 
  left_join(ref_rate_szn)

# ---- team-calc ----------------------------------------------------------

team_calc <- rated_refs %>% 
  pivot_longer(c(home_team_abbr, away_team_abbr),
               names_to = "loc", values_to = "team") %>% 
  mutate(nat_tv_indicator = national_tv %in% c("ABC", "ESPN", "TNT",
                                               "TNT/TBS", "ABC/ESPN",
                                               "TNT OT",
                                               "ESPN & ABC Simulcast")) %>% 
  group_by(szn, szn_type, team) %>% 
  summarise(ref_rating = mean(ref_rating, na.rm = T),
            games = n_distinct(game_id),
            total_officials = n_distinct(game_id, official),
            tv_games = n_distinct(game_id[nat_tv_indicator]))


season_calc <- team_calc %>% 
  ungroup() %>% 
  # filter(szn_type == "regular season") %>% 
  select(szn, szn_type, team, ref_rating, tv_games, games)

season_calc %>% 
  filter(szn == 2022, szn_type == "regular season") %>% 
  arrange(desc(ref_rating)) %>% 
  knitr::kable()

write_csv(season_calc, "2-eda/L2M/figures/haberstroh_team_ratings.csv")
write_csv(ref_rate_szn, "2-eda/L2M/figures/haberstroh_ref_ratings.csv")