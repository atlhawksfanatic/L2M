# 0-bkref-data.R

# ---- start --------------------------------------------------------------

library("httr")
library("lubridate")
library("rvest")
# library("splashr")
library("tidyverse")

local_dir   <- "0-data/bkref"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)


# ---- team-list ----------------------------------------------------------

# Only download the bkref team id list once and save it

if (file.exists(paste0(local_dir, "/team_ids.rds"))) {
  team_list <- read_rds(paste0(local_dir, "/team_ids.rds"))
} else {
  team_list <- paste0("https://raw.githubusercontent.com/bttmly/",
                      "nba/master/data/teams.json") %>% 
    GET() %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>% 
    mutate_all(as.character)
  
  write_csv(team_list, paste0(local_dir, "/team_ids.csv"))
  write_rds(team_list, paste0(local_dir, "/team_ids.rds"))
  
}

team_dictionary <- team_list$abbreviation
names(team_dictionary) <- team_list$simpleName


# ---- game-calls ---------------------------------------------------------

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

# ---- game-ids -----------------------------------------------------------

# Structure is simply: 201503010HOU.html - date then home team

bkref_games <- l2m_games %>% 
  select(bkref_id, date, home, away) %>% 
  distinct()

# Been getting an error about HTTP2 framing layer
httr::set_config(httr::config(http_version = 0))

if (file.exists("0-data/bkref/bkref_box.rds")) {
  old_box <- read_rds("0-data/bkref/bkref_box.rds")
} else {
  old_box <- data.frame(bkref_id = NA)
}

# Only download the missing box scores:
bkref_games <- filter(bkref_games, !(bkref_id %in% old_box$bkref_id))

bkref_box_scores <- map(bkref_games$bkref_id, function(x) {
  print(x)
  Sys.sleep(runif(1, 2, 4))
  url_temp <- paste0("https://www.basketball-reference.com/boxscores/",
                     x,
                     ".html")
  home_team <- str_sub(x, -3)
  url_get <- GET(url_temp)
  
  if (url_get$status_code != 200) {
    bkref_results <- tibble(bkref_id = x,
                            home_players = "error", away_players = "error")
    return(bkref_results)
    
  } else {
    url_tables <- url_get %>% 
      read_html() %>% 
      html_table()
    
    home_players <- url_tables[[1 + length(url_tables)/2]][-1, 1]
    home_mins    <- url_tables[[1 + length(url_tables)/2]][-1, 2]
    home_mins    <- as.numeric(str_extract(home_mins, ".+?(?=:)")) +
      as.numeric(str_remove(home_mins, ".*:")) / 60
    
    home <- tibble(player_name = home_players, player_min = home_mins,
                   player_side = "home")
    
    away_players <- url_tables[[1]][-1, 1]
    away_mins    <- url_tables[[1]][-1, 2]
    away_mins    <- as.numeric(str_extract(away_mins, ".+?(?=:)")) +
      as.numeric(str_remove(away_mins, ".*:")) / 60
    
    away <- tibble(player_name = away_players, player_min = away_mins,
                   player_side = "away")
    
    players <- bind_rows(home, away) %>% 
      filter(!(player_name %in% c("Reserves", "Team Totals")))
    
    bottom_info <- url_get %>% 
      read_html() %>% 
      html_nodes(paste0("#all_box_", tolower(home_team), "_advanced+ div")) %>% 
      html_text(trim = T) %>% 
      str_remove_all("\\n")
    
    if (length(bottom_info) == 0) {
      bottom_info <- url_get %>% 
        read_html() %>% 
        # Problems with the bottom_info no longer detectable. 
        html_text(trim = T) %>% 
        str_remove_all("\\n")
    }
    
    refs <- bottom_info %>% 
      str_extract(pattern = "(?<=Officials:).*(?=Attendance)") %>% 
      str_split(",", simplify = T) %>% 
      str_trim()
    attendance <- bottom_info %>% 
      str_extract(pattern = "(?<=Attendance:).*(?=Time)") %>% 
      parse_number()
    if (is.na(attendance)) {
        attendance <- bottom_info %>% 
          str_extract(pattern = "(?<=Attendance:).*") %>% 
          parse_number()
      }
    
    
    bkref_results <- tibble(bkref_id = x,
                            player_name = players$player_name,
                            player_side = players$player_side,
                            player_min = players$player_min,
                            ref_1 = refs[1], ref_2 = refs[2], ref_3 = refs[3],
                            attendance)
    return(bkref_results)
  }
})

# If there are no box scores downloaded, set the box scores to the old box scores
if (is_empty(bkref_box_scores)) {
  box_scores <- old_box
} else {
  new_box <- bkref_box_scores %>% 
    bind_rows() %>% 
    left_join(bkref_games) %>% 
    mutate(player_team = ifelse(player_side == "home", home, away))
  
  # Add in the new box scores to the old ones
  box_scores <- bind_rows(old_box, new_box) %>% 
    filter(!is.na(bkref_id))
}

# Hack for Lauren Holtkamp missing from referee assignments
holt_missing = c("201912090NOP", "201912060CHI")
box_scores <- box_scores %>% 
  mutate(ref_3 = ifelse(is.na(ref_3) & bkref_id %in% holt_missing,
                        "Lauren Holtkamp", ref_3))

write_csv(box_scores, paste0(local_dir, "/bkref_box.csv"))
write_rds(box_scores, paste0(local_dir, "/bkref_box.rds"))
