# 0-bkref-data.R

# ---- start --------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(tidyverse)

local_dir   <- "0-data/bkref"
data_source <- paste0(local_dir, "/raw")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)

# ---- game-calls ---------------------------------------------------------

l2m_games <- read_csv("1-tidy/L2M/L2M_raw.csv")

# ---- game-ids -----------------------------------------------------------

# Structure is simply: 201503010HOU.html - date then home team

bkref_games <- l2m_games %>% 
  select(bkref_id, date, home, away) %>% 
  distinct()

# Been getting an error about HTTP2 framing layer
httr::set_config(httr::config(http_version = 0))

if (file.exists("0-data/bkref/bkref_box.csv")) {
  old_box <- read_csv("0-data/bkref/bkref_box.csv")
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
      html_table(fill = TRUE)
    
    home_players <- url_tables[[1 + length(url_tables)/2]][-1, 1][[1]]
    home_mins    <- url_tables[[1 + length(url_tables)/2]][-1, 2][[1]]
    home_mins    <- as.numeric(str_extract(home_mins, ".+?(?=:)")) +
      as.numeric(str_remove(home_mins, ".*:")) / 60
    
    home <- tibble(player_name = home_players, player_min = home_mins,
                   player_side = "home")
    
    away_players <- url_tables[[1]][-1, 1][[1]]
    away_mins    <- url_tables[[1]][-1, 2][[1]]
    away_mins    <- as.numeric(str_extract(away_mins, ".+?(?=:)")) +
      as.numeric(str_remove(away_mins, ".*:")) / 60
    
    away <- tibble(player_name = away_players, player_min = away_mins,
                   player_side = "away")
    
    players <- bind_rows(home, away) %>% 
      filter(!(player_name %in% c("Reserves", "Team Totals")))
    
    bottom_info <- url_get %>% 
      read_html() %>% 
      html_nodes(paste0("#all_box_", tolower(home_team), "_advanced+ div")) %>% 
      html_text(trim = T) #%>% 
      # str_remove_all("\\n")
    
    if (length(bottom_info) == 0) {
      bottom_info <- url_get %>% 
        read_html() %>% 
        # Problems with the bottom_info no longer detectable. 
        html_text(trim = T) #%>% 
        # str_remove_all("\\n")
    }
    
    refs <- bottom_info %>% 
      str_extract(pattern = "(?<=Officials:).*") %>% 
      str_split(",", simplify = T) %>% 
      str_trim()
    attendance <- bottom_info %>% 
      str_extract(pattern = "(?<=Attendance:).*") %>% 
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
                        "Lauren Holtkamp", ref_3)) %>% 
  # Change instances of "Lauren Holtkamp" to "Lauren Holtkamp-Sterling"
  mutate(ref_1 = str_replace(ref_1, "Holtkamp$", "Holtkamp-Sterling"),
         ref_2 = str_replace(ref_2, "Holtkamp$", "Holtkamp-Sterling"),
         ref_3 = str_replace(ref_3, "Holtkamp$", "Holtkamp-Sterling")) %>% 
  arrange(bkref_id)


write_csv(box_scores, paste0(local_dir, "/bkref_box.csv"))
write_rds(box_scores, paste0(local_dir, "/bkref_box.rds"))
