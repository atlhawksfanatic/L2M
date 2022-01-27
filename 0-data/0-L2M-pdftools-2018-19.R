# 0-L2M-pdftools-2018-19:
#  Read in all the L2M PDFs with the pdftools package

# ---- start --------------------------------------------------------------

# Correct version of pdftools?
# devtools::install_version("pdftools", version = "2.2",
#                           repos = "http://cran.us.r-project.org")

library(pdftools)
library(stringr)
library(tidyverse)

local_dir   <- "0-data/L2M/2018-19"
raw_data    <- paste0(local_dir, "/raw")
if (!file.exists(raw_data)) dir.create(raw_data)

# ---- pdfs ---------------------------------------------------------------


raw_files <- dir(raw_data, full.names = T, pattern = "*.pdf")

pdf_raw <- map(raw_files, function(x) {
  print(x)
  raw_text <- pdf_text(x)
  raw_map <- map2(raw_text, seq_along(raw_text), function(y, page) {
    temp <- y %>% 
      str_split("\n") %>% 
      unlist() %>% 
      str_trim()
    
    # Each report will start off with the two teams and an @ symbol
    #  I don't think there's any other @s in a pdf
    begin <- grep("@", temp)
    # in case of another "@" instance just take the first
    if (length(begin) > 1) {
      begin <- begin[1]
      game_details = temp[begin]
    } else if (length(begin) == 1) {
      game_details = temp[begin]
      # But if it doesn't exist, then it's a secondary page so set to 1
    } else if (length(begin) == 0) { 
      begin = 1
      game_details = NA
    }
    
    # Each report finishes with a footer that describes the event assessments
    done  <- grep("Event Assessments", temp)
    # But there might not be any pages where this ends
    if (length(done) == 0) {
      done = length(temp) + 1
    }
    
    temp_info <- temp[(begin):(done - 1)]
    # L2M-NOP-DAL-12-26-2018.pdf has a messed up second page
    if (basename(x) == "L2M-NOP-DAL-12-26-2018.pdf" & page == 2) {
      temp_info <- temp[(begin + 3):(done - 1)]
      game_details = "Pelicans (119) @ Mavericks (122) (Dec 26, 2018)"
      
    }
    # Does the line contain a quarter reference? Then it's probably a play
    plays <- temp_info[c(grep("^Period", temp_info),
                         grep("^Q", temp_info))]
    
    if (length(plays) > 1) {
      play_data <- read_fwf(I(plays),
                            fwf_empty(I(plays)),
                            col_types = cols(.default = "c"),
                            na = character())
      
      # play_data <- read_table(plays, col_names = FALSE,
      #                         col_types = cols(.default = "c"))
      # play_data <- read_table(plays)
      # if (length(names(play_data)) == 7) {
      #   names(play_data) = c("period", "time", "call_type", "committing",
      #                        "disadvantaged", "decision", "video")
      # }
    } else if (length(plays) == 1) {
      play_data <- read_fwf(I(append(plays, NA)),
                            fwf_empty(I(plays)),
                            col_types = cols(.default = "c"),
                            na = character())
      
      # play_data <- read_table(append(plays, NA),
      #                         col_names = FALSE,
      #                         col_types = cols(.default = "c"))
      play_data <- play_data[1, ]
      play_data$error <- "warning - one line read"
    } else {
      play_data <- data.frame(error = "unknown error")
    }
    
    n_cols <- ncol(play_data)
    
    if (n_cols == 1) {
      play_data = data.frame(period = "Period")
    } else if (n_cols == 6) {
      names(play_data) <- c("period", "time", "call_type", "committing",
                            "disadvantaged", "decision")
      
    } else if (n_cols == 7) {
      if (basename(x) == "L2M-PHX-ORL-12-26-2018.pdf") {
        names(play_data) <- c("period", "time", "call_type1", "call_type2",
                              "committing", "disadvantaged", "decision")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type1, call_type2)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
      } else {
        names(play_data) <- c("period", "time", "call_type", "committing",
                              "disadvantaged", "decision", "decision2")
        play_data <- play_data %>% 
          mutate(decision = ifelse(is.na(decision) | decision == "",
                                   decision2, decision)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
      }
      
    } else if (n_cols == 8) {
      if (basename(x) == "L2M-PHX-ORL-12-26-2018.pdf") {
        # There's still an error with:
        # 00:09.2	Foul:	Foul		Loose Ball	Mikal Bridges
        names(play_data) <- c("period", "time", "call_type1", "call_type2",
                              "committing", "disadvantaged", "decision", "junk")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type1, call_type2)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
      } else {
        names(play_data) <- c("period", "time", "call_type",
                              "committing", "disadvantaged",
                              "X6",
                              "decision", "junk")
        play_data <- play_data %>% 
          mutate(call_type = ifelse(X6 == "Robin Lopez",
                                    paste(call_type, committing),
                                    call_type),
                 committing = ifelse(X6 == "Robin Lopez",
                                     disadvantaged,
                                     committing),
                 disadvantaged = ifelse(X6 == "Robin Lopez",
                                        X6,
                                        disadvantaged)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
      }
      
    } else if (n_cols == 9) {
      if (basename(x) == "L2M-SAC-PHX-01-08-2019.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2",
                              "call_type3", "call_type4",
                              "committing", "disadvantaged",
                              "decision")
        play_data <- play_data %>%
          mutate(call_type = str_squish(paste(call_type1, call_type2,
                                              call_type3, call_type4))) %>%
          select(period, time, call_type, committing, disadvantaged, decision)
      } else if (basename(x) == "L2M-SAS-DEN-12-28-2018.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2",
                              "call_type3",
                              "committing", "disadvantaged",
                              "decision", "junk")
        play_data <- play_data %>%
          mutate(call_type = str_squish(paste(call_type1, call_type2,
                                              call_type3))) %>%
          select(period, time, call_type, committing, disadvantaged, decision)
      } else {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2",
                              "player1", "player2",
                              "X7",
                              "decision", "junk")
        play_data <- play_data %>%
          mutate(call_type = ifelse(X7 %in% c("Josh Richardson", "Mike Conley"),
                                    paste(call_type1, call_type2, player1),
                                    paste(call_type1, call_type2)),
                 committing = ifelse(X7 %in% c("Josh Richardson", "Mike Conley"),
                                     player2,
                                     player1),
                 disadvantaged = ifelse(X7 %in% c("Josh Richardson", "Mike Conley"),
                                        X7,
                                        player2)) %>%
          select(period, time, call_type, committing, disadvantaged, decision)
        
      }
      
    } else if (n_cols == 10) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2", "X5",
                            "player1", "player2", "junk1",
                            "decision", "junk2")
      play_data <- play_data %>% 
        mutate(call_type = ifelse(X5 == "Bounds",
                                  paste(call_type1, call_type2, X5),
                                  paste(call_type1, call_type2)),
               committing = ifelse(X5 %in% c("Gordon Hayward",
                                             "Jayson Tatum",
                                             "Lou Williams",
                                             "Montrezl Harrell"),
                                   X5, player1),
               disadvantaged = ifelse(X5 %in% c("Gordon Hayward",
                                                "Jayson Tatum",
                                                "Lou Williams",
                                                "Montrezl Harrell"),
                                      paste(player1, player2), player2)) %>% 
        select(period, time, call_type, committing, disadvantaged, decision)
      
    } else if (n_cols == 11) {
      if (basename(x) == "L2M-HOU-NOP-12-29-2018.pdf") {
        names(play_data) <- c("period", "time",
                              "X3", "X4", "X5", "X6", "X7",
                              "committing",
                              "disadvantaged",
                              "decision", "junk3")
        play_data <- play_data %>% 
          mutate(call_type = str_trim(paste(X3, X4, X5, X6, X7))) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
      } else {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2", "junk1",
                              "player1", "player2", "player3",
                              "junk2",
                              "decision", "junk3")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type1, call_type2),
                 player = paste(player1, player2, player3),
                 committing = word(player, 1, 2),
                 disadvantaged = word(player, 3, 4)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
      }
      
    } else if (n_cols == 12) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2", "call_type3",
                            "call_type4", "call_type5",
                            "committing", "disadvantaged",
                            "junk1",
                            "decision", "junk2")
      play_data <- play_data %>% 
        mutate(call_type = paste(call_type1, call_type2, call_type3,
                                 call_type4, call_type5)) %>% 
        select(period, time, call_type, committing, disadvantaged, decision)
    }
    
    # str_split(y, "\\s{2,}", simplify = T)
    temp_com <- str_remove(temp_info[grep("^Comment", temp_info)], "Comment:")
    comment  <- data.frame(comments = str_trim(temp_com))
    # add in an NA comment to account for the header
    comments <- bind_rows(data.frame(comments = NA), comment)
    
    if (nrow(play_data) == nrow(comments)) {
      results <- bind_cols(play_data, comments)
    } else if (nrow(play_data) > nrow(comments)) {
      # Add on NAs to the end of comments with a column indicating warning
      n_rep <- nrow(play_data) - nrow(comments)
      
      comments <- bind_rows(data.frame(comments = NA),
                            comment,
                            data.frame(comments = rep(NA, n_rep)))
      
      results <- play_data %>% 
        mutate(comment_error = "warning, comments less than plays")
      
    } else if (nrow(play_data) < nrow(comments)) {
      comments2 <- comments %>% filter(!is.na(comments))
      
      # Sometimes this is an error because there is a blank comment
      if (nrow(play_data) == nrow(comments2)) {
        results <- bind_cols(play_data, comments2)
      } else {
        results <- play_data %>% 
          mutate(comment_error = "error, comments exceed plays")
      }
      
    }
    
    results$game_details <- game_details
    results$columns      <- n_cols
    results$page         <- page
    
    return(results)
  })
  
  raw_results <- raw_map %>% 
    bind_rows() %>% 
    mutate(file = basename(x)) %>% 
    fill(game_details)
  
  return(raw_results)
})

raw_results <- pdf_raw %>% 
  bind_rows() %>% 
  mutate_all(str_trim)

results <- raw_results %>% 
  filter(!(grepl("Period", period)), !is.na(period)) %>%
  select(period, time, call_type, committing, disadvantaged,
         decision, comments, game_details, page, file)

# Minor corrections found
results <- results %>% 
  # Correcting L2M-DEN-NOP-01-30-2019.pdf last page
  mutate(call_type = ifelse(committing == "Violation Nuggets",
                            "Turnover: 8 Second Violation", call_type),
         disadvantaged = ifelse(committing == "Violation Nuggets",
                                "Pelicans", disadvantaged),
         committing = ifelse(committing == "Violation Nuggets",
                             "Nuggets", committing)) %>% 
  # Correcting L2M-MEM-DEN-12-10-2018.pdf
  mutate(call_type = ifelse(committing == "Away from Play",
                            "Foul: Away from Play", call_type),
         disadvantaged = ifelse(committing == "Away from Play",
                                "Jamal Murray", disadvantaged),
         committing = ifelse(committing == "Away from Play",
                             "JaMychal Green", committing)) %>% 
  # Correcting an odd time in L2M-OKC-BOS-02-03-2019.pdf of "00:96" for a
  #  Westbrook play
  mutate(time = ifelse(time == "00:96", "00:56.9", time)) %>% 
  mutate_all(str_trim)

# Enter in the home and away teams plus final scores
corrections <- results %>% 
  mutate(game_date = word(game_details, -3, -1),
         game_date = str_remove_all(game_date, "\\)|\\("),
         temp_team = word(game_details, 1, -4),
         
         away1 = str_trim(str_remove(temp_team, "@.*$")),
         away_score = str_extract(away1,  "(?<=\\().+?(?=\\))"),
         away_team = str_trim(str_remove(away1, "\\(.*\\)")),
         home1 = str_trim(str_remove(temp_team, ".*@")),
         home_score = str_extract(home1,  "(?<=\\().+?(?=\\))"),
         home_team = str_trim(str_remove(home1, "\\(.*\\)"))) %>% 
  # Some missing scores to add in
  mutate(away_score = ifelse(game_details == "Pacers @ Bulls (Jan 04, 2019)",
                             "119", away_score),
         home_score = ifelse(game_details == "Pacers @ Bulls (Jan 04, 2019)",
                             "116", home_score),
         away_score = ifelse(game_details == "Spurs @ Lakers (Dec 05, 2018)",
                             "113", away_score),
         home_score = ifelse(game_details == "Spurs @ Lakers (Dec 05, 2018)",
                             "121", home_score),
         away_score = ifelse(game_details == "Nuggets @ Magic (Dec 05, 2018)",
                             "124", away_score),
         home_score = ifelse(game_details == "Nuggets @ Magic (Dec 05, 2018)",
                             "118", home_score),
         away_score = ifelse(game_details == "Thunder @ Nets (Dec 05, 2018)",
                             "114", away_score),
         home_score = ifelse(game_details == "Thunder @ Nets (Dec 05, 2018)",
                             "112", home_score)) %>% 
  # Turn NAs into blanks
  replace_na(list(call_type = "", committing = "", disadvantaged = "",
                  decision = "")) %>% 
  select(-temp_team, -away1, -home1)

# Some game_details on the second page are missing scores, so group by file
#  then fill in scores and ungroup:
corrections <- corrections %>% 
  group_by(file) %>% 
  fill(away_score, home_score) %>% 
  ungroup()

write_csv(corrections, paste0(local_dir, "/pdftools_L2M_201819_all.csv"))
write_rds(corrections, paste0(local_dir, "/pdftools_L2M_201819_all.rds"))
