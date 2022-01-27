# 0-L2M-pdftools-2017-18:

# ---- start --------------------------------------------------------------


library(pdftools)
library(stringr)
library(tidyverse)

local_dir   <- "0-data/L2M/2017-18"
raw_data    <- paste0(local_dir, "/raw")
if (!file.exists(raw_data)) dir.create(raw_data, recursive = T)

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
    
    if (n_cols == 6) {
      names(play_data) <- c("period", "time", "call_type", "committing",
                            "disadvantaged", "decision")
      
    } else if (n_cols == 7) {
      names(play_data) <- c("period", "time", "call_type", "committing",
                            "disadvantaged", "decision", "decision2")
      play_data <- play_data %>% 
        mutate(decision = ifelse(is.na(decision) | decision == "",
                                 decision2, decision)) %>% 
        select(-decision2)
      
    } else if (n_cols == 8) {
      # Hack for L2M-PHI-WAS-10-18-2017.pdf 
      if (basename(x) == "L2M-PHI-WAS-10-18-2017.pdf") {
        names(play_data) <- c("period", "time", "call_type", "X3",
                              "committing", "disadvantaged",
                              "X6", "decision")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type, X3)) %>% 
          select(-X3, -X6)
      } else {
        names(play_data) <- c("period", "time", "call_type",
                              "committing", "disadvantaged",
                              "X6",
                              "decision", "junk")
        # L2M-ORL-MEM-11-01-2017.pdf Hacks
        play_data <- play_data %>% 
          mutate(call_type = ifelse(X6 %in% c("Jonathon Simmons",
                                              "Nikola Vucevic",
                                              "Terrence Ross"),
                                    paste(call_type, committing),
                                    call_type),
                 committing = ifelse(X6 %in% c("Jonathon Simmons",
                                               "Nikola Vucevic",
                                               "Terrence Ross"),
                                     disadvantaged,
                                     committing),
                 disadvantaged = ifelse(X6 %in% c("Jonathon Simmons",
                                                  "Nikola Vucevic",
                                                  "Terrence Ross"),
                                        X6,
                                        disadvantaged)) %>% 
          select(-X6, -junk)
      }
      
    } else if (n_cols == 9) {
      # Hack for L2M-PHI-WAS-10-18-2017.pdf 
      if (basename(x) == "L2M-PHI-WAS-10-18-2017.pdf") {
        names(play_data) <- c("period", "time", "call_type", "X4",
                              "X5", "committing", "disadvantaged",
                              "X8", "decision")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type, X4)) %>% 
          select(-X4, -X5, -X8)
      } else {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2",
                              "committing", "disadvantaged",
                              "X7", "decision", "junk")
        play_data <- play_data %>%
          mutate(call_type = paste(call_type1, call_type2)) %>%
          select(-call_type1, -call_type2, -X7, -junk)
        
        # L2M-TOR-CLE-05-05-2018.pdf hacks
        play_data <- play_data %>% 
          # Turnover: Discontinue Dribble CJ Miles
          mutate(committing = ifelse(call_type == "Turnover: Discontinue Dribble CJ Miles",
                                     "CJ Miles", committing),
                 call_type = ifelse(call_type == "Turnover: Discontinue Dribble CJ Miles",
                                    "Turnover: Discontinue Dribble", call_type)) %>% 
          # Turnover: Traveling OG Anunoby
          mutate(committing = ifelse(call_type == "Turnover: Traveling OG Anunoby",
                                     "OG Anunoby", committing),
                 call_type = ifelse(call_type == "Turnover: Traveling OG Anunoby",
                                    "Turnover: Traveling", call_type)) %>% 
          # Foul: Shooting OG Anunoby
          mutate(disadvantaged = ifelse(call_type == "Foul: Shooting OG Anunoby",
                                        "LeBron James", disadvantaged),
                 committing = ifelse(call_type == "Foul: Shooting OG Anunoby",
                                     "OG Anunoby", committing),
                 call_type = ifelse(call_type == "Foul: Shooting OG Anunoby",
                                    "Foul: Shooting", call_type))
      }
      
    } else if (n_cols == 10) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2", "X5",
                            "committing", "disadvantaged", "junk1",
                            "decision", "junk2")
      play_data <- play_data %>% 
        # Bounds Allen Crabbe
        # Bounds Buddy Hield 
        # Bounds CJ Miles
        # Bounds Goran Dragic
        # Bounds Lonzo Ball 
        # Bounds Paul Millsap
        # Bounds Spencer Dinwiddie
        mutate(call_type = ifelse(X5 == "Bounds",
                                  paste(call_type1, call_type2, X5),
                                  paste(call_type1, call_type2)),
               committing = ifelse(X5 == "Bounds",
                                  committing,
                                  paste(X5, committing))) %>% 
        select(-junk1, -junk2, -X5, -call_type1, -call_type2)

    } else if (n_cols == 11) {
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
        select(-junk1, -junk2, -junk3,
               -call_type1, -call_type2,
               -player, -player1, -player2, -player3)
      
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
        select(-junk1, -junk2,
               -call_type1, -call_type2, -call_type3,
               -call_type4, -call_type5)
      # Gary Harris turnover hack
      play_data <- play_data %>% 
        mutate(committing = ifelse(call_type == "Turnover: Stepped out of Bounds Gary Harris",
                                   "Gary Harris", committing),
               call_type = ifelse(call_type == "Turnover: Stepped out of Bounds Gary Harris",
                                  "Turnover: Stepped out of Bounds",
                                  call_type))
      
    }
    
    # str_split(y, "\\s{2,}", simplify = T)
    temp_com <- str_remove(temp_info[grep("^Comment", temp_info)], "Comment:")
    comment  <- data.frame(comments = str_trim(temp_com))
    # add in an NA comment to account for the header
    comments <- bind_rows(data.frame(comments = NA), comment)
    
    if (nrow(play_data) == nrow(comments)) {
      results <- bind_cols(play_data, comments)
    } else if (nrow(play_data) > nrow(comments)) {
      # Should actually figure out a way to add on NAs to the end of comments
      #  and create a new column which states this is an error/warning
      n_rep <- nrow(play_data) - nrow(comments)
      
      comments <- bind_rows(data.frame(comments = NA),
                            comment,
                            data.frame(comments = rep(NA, n_rep)))
      
      results <- play_data %>% 
        mutate(comment_error = "warning, comments less than plays")
      
    } else if (nrow(play_data) < nrow(comments)) {
      results <- play_data %>% 
        mutate(comment_error = "error, comments exceed plays")
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
  select(-columns)

# Enter in the home and away teams plus final scores
corrections <- results %>% 
  mutate(game_date = word(game_details, -3, -1),
         temp_team = word(game_details, 1, -4),
         away1 = str_trim(str_remove(temp_team, "@.*$")),
         away_score = str_extract(away1,  "(?<=\\().+?(?=\\))"),
         away_team = str_trim(str_remove(away1, "\\(.*\\)")),
         home1 = str_trim(str_remove(temp_team, ".*@")),
         home_score = str_extract(home1,  "(?<=\\().+?(?=\\))"),
         home_team = str_trim(str_remove(home1, "\\(.*\\)")),
         # Austin Rivers was miscoded as Doc Rivers on March 4, 2018
         disadvantaged = ifelse(disadvantaged == "Doc Rivers", "Austin Rivers",
                                disadvantaged)) %>% 
  # Turn NAs into blanks
  replace_na(list(call_type = "", committing = "",
                  disadvantaged = "", decision = "")) %>%
  select(-temp_team, -away1, -home1)


write_csv(corrections, paste0(local_dir, "/pdftools_L2M_201718.csv"))
write_rds(corrections, paste0(local_dir, "/pdftools_L2M_201718.rds"))
