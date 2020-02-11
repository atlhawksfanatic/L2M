# 0-L2M-pdftools-2019-20:
#  Read in all the L2M PDFs with the pdftools package

# ---- start --------------------------------------------------------------

library("pdftools")
library("stringr")
library("tidyverse")

local_dir   <- "0-data/L2M/2019-20"
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
    
    # Does the line contain a quarter reference? Then it's probably a play
    plays <- temp_info[c(grep("^Period", temp_info),
                         grep("^Q", temp_info))]
    
    if (length(plays) > 1) {
      play_data <- read_table(plays, col_names = FALSE,
                              col_types = cols(.default = "c"))
      # play_data <- read_table(plays)
      # if (length(names(play_data)) == 7) {
      #   names(play_data) = c("period", "time", "call_type", "committing",
      #                        "disadvantaged", "decision", "video")
      # }
    } else if (length(plays) == 1) {
      play_data <- read_table(append(plays, NA),
                              col_names = FALSE,
                              col_types = cols(.default = "c"))
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
      names(play_data) <- c("period", "time", "call_type1", "call_type2",
                            "committing", "disadvantaged", "decision")
      play_data <- play_data %>% 
        mutate(call_type = paste(call_type1, call_type2)) %>% 
        select(period, time, call_type, committing, disadvantaged, decision)
      
    } else if (n_cols == 8) {
      names(play_data) <- c("period", "time", "call_type1", "call_type2",
                            "committing", "disadvantaged", "decision", "junk")
      play_data <- play_data %>% 
        mutate(call_type = paste(call_type1, call_type2)) %>% 
        select(period, time, call_type, committing, disadvantaged, decision)
      
    } else if (n_cols == 10) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2", "X5",
                            "committing", "disadvantaged", "junk1",
                            "decision", "junk2")
      play_data <- play_data %>% 
        mutate(call_type  = paste(call_type1, call_type2),
               committing = paste(X5, committing)) %>% 
        select(-junk1, -junk2, -X5, -call_type1, -call_type2)
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
  mutate(away_score = ifelse(game_details == "Spurs @ Suns (Dec 14, 2019)",
                             "121", away_score),
         home_score = ifelse(game_details == "Spurs @ Suns (Dec 14, 2019)",
                             "119", home_score)) %>% 
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

write_csv(corrections, paste0(local_dir, "/pdftools_L2M_201920_all.csv"))
write_rds(corrections, paste0(local_dir, "/pdftools_L2M_201920_all.rds"))
