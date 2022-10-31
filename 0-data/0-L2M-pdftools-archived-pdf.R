# 0-L2M-pdftools-archived-pdf:

# ---- start --------------------------------------------------------------


library(pdftools)
library(stringr)
library(tidyverse)

local_dir   <- "0-data/L2M/archived-pdf"
raw_data    <- paste0(local_dir, "/raw")
if (!file.exists(raw_data)) dir.create(raw_data, recursive = T)

raw_files <- dir(raw_data, full.names = T, pattern = "*.pdf")
# We have a corrected version of Indiana-Chicago we need to ignore
raw_files <- raw_files[!grepl("L2M-IND-CHI-11-16-15.pdf", raw_files)]

# ---- pdfs-map -----------------------------------------------------------

# Map across each PDF file
pdf_raw <- map(raw_files, function(x) {
  print(x)
  
  raw_text <- tryCatch(pdf_text(x), error = function(e) NA)
  if (is.na(raw_text)[1]) return(data.frame(error = "error",
                                            file = basename(x)))

## ---- map-by-page -------------------------------------------------------

  # Cycle through each of the pages
  raw_map <- map2(raw_text, seq_along(raw_text), function(y, page) {
    print(page)
    temp <- y %>% 
      str_split("\n") %>% 
      unlist() %>% 
      str_trim()
      
### ---- find-start-and-end ----------------------------------------------
    
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
    
    # Incorrect game date on a file
    if (basename(x) == "L2M-HOU-SAS-05-09-17.pdf") {
      game_details = "Rockets (107) @ Spurs (110) (May 09, 2017)"
    } else if (basename(x) == "L2M-BOS-WAS-05-12-17-2.pdf") {
      game_details = "Celtics (91) @ Wizards (92) (May 12, 2017)"
    }
    
    # Each report finishes with a footer that describes the event assessments
    done  <- grep("Event Assessments", temp)
    # But there might not be any pages where this ends
    if (length(done) == 0) {
      done = length(temp) + 1
    }
    
    # Subset to only the play information
    temp_info <- temp[(begin):(done - 1)]
    # L2M-NOP-DAL-12-26-2018.pdf has a messed up second page
    if (basename(x) == "L2M-NOP-DAL-12-26-2018.pdf" & page == 2) {
      temp_info <- temp[(begin + 3):(done - 1)]
      game_details = "Pelicans (119) @ Mavericks (122) (Dec 26, 2018)"
      
    }
    
    # Does the line contain a quarter reference? Then it's probably a play
    plays <- temp_info[c(grep("^Period", temp_info),
                         grep("^Q", temp_info))]
    
    # A few problems with plays extending beyond one line, create the alt_plays
    #  in the event that those exist
    first_line <- grep("^Period", temp_info)
    starting <- grep("^Q", temp_info)
    ending   <- grep("^Comment", temp_info)
    
    # Is the beginning of the line a call?
    call_start <- grep("^(CC|CNC|IC|INC)", temp_info)
    
    # What if starting and ending are not equal?
    if (length(starting) > length(ending)) {
      ending <- append(ending, starting[length(starting)])
    } else if (length(starting) < length(ending)) {
      starting <- append(ending[1], starting)
    }
    alt <- map2(starting, ending, function(x, y) {
      combine_lines <- seq(x, y - 1)
      temp_info[combine_lines]
    })
    
    if (basename(x) == "L2M-NYK-ATL-01-29-17.pdf") {
      alt <- pmap(list(starting, lag(starting), ending),
                  function(x, y, z) {
                    xx <- call_start[call_start < x & call_start > y]
                    combine_lines <- append(seq(x, z - 1), na.omit(xx))
                    temp_info[combine_lines]
                  })
    }
    
    alt_plays <- append(temp_info[first_line], unlist(alt))
    
    
    if (length(plays) > 1) {
      play_data <- read_fwf(I(plays),
                            fwf_empty(I(plays)),
                            col_types = cols(.default = "c"),
                            na = character())
      # play_data <- read_table(plays, col_names = FALSE,
      #                         col_types = cols(.default = "c"))
      
      alt_play_data <- tryCatch(read_fwf(I(alt_plays),
                                         fwf_empty(I(alt_plays)),
                                         col_types = cols(.default = "c"),
                                         na = character()),
                                error = function(e) NA)
      # alt_play_data <- tryCatch(read_table(alt_plays, col_names = FALSE,
      #                                      col_types = cols(.default = "c")),
      #                           error = function(e) NA)
      
      # Find the rows that do not start with Q or Period, move them all over
      #  one variable and then combine the two rows!!!
      alt_2 <- alt_play_data
      alt_2[!grepl(pattern = "^Q|^Period", alt_play_data$X1), 2] <-
        alt_play_data[!grepl(pattern = "^Q|^Period", alt_play_data$X1), 1]
      alt_2[!grepl(pattern = "^Q|^Period", alt_play_data$X1),1] <- NA
      # Now combine the rows
      good_rows <- map2(alt_2[which(is.na(alt_2[[1]])) - 1,],
                        alt_2[which(is.na(alt_2[[1]])),],
                        function(x, y) {
                          paste(x, y)
                        })
      good <- good_rows %>%
        bind_rows() %>%
        mutate_all(list(~str_remove_all(., "NA")))
      
      alt_2[which(is.na(alt_2[[1]])) - 1,] <- good
      alt_2 <- alt_2[which(!is.na(alt_2[[1]])),] %>%
        mutate_all(str_trim)
      
      # Problematic files which need to use the alt_plays
      if (basename(x) == "L2M-ATL-MIL-12-9-16.pdf") {
        alt_3 <- read_fwf(I(alt_2$X1),
                          fwf_empty(I(alt_2$X1)),
                          col_types = cols(.default = "c"),
                          na = character())
        # alt_3 <- read_table(alt_2$X1, col_names = FALSE)
        alt_3 <- bind_cols(alt_3, alt_2[-1])
        
        play_data <- alt_3 %>% 
          select(X1, X2 = X2...2, X3 = X3...3, X21 = X2...4, X10, X11, X12) %>% 
          rename(X4 = X21, X5 = X10, X6 = X11, X7 = X12) %>% 
          mutate(X2 = case_when(grepl("Dellavedova \\(MIL", X4) ~ "00:22.2",
                                grepl("LO SLA", X4) ~ "00:12.0",
                                grepl("Parker \\(MIL", X4) ~ "00:05.2",
                                grepl("Sefolosha \\(ATL", X4) ~ "00:01.6",
                                T ~ X2),
                 X3 = case_when(grepl("Dellavedova \\(MIL", X4) ~ "Foul: Personal",
                                grepl("LO SLA", X4) ~ "Violation: Jump Ball",
                                grepl("Parker \\(MIL", X4) ~ "Foul: Personal",
                                grepl("Sefolosha \\(ATL", X4) ~ "Foul: Shooting",
                                T ~ X3),
                 X5 = case_when(grepl("Dellavedova \\(MIL", X4) ~ "Dennis Schroder",
                                grepl("LO SLA", X4) ~ "Kyle Korver",
                                grepl("Parker \\(MIL", X4) ~ "Dennis Schroder",
                                grepl("Sefolosha \\(ATL", X4) ~ "Giannis Antetokounmpo",
                                T ~ X5),
                 X4 = case_when(grepl("Dellavedova \\(MIL", X4) ~ "Matthew Dellavedova",
                                grepl("LO SLA", X4) ~ "Greg Monroe",
                                grepl("Parker \\(MIL", X4) ~ "Jabari Parker",
                                grepl("Sefolosha \\(ATL", X4) ~ "Thabo Sefolosha",
                                T ~ X4))
        
      } else if (basename(x) == "L2M-HOU-OKC-11-16-16.pdf") {
        alt_3 <- read_fwf(I(alt_2$X1),
                          fwf_empty(I(alt_2$X1)),
                          col_types = cols(.default = "c"),
                          na = character())
        # alt_3 <- read_table(alt_2$X1, col_names = FALSE)
        alt_3 <- bind_cols(alt_3, alt_2[-1])
        
        play_data <- alt_3 %>% 
          select(X1, X2 = X2...2, X3 = X3...3, X4 = X4...4, X21 = X2...5, X10, X11, X12) %>% 
          rename(X5 = X21, X6 = X10, X7 = X11, X8 = X12) %>% 
          mutate(X2 = case_when(grepl("Gordon \\(HOU", X5) ~ "00:11.9",
                                grepl("Ariza \\(HOU", X5) ~ "00:10.0",
                                grepl("Westbrook \\(OKC", X5) ~ "00:08.0",
                                grepl("In an attempt", X5) ~ "00:07.1",
                                T ~ X2),
                 X3 = case_when(grepl("Gordon \\(HOU", X5) ~ "Foul: Personal",
                                grepl("Ariza \\(HOU", X5) ~ "Foul: Personal",
                                grepl("Westbrook \\(OKC", X5) ~ "Foul: Loose Ball",
                                grepl("In an attempt", X5) ~ "Turnover: Stepped out of Bounds",
                                T ~ X3),
                 X4 = case_when(grepl("Gordon \\(HOU", X5) ~ "Eric Gordon",
                                grepl("Ariza \\(HOU", X5) ~ "Trevor Ariza",
                                grepl("Westbrook \\(OKC", X5) ~ "Russell Westbrook",
                                grepl("In an attempt", X5) ~ "Russell Westbrook",
                                T ~ X4),
                 X6 = case_when(grepl("Gordon \\(HOU", X5) ~ "Victor Oladipo",
                                grepl("Ariza \\(HOU", X5) ~ "Alex Abrines",
                                grepl("Westbrook \\(OKC", X5) ~ "Clint Capela",
                                grepl("In an attempt", X5) ~ "",
                                T ~ X6)) %>% 
          select(-X5)
        
      } else if (basename(x) == "L2M-BKN-ORL-12-16-16.pdf") {
        play_data <- play_data %>% 
          mutate(X3 = ifelse(X2 == ".", "Instant Replay: Support Ruling", X3),
                 X4 = ifelse(X2 == ".", "", X4),
                 X5 = ifelse(X2 == ".", "", X5),
                 X6 = ifelse(X2 == ".", "CC", X6),
                 X2 = ifelse(X2 == ".", "00:08.7", X2))
        
      } else if (basename(x) == "L2M-BOS-OKC-12-11-16.pdf") {
        play_data <- play_data %>% 
          mutate(X3 = ifelse(X2 == "", "Turnover: Backcourt Turnover", X3),
                 X4 = ifelse(X2 == "", "Avery Bradley", X4),
                 X5 = ifelse(X2 == "", "", X5),
                 X2 = ifelse(X2 == "", "00:27.6", X2))
        
      } else if (basename(x) == "L2M-MIL-NYK-01-04-17.pdf") {
        alt_3 <- read_fwf(I(alt_2$X1),
                          fwf_empty(I(alt_2$X1)),
                          col_types = cols(.default = "c"),
                          na = character())
        # alt_3 <- read_table(alt_2$X1, col_names = FALSE)
        alt_3 <- bind_cols(alt_3, alt_2[-1])
        
        play_data <- alt_3 %>% 
          select(X1, X2 = X2...2, X3 = X3...3, X4 = X4...4, X5 = X2...5,
                 X6 = X3...6, X7 = X4...7) %>% 
          mutate(X2 = case_when(grepl("After communicating", X5) ~ "00:42.4",
                                grepl("Antetokounmpo \\(MIL\\) makes marginal", X5) ~ "00:37.7",
                                grepl("Noah \\(NYK\\) and", X5) ~ "00:33.7",
                                grepl("Noah \\(NYK\\) clamps", X5) ~ "00:33.7",
                                grepl("Rose \\(NYK\\) is inside", X5) ~ "00:33.7",
                                grepl("\\[Detectable", X5) ~ "00:02.8",
                                T ~ X2),
                 X3 = case_when(grepl("After communicating", X5) ~ X3,
                                grepl("Antetokounmpo \\(MIL\\) makes marginal", X5) ~ "Foul: Offensive",
                                grepl("Noah \\(NYK\\) and", X5) ~ "Foul: Loose Ball",
                                grepl("Noah \\(NYK\\) clamps", X5) ~ "Foul: Loose Ball",
                                grepl("Rose \\(NYK\\) is inside", X5) ~ "Violation: Lane",
                                grepl("\\[Detectable", X5) ~ "Turnover: 5 Second Violation",
                                T ~ X3),
                 X4 = case_when(grepl("After communicating", X5) ~ X4,
                                grepl("Antetokounmpo \\(MIL\\) makes marginal", X5) ~ "Giannis Antetokounmpo",
                                grepl("Noah \\(NYK\\) and", X5) ~ "Joakim Noah",
                                grepl("Noah \\(NYK\\) clamps", X5) ~ "Joakim Noah",
                                grepl("Rose \\(NYK\\) is inside", X5) ~ "Derrick Rose",
                                grepl("\\[Detectable", X5) ~ "Giannis Antetokounmpo",
                                T ~ X4),
                 X5 = case_when(grepl("After communicating", X5) ~ "",
                                grepl("Antetokounmpo \\(MIL\\) makes marginal", X5) ~ "Lance Thomas",
                                grepl("Noah \\(NYK\\) and", X5) ~ "Jabari Parker",
                                grepl("Noah \\(NYK\\) clamps", X5) ~ "Greg Monroe",
                                grepl("Rose \\(NYK\\) is inside", X5) ~ "",
                                grepl("\\[Detectable", X5) ~ "",
                                grepl("\\[Incidental", X5) ~ "",
                                T ~ X5))
        
      } else if (basename(x) == "L2M-TOR-BOS-02-01-17.pdf") {
        play_data <- play_data %>% 
          mutate(X3 = ifelse(X2 == "", "Instant Replay: Overturn Ruling", X3),
                 X4 = ifelse(X2 == "", "", X4),
                 X5 = ifelse(X2 == "", "", X5),
                 X2 = ifelse(X2 == "", "01:21.0", X2))
        
      } else if (basename(x) == "Hornets-90-Heat-88.pdf" & page == 1) {
        play_data <- alt_2
        
      }
      else if (basename(x) == "Hornets-90-Heat-88.pdf" & page == 2) {
        play_data <- play_data %>% 
          mutate(X3 = ifelse(X2 == "", "Instant Replay: Overturn Ruling", X3),
                 X4 = ifelse(X2 == "", "", X4),
                 X5 = ifelse(X2 == "", "", X5),
                 X2 = ifelse(X2 == "", "00:02.3", X2))
        
      } else if (basename(x) == "L2M-MIL-POR-03-21-17.pdf") {
        play_data <- alt_2 %>% 
          mutate(X5 = paste(X4, X5)) %>% 
          select(-X4)
        
      } else if (basename(x) %in%  c("L2M-DEN-LAL-01-31-17.pdf",
                                     "L2M-DET-TOR-02-12-17.pdf")) {
        play_data <- alt_2 %>% 
          mutate(X6 = paste(X5, X6)) %>% 
          select(-X5)
        
      } else if (basename(x) == "L2M-GSW-MIL-11-19-16.pdf" & page == 1) {
        play_data <- alt_2 %>% 
          mutate(X6 = paste(X5, X6)) %>% 
          select(-X5)
        
      } else if (basename(x) == "L2M-GSW-MIL-11-19-16.pdf" & page == 2) {
        play_data <- alt_2
        
      } else if (basename(x) == "L2M-IND-CLE-04-02-17.pdf" & page == 2) {
        play_data <- alt_2 %>% 
          mutate(X6 = paste(X5, X6)) %>% 
          select(-X5)
        
      } else if (basename(x) == "L2M-IND-CLE-04-02-17.pdf" & page != 2) {
        play_data <- alt_2
        
      } else if (basename(x) == "L2M-NYK-ATL-01-29-17.pdf" & page == 2) {
        play_data <- alt_2 %>% 
          mutate(X8 = ifelse(is.na(str_extract(X2, "(CC|CNC|IC|INC)")),
                             X8, str_extract(X2, "(CC|CNC|IC|INC)")),
                 X2 = str_trim(str_remove(X2, "(CC|CNC|IC|INC)")))
        
      } else if (basename(x) %in% c("L2M-ATL-MIL-03-24-17.pdf",
                                    "L2M-BOS-PHI-12-3-16.pdf",
                                    "L2M-CHI-MEM-01-15-17.pdf",
                                    "L2M-NOP-TOR-01-31-17.pdf",
                                    "L2M-NYK-ATL-01-29-17.pdf",
                                    "L2M-NYK-PHX-12-13-16.pdf",
                                    "L2M-OKC-HOU-01-05-17.pdf",
                                    "L2M-OKC-MIL-01-02-17.pdf")) {
        play_data <- alt_2
      }
      
    } else if (length(plays) == 1) {
      play_data <- read_fwf(I(append(plays, NA)),
                            fwf_empty(I(append(plays, NA))),
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

## ---- number-columns -----------------------------------------------------
    
    n_cols <- ncol(play_data)
    
    # Based on the number of columns, there are different ways to read in the
    #  data with a few exceptions

### ---- one-column --------------------------------------------------------
    
    if (n_cols == 1) {
      play_data = data.frame(period = "Period")
      
### ---- six-columns -------------------------------------------------------- 
    } else if (n_cols == 6) {
      names(play_data) <- c("period", "time", "call_type", "committing",
                            "disadvantaged", "decision")
### ---- seven-columns ------------------------------------------------------
    } else if (n_cols == 7) {
      if (basename(x) %in% c("L2M-PHX-ORL-12-26-2018.pdf",
                             "Spurs-121-Suns-119-OT.pdf")) {
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
### ---- eight-columns ------------------------------------------------------      
    } else if (n_cols == 8) {
      # Hack for L2M-PHI-WAS-10-18-2017.pdf 
      if (basename(x) == "L2M-PHI-WAS-10-18-2017.pdf") {
        names(play_data) <- c("period", "time", "call_type", "X3",
                              "committing", "disadvantaged",
                              "X6", "decision")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type, X3)) %>% 
          select(-X3, -X6)
      } else if (basename(x) == "L2M-PHX-ORL-12-26-2018.pdf") {
        # There's still an error with:
        # 00:09.2	Foul:	Foul		Loose Ball	Mikal Bridges
        names(play_data) <- c("period", "time", "call_type1", "call_type2",
                              "committing", "disadvantaged", "decision", "junk")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type1, call_type2)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
      } else if (basename(x) %in% c("L2M-ORL-MEM-11-01-2017.pdf",
                                    "L2M-CLE-CHI-01-27-2019.pdf")) {
        names(play_data) <- c("period", "time", "call_type",
                              "committing", "disadvantaged",
                              "X6",
                              "decision", "junk")
        play_data <- play_data %>%
          mutate(call_type = ifelse(X6 %in% c("Jonathon Simmons",
                                              "Nikola Vucevic",
                                              "Terrence Ross",
                                              "Robin Lopez"),
                                    paste(call_type, committing),
                                    call_type),
                 committing = ifelse(X6 %in% c("Jonathon Simmons",
                                               "Nikola Vucevic",
                                               "Terrence Ross",
                                               "Robin Lopez"),
                                     disadvantaged,
                                     committing),
                 disadvantaged = ifelse(X6 %in% c("Jonathon Simmons",
                                                  "Nikola Vucevic",
                                                  "Terrence Ross",
                                                  "Robin Lopez"),
                                        X6,
                                        disadvantaged)) %>%
          select(period, time, call_type, committing, disadvantaged, decision)
      } else {
        names(play_data) <- c("period", "time", "call_type",
                              "committing", "disadvantaged",
                              "X6",
                              "decision", "junk")
        # play_data <- play_data %>% 
        #   mutate(call_type = ifelse(X6 %in% c("Robin Lopez"),
        #                             paste(call_type, committing),
        #                             call_type),
        #          committing = ifelse(X6 %in% c("Robin Lopez"),
        #                              disadvantaged,
        #                              committing),
        #          disadvantaged = ifelse(X6 %in% c("Robin Lopez"),
        #                                 X6,
        #                                 disadvantaged))
        
        # Sometimes an Instant Replay will cause a split in call_type
        if ((sum(play_data$call_type == "", na.rm = T) > 0) |
            (any(grepl("Ruling", play_data$committing)))) {
          names(play_data) <- c("period", "time", "X3", "call_type",
                                "committing", "disadvantaged",
                                "decision", "junk")
          play_data <- play_data %>%
            mutate(call_type = paste(X3, call_type)) %>%
            select(period, time, call_type, committing,
                   disadvantaged, decision)
        } else {
          play_data <- play_data %>% 
            mutate(decision = paste(X6, decision)) %>% 
            select(period, time, call_type, committing,
                   disadvantaged, decision)
        }
      }
### ---- nine-columns -----------------------------------------------------
    } else if (n_cols == 9) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2",
                            "player1", "player2",
                            "X7",
                            "decision", "junk")
      if (basename(x) == "L2M-IND-CLE-04-02-17.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type", "player1",
                              "X5", "X6",
                              "X7",
                              "decision", "junk")
        play_data <- play_data %>% 
          mutate(committing = player1,
                 disadvantaged = paste(X5, X6, X7)) %>% 
          select(period, time, call_type, committing,
                 disadvantaged, decision)
      } else if (basename(x) == "L2M-NYK-ATL-01-29-17.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type", "player11",
                              "player12", "player21",
                              "player22",
                              "decision", "junk")
        play_data <- play_data %>% 
          mutate(committing = str_squish(str_c(player11, player12, sep = " ")),
                 disadvantaged = str_squish(str_c(player21, player22, sep = " "))) %>% 
          select(period, time, call_type, committing,
                 disadvantaged, decision)
      } else if (basename(x) == "L2M-CHI-MEM-01-15-17.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type", "player1",
                              "X5", "X6",
                              "X7",
                              "decision", "junk")
        play_data <- play_data %>% 
          mutate(committing = paste(player1, X5),
                 disadvantaged = paste(X6, X7)) %>% 
          select(period, time, call_type, committing,
                 disadvantaged, decision)
      } else if (basename(x) == "L2M-PHI-WAS-10-18-2017.pdf") {
        names(play_data) <- c("period", "time", "call_type", "X4",
                              "X5", "committing", "disadvantaged",
                              "X8", "decision")
        play_data <- play_data %>% 
          mutate(call_type = paste(call_type, X4)) %>% 
          select(-X4, -X5, -X8)
      } else if (basename(x) == "L2M-SAC-PHX-01-08-2019.pdf") {
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
        play_data <- play_data %>%
          mutate(call_type = ifelse(X7 %in% c("Josh Richardson", "Mike Conley"),
                                    paste(call_type1, call_type2, player1),
                                    paste(call_type1, call_type2)),
                 committing = ifelse(X7 %in% c("Josh Richardson", "Mike Conley"),
                                     player2,
                                     player1),
                 disadvantaged = ifelse(X7 %in% c("Josh Richardson",
                                                  "Mike Conley"),
                                        X7,
                                        player2))
      }
      # Problem with technical fouls some times
      if (sum(play_data$X7 %in% c("CC", "CNC", "IC", "INC"), na.rm = T) > 0) {
        play_data <- play_data %>% 
          mutate(call_type = call_type1,
                 committing = ifelse(call_type2 == "Saunders, Flip/JJ",
                                     "Saunders, Flip", call_type2),
                 disadvantaged = ifelse(call_type2 == "Saunders, Flip/JJ",
                                        "JJ Redick", player2),
                 decision = X7)
        
        if (sum(play_data$call_type == "", na.rm = T) > 0) {
          play_data <- play_data %>% 
            mutate(call_type = ifelse(X7 %in% c("Josh Richardson",
                                                "Mike Conley"),
                                      paste(call_type1, call_type2, player1),
                                      paste(call_type1, call_type2)),
                   committing = ifelse(X7 %in% c("Josh Richardson",
                                                 "Mike Conley"),
                                       player2,
                                       player1),
                   disadvantaged = ifelse(X7 %in% c("Josh Richardson",
                                                    "Mike Conley"),
                                          X7,
                                          player2)) %>%
            select(period, time, call_type, committing,
                   disadvantaged, decision)
        } else {
          play_data <- play_data %>%
            select(period, time, call_type, committing,
                   disadvantaged, decision)
        }
      } else {
        
        play_data <- play_data %>%
          # There is one problem with Chris Paul commiting a foul on Teague
          mutate(committing = ifelse(call_type == "Foul: Personal Chris",
                                     "Chris Paul", committing),
                 call_type = ifelse(call_type == "Foul: Personal Chris",
                                    "Foul: Personal", call_type)) %>% 
          # # Problem with Millsap fouling Carmelo
          # mutate(committing = ifelse(call_type == "Foul: Personal Paul",
          #                            "Paul Millsap", committing),
          #        disadvantaged = ifelse(call_type == "Foul: Personal Paul",
          #                               "Carmelo Anthony", disadvantaged),
          #        call_type = ifelse(call_type == "Foul: Personal Paul",
          #                           "Foul: Personal", call_type)) %>% 
          # Problem with Enes Kanter fouling Eric Gordon
          mutate(committing = ifelse(call_type == "Foul: Shooting Enes",
                                     "Enes Kanter", committing),
                 disadvantaged = ifelse(call_type == "Foul: Shooting Enes",
                                        "Eric Gordon", disadvantaged),
                 call_type = ifelse(call_type == "Foul: Shooting Enes",
                                    "Foul: Shooting", call_type)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
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
### ---- ten-columns ------------------------------------------------------
    } else if (n_cols == 10) {
      names(play_data) <- c("period", "time",
                            "call_type1", "call_type2", "X5",
                            "player1", "player2", "X8",
                            "decision", "X10")
      if (sum(play_data$X8 %in% c("CC", "CNC", "IC", "INC"), na.rm = T) > 0) {
        if (basename(x) == "L2M-LAL-MIN-3-25-15.pdf") {
          play_data <- play_data %>% 
            mutate(call_type = paste(call_type1, call_type2, X5),
                   committing = player1,
                   disadvantaged = player2,
                   decision = X8) %>% 
            select(period, time, call_type, committing,
                   disadvantaged, decision)
        } else {
          play_data <- play_data %>% 
            mutate(call_type = paste(call_type1, call_type2),
                   committing = X5,
                   disadvantaged = player1,
                   decision = X8) %>% 
            select(period, time, call_type, committing,
                   disadvantaged, decision)
          # play_data <- play_data %>% 
          #   mutate(committing = ifelse(X5 == "Bounds",
          #                              player1,
          #                              paste(X5, player1))) %>% 
          #   mutate(call_type = ifelse(X5 == "Bounds",
          #                             paste(call_type1, call_type2, X5),
          #                             paste(call_type1, call_type2)),
          #          committing = ifelse(X5 %in% c("Gordon Hayward",
          #                                        "Jayson Tatum",
          #                                        "Lou Williams",
          #                                        "Montrezl Harrell"),
          #                              X5, player1),
          #          disadvantaged = ifelse(X5 %in% c("Gordon Hayward",
          #                                           "Jayson Tatum",
          #                                           "Lou Williams",
          #                                           "Montrezl Harrell"),
          #                                 paste(player1, player2), player2)) %>% 
          #   select(period, time, call_type, committing, disadvantaged, decision)
        }
        
      } else if (basename(x) == "L2M-IND-CLE-04-25-2018.pdf") {
        play_data <- play_data %>%
          mutate(call_type = paste(call_type1, call_type2),
                 committing = paste(X5, player1),
                 disadvantaged = player2) %>% 
          select(period, time, call_type, committing,
                 disadvantaged, decision)
        
      } else {
        play_data <- play_data %>% 
          mutate(call_type = ifelse(X5 == "Bounds",
                                    paste(call_type1, call_type2, X5),
                                    paste(call_type1, call_type2)),
                 committing = ifelse(X5 %in% c("Gordon Hayward",
                                               "Jayson Tatum",
                                               "Lou Williams",
                                               "Montrezl Harrell",
                                               "Ian Clark"),
                                     X5, player1),
                 disadvantaged = ifelse(X5 %in% c("Gordon Hayward",
                                                  "Jayson Tatum",
                                                  "Lou Williams",
                                                  "Montrezl Harrell",
                                                  "Ian Clark"),
                                        paste(player1, player2), player2)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
      }
### ---- eleven-columns ---------------------------------------------------
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
      } else if (basename(x) == "L2M-DEN-NOP-01-30-2019.pdf") {
        names(play_data) <- c("period", "time",
                              "X3", "X4", "X5", "X6",
                              "committing",
                              "disadvantaged", "X9",
                              "decision", "junk3")
        play_data <- play_data %>% 
          mutate(call_type = str_trim(paste(X3, X4, X5, X6))) %>% 
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
### ---- twelve-columns ---------------------------------------------------      
    } else if (n_cols == 12) {
      if (basename(x) == "L2M-POR@MEM-2-8-16.pdf") {
        names(play_data) <- c("period", "time",
                              "call_type1", "call_type2", "call_type3",
                              "call_type4", "committing",
                              "X8", "X9",
                              "disadvantaged",
                              "decision", "junk2")
        play_data <- play_data %>% 
          mutate(call_type = ifelse(time == "",
                                    paste(call_type2, call_type3, call_type4),
                                    paste(call_type1, call_type2, call_type3,
                                          call_type4)),
                 time = ifelse(time == "", call_type1, time),
                 committing = ifelse(committing == "",
                                     paste(X8, X9), committing)) %>% 
          select(period, time, call_type, committing, disadvantaged, decision)
        
      } else {
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
        
      }
      
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
      # Add on NAs to the end of comments and create a new column which
      #  states this is an error/warning
      n_rep <- nrow(play_data) - nrow(comments)
      
      comments <- bind_rows(data.frame(comments = NA),
                            comment,
                            data.frame(comments = rep(NA, n_rep)))
      
      results <- play_data %>% 
        mutate(comment_error = "warning, comments less than plays") %>% 
        bind_cols(comments)
      
    } else if (nrow(play_data) < nrow(comments)) {
      n_rep <- nrow(play_data) - nrow(comments)
      
      overage <- data.frame(comments = comments[seq(0, n_rep),])
      
      results <- play_data %>% 
        mutate(comment_error = "error, comments exceed plays") %>% 
        bind_cols(overage)
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

# ---- bind ---------------------------------------------------------------

raw_results <- pdf_raw %>% 
  bind_rows() %>% 
  mutate_all(str_trim)

results <- raw_results %>% 
  filter(!(grepl("Period", period)), !is.na(period)) %>% 
  # Make the "Video" decisions blank
  mutate(decision = ifelse(decision == "Video", "", decision),
         # Bogdanovic error
         comments = ifelse(disadvantaged == "Otto Porter Bogdanovic",
                           paste0("Bogdanovic ", comments), comments),
         disadvantaged = ifelse(disadvantaged == "Otto Porter Bogdanovic",
                                "Otto Porter", disadvantaged)) %>% 
  # Gortat error
  mutate(comments = ifelse(disadvantaged == "Rondae Hollis-Jefferson Gortat",
                           paste0("Gortat ", comments), comments),
         disadvantaged = ifelse(disadvantaged == "Rondae Hollis-Jefferson Gortat",
                                "Rondae Hollis-Jefferson", disadvantaged)) %>% 
  # Committing and Disadvantaged to blanks
  mutate(committing = case_when(is.na(committing) ~ "",
                                committing == "," ~ "",
                                committing == "NA NA" ~ "",
                                T ~ committing),
         disadvantaged = case_when(is.na(disadvantaged) ~ "",
                                   disadvantaged == "," ~ "",
                                   disadvantaged == "NA NA" ~ "",
                                   T ~ disadvantaged)) %>%
  select(-columns, -comment_error) %>% 
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



# ---- quarter-issues -----------------------------------------------------

# Bad on left, good on right
period_cross <- c("Q3" = "Q4")
call_type_cross <- c("Jump Ball" = "Violation: Jump Ball",
                     "Other" = "Violation: Other",
                     "Other: Held Ball" = "Violation: Other",
                     "Other: Jump Ball" = "Violation: Jump Ball",
                     "Other: Timeout" = "Violation: Other",
                     "Shot Clock" = "Violation: Other")
decision_cross <- c("CC*" = "CC",
                    "CNC*" = "CNC",
                    "IC*" = "IC",
                    "INC*" = "INC")

results <- results %>% 
  mutate(period = ifelse(is.na(period_cross[period]),
                         period, period_cross[period]),
         call_type = ifelse(is.na(call_type_cross[call_type]),
                            call_type, call_type_cross[call_type]),
         decision = ifelse(is.na(decision_cross[decision]),
                           decision, decision_cross[decision]))


# ---- game-corrections ---------------------------------------------------


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
         home_team = str_trim(str_remove(home1, "\\(.*\\)")),
         # Austin Rivers was miscoded as Doc Rivers on March 4, 2018
         disadvantaged = ifelse(disadvantaged == "Doc Rivers", "Austin Rivers",
                                disadvantaged))  %>% 
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
                             "112", home_score),
         away_score = ifelse(game_details == "Spurs @ Suns (Dec 14, 2019)",
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


write_csv(corrections, paste0(local_dir, "/pdftools_L2M_archive_all.csv"))
write_rds(corrections, paste0(local_dir, "/pdftools_L2M_archive_all.rds"))