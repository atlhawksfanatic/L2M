# 0-NBRA-bios.R
#  Referee biographies from their labor union:
#  https://www.nbra.net/nba-officials/referee-biographies/

# ---- start --------------------------------------------------------------

library("httr")
library("rvest")
library("tidyverse")
# library("splashr")

# Create a directory for the data
local_dir     <- "0-data/NBRA/bios"
data_source   <- paste0(local_dir, "/raw")
# scrape_source <- paste0(data_source, "/scraped_splashr")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source, recursive = T)
# if (!file.exists(scrape_source)) dir.create(scrape_source, recursive = T)

# ---- scrape -------------------------------------------------------------


url_nbra <- "https://www.nbra.net/nba-officials/referee-biographies/"

# read in url from above, then extract the links that comply with:
links_nbra <- read_html(url_nbra) %>% 
  html_nodes(".child")

# NEED TO: go back through and convert %% to |> and adjust the .
tidy_links <- 
  tibble(ref_name = links_nbra %>%
           html_node("[class='title']") %>%
           html_text() %>%
           str_remove_all("\n") %>%
           str_remove_all("[0-9]") %>%
           str_trim(),
         ref_number = links_nbra %>%
           html_node("[class='number']") %>%
           html_text(),
         ref_url = html_attr(links_nbra, "href"),
         ref_image = links_nbra %>%
           html_node("[class='image']") |>
           as.character() |> 
           str_extract("(?<=\\().+?(?=\\))") %>% 
           str_sub(2, nchar(.) - 1)) %>% 
  mutate(ref_image_file = paste0(data_source, "/",
                                 str_to_lower(str_replace_all(ref_name, " ",
                                                              "_")),
                                 ref_number, ".jpg"))

# ---- save-images --------------------------------------------------------

map2(tidy_links$ref_image_file, tidy_links$ref_image,
     function(x, y) {
       if (!file.exists(x) & y != "") download.file(y, x)
       }
     )


# ---- bio-descriptions ---------------------------------------------------

# go into each of the biography pages and extract information

if (file.exists("0-data/NBRA/bios/ref_bios.csv")) {
  nbra_bios_old <- read_csv("0-data/NBRA/bios/ref_bios.csv")
} else {
  nbra_bios_old <- data.frame(ref_name = NA)
}


map_bios <- map(tidy_links$ref_url, function(x) {
  print(x)
  temp_stats <- read_html(x) %>% 
    html_nodes("td , th") %>% 
    html_text()
  
  stats <- tibble(x1 = str_to_upper(temp_stats[seq(1, length(temp_stats), 2)]),
                  x2 = temp_stats[seq(2, length(temp_stats), 2)]) %>% 
    mutate_all(str_trim) %>% 
    # Hack for inconsistent "stats" about the referee
    mutate(x1 = case_when(x1 == "" ~ NA_character_,
                          x1 == "NBRA EXPERIENCE" ~ "NBA EXPERIENCE",
                          x1 == "MBA EXPERIENCE" ~ "NBA EXPERIENCE",
                          x1 == "BIRTHDAY" ~ "BORN",
                          x1 == "RESIDE" ~ "RESIDES",
                          x1 == "HIGH SCHOOL" ~ "HS",
                          x1 == "HS GLENDORA, CALIF. COLLEGE" ~ "COLLEGE",
                          x1 == "FAVORITE BOOKS" ~ "FAVORITE BOOK",
                          x1 == "FAVORITE TV SHOWS" ~ "FAVORITE TV SHOW",
                          x1 == "FAVORITE MEAT" ~ "FAVORITE MEAL",
                          x1 == "HIDDEN TALENTS" ~ "HIDDEN TALENT",
                          x1 == "WOULD MOST LIKE TO TRAVEL" ~
                            "WOULD MOST LIKE TO VISIT",
                          T ~ x1)) %>% 
    fill(x1) %>% 
    group_by(x1) %>% 
    summarise(x2 = paste(x2, collapse = "\n")) %>% 
    spread(x1, x2)
  
  temp_bio <- read_html(x) %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    paste(collapse = "\n")
  
  stats$BIO     <- temp_bio
  stats$scrape  <- Sys.time()
  stats$ref_url <- x
  
  Sys.sleep(3)
  return(stats)
})

nbra_bios <- map_bios %>% 
  bind_rows() %>% 
  # Consistent variable names
  rename_all(~str_to_lower(str_replace_all(., " ", "_"))) %>% 
  full_join(tidy_links) %>% 
  select(ref_name, ref_number, everything()) %>% 
  mutate(nba_exp = parse_number(nba_experience),
         ref_number = parse_number(ref_number))

# Joining together the new bios with the old, only keep the newest bio
both_bios <- bind_rows(nbra_bios, nbra_bios_old) %>% 
  # Hack for Matt Boland
  mutate(ref_name = case_when(ref_name == "Matt Boland" ~ "Matthew Boland",
                              T ~ ref_name)) %>% 
  arrange(ref_name, scrape) %>% 
  select(ref_name, ref_number, nba_exp, born, college, hs, nba_experience,
         resides, bio, scrape, everything())

# Dump them all
write_csv(both_bios, paste0(local_dir, "/ref_bios_all.csv"))
write_rds(both_bios, paste0(local_dir, "/ref_bios_all.rds"))

# Only keep the most recent scrape
recent_bios <- both_bios %>% 
  group_by(ref_name) %>% 
  filter(scrape == max(scrape))

write_csv(recent_bios, paste0(local_dir, "/ref_bios_recent.csv"))
write_rds(recent_bios, paste0(local_dir, "/ref_bios_recent.rds"))

write_csv(recent_bios, "3-shiny/ref-shiny/ref_bios.csv")
write_rds(recent_bios, "3-shiny/ref-shiny/ref_bios.rds")