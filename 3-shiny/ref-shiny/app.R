library(DT)
library(lubridate)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(tidyr)

# l2m <- read.csv("L2M.csv", stringsAsFactors = FALSE)
# l2m <- read.csv("3-shiny/ref-shiny/L2M_stats_nba.csv", stringsAsFactors = FALSE)
l2m <- read.csv("L2M_stats_nba.csv", stringsAsFactors = FALSE)

# bios <- read.csv("3-shiny/ref-shiny/ref_bios.csv", stringsAsFactors = FALSE) |>
bios <- read.csv("ref_bios.csv", stringsAsFactors = FALSE) |>
  # Name typos
  mutate(ref_name = case_when(ref_name == "Suyash Metha" ~ "Suyash Mehta",
                              ref_name == "Joshua Tiven" ~ "Josh Tiven",
                              T ~ ref_name)) |> 
  mutate(Referee = case_when(ref_name == "Matthew Boland" ~ "Matt Boland",
                             ref_name == "James Capers" ~ "James Capers Jr.",
                             ref_name == "JB DeRosa" ~ "J.B. DeRosa",
                             # ref_name == "Joshua Tiven" ~ "Josh Tiven",
                             ref_name == "CJ Washington" ~ "C.J. Washington",
                             T ~ ref_name)) |> 
  select(Referee, ref_name, ref_url) |> 
  distinct()

l2m_ref <- l2m |> 
  pivot_longer(c(OFFICIAL_1, OFFICIAL_2, OFFICIAL_3),
               values_to = "Referee") |> 
  left_join(bios) |> 
  mutate(Referee = if_else(is.na(ref_name),
                           Referee,
                           paste0('<a href="', ref_url,
                                  '" target="_blank" >',
                                  ref_name, '</a>'))) |> 
  filter(!is.na(Referee)) |> 
  # Group by ref for output down the line
  group_by(Referee) |> 
  # Count how many games were refereed
  mutate(games = n_distinct(gid))

max_games <-
  l2m_ref |> 
  group_by(Referee) |> 
  summarise(games = n_distinct(gid))

vrbls_signif <- c("Calls per period",
                  "Correct Calls per period" = "CC per period",
                  "Correct Non-Calls per period" = "CNC per period",
                  "Incorrect Calls per period" = "IC per period",
                  "Incorrect Non-Calls per period" = "INC per period")
vrbls_pct    <- c("Correct Call Percentage",
                  "Bad Calls Percentage",
                  # "Good Graded Percentage",
                  "Bad Graded Percentage")
vrbls <- c(vrbls_signif, vrbls_pct)


# --- ui ------------------------------------------------------------------


ui <-  fluidPage(
  titlePanel("Basic L2M Referee Stats"),
  
  sidebarPanel(
    fluidRow(
      # Button
      downloadButton("download_raw", "Download raw L2M data")
    ),
    # Let's get a season variable
    fluidRow(
      prettyCheckboxGroup("season", "Season:",
                          choices = unique(as.character(l2m_ref$season)),
                          selected = unique(as.character(l2m_ref$season)),
                          inline = TRUE)
    ),
    # What if we have a date range instead of season?
    fluidRow(
      dateRangeInput("game_range",
                     label = "Date range of L2M reports:",
                     start = min(l2m$date), end = max(l2m$date),
                     min = min(l2m$date), max = max(l2m$date),
                     startview = "decade"
      )
    ), 
    fluidRow(
      actionButton("update_range", label = "Update Date Range")
    ),
    # Need to have a slider for number of games refereed
    fluidRow(
      sliderInput("obs", "Number of games refereed:", step = 1,
                  min = 0, max = max(max_games$games), 
                  value = c(0, max(max_games$games))
      )
    ),
    
    # What variables to display?
    fluidRow(
      pickerInput("vrbls", "Select variables to display:",
                  choices = vrbls,
                  selected = vrbls,
                  options = list("actions-box" = TRUE,
                                 size = 10,
                                 "selected-text-format" = "count > 3"),
                  multiple = TRUE)
    ),
    
    # # Now the away team selector
    fluidRow(
      pickerInput("team", "Team (regardless of home/away):",
                  choices = sort(unique(l2m_ref$home_team)),
                  selected = sort(unique(l2m_ref$home_team)),
                  options = list(`actions-box` = TRUE,
                                 size = 10,
                                 `selected-text-format` = "count > 3"),
                  multiple = TRUE)
    ),
    
    # fluidRow(
    #   pickerInput("away_team", "Away Team:",
    #               choices = sort(unique(l2m_ref$away_team)),
    #               selected = sort(unique(l2m_ref$away_team)),
    #               options = list(`actions-box` = TRUE,
    #                              size = 10,
    #                              `selected-text-format` = "count > 3"),
    #               multiple = TRUE)
    # ),
    # fluidRow(
    #   pickerInput("home_team", "Home Team:",
    #               choices = sort(unique(l2m_ref$home_team)),
    #               selected = sort(unique(l2m_ref$home_team)),
    #               options = list(`actions-box` = TRUE,
    #                              size = 10,
    #                              `selected-text-format` = "count > 3"),
    #               multiple = TRUE)
    # ),
    
    fluidRow(
      column(6, 
             pickerInput("away_team", "Away Team:",
                         choices = sort(unique(l2m_ref$away_team)),
                         selected = sort(unique(l2m_ref$away_team)),
                         options = list(`actions-box` = TRUE,
                                        size = 10,
                                        `selected-text-format` = "count > 3"),
                         multiple = TRUE)
      ),
      column(6,
             pickerInput("home_team", "Home Team:",
                         choices = sort(unique(l2m_ref$home_team)),
                         selected = sort(unique(l2m_ref$home_team)),
                         options = list(`actions-box` = TRUE,
                                        size = 10,
                                        `selected-text-format` = "count > 3"),
                         multiple = TRUE)
      )
    ),
    
    
    # Let's describe what the variables are
    p(withMathJax("Correct Calls Percentage is defined as: $$ \\frac{CC}{CC + IC} $$ ")),
    p("Bad Calls Percentage is defined as: $$ \\frac{IC + INC}{CC + IC + INC} $$"),
    p("Bad Graded Percentage is defined as: $$ \\frac{IC + INC}{CC + CNC + IC + INC} $$"),
    # p("Good Graded Percentage is defined as: $$ \\frac{CC + CNC}{CC + CNC + IC + INC} $$"),
    
    fluidRow(
      # Button
      downloadButton("download_current", "Download selected data")
    ),
  ),
  
  mainPanel(tableOutput("summary_table"),
            DT::dataTableOutput("table"))
)

# ---- server -------------------------------------------------------------


server <- function(input, output, session) {
  js <- c(
    "table.on('draw.dt', function(){",
    "  var PageInfo = table.page.info();",
    "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
    "    cell.innerHTML = i + 1 + PageInfo.start;",
    "  });",
    "})")
  
  l2m_data <- reactive({
    if (input$update_range == 0) {
      l2m_ref
    } else {
      # Only update the underlying data if the action button was hit
      isolate(l2m_ref |> 
                # Filter for the game range
                filter(date >= input$game_range[1],
                       date <= input$game_range[2]) |> 
                # Filter for the season
                filter(season %in% input$season) |> 
                # Group by ref for output down the line
                group_by(Referee) |> 
                # Count how many games were refereed
                mutate(games = n_distinct(gid)))
    }
  })
  
  observeEvent(input$update_range, {
    # Change the slider for games too
    updateSliderInput(session, "obs",
                      max = max(l2m_data()$games),
                      value = c(0, max(l2m_data()$games)))
  })
  
  ref_table <- reactive({ 
    l2m_data() |> 
      # Filter by the number of games refereed
      filter(games >= input$obs[1], games <= input$obs[2]) |> 
      # # Filter for the season
      # filter(season %in% input$season) |> 
      # Filter for the Team
      filter(away_team %in% input$team | home_team %in% input$team) |> 
      # Filter for the away teams
      filter(away_team %in% input$away_team) |> 
      # Filter for the home teams
      filter(home_team %in% input$home_team) |> 
      summarise(Games = n_distinct(gid),
                "Calls per period" = sum(decision_3 %in% c("CC", "IC")) /
                  n_distinct(gid, period),
                "CC per period" = sum(decision_3 == "CC") /
                  n_distinct(gid, period),
                "CNC per period" = sum(decision_3 == "CNC") /
                  n_distinct(gid, period),
                "IC per period" = sum(decision_3 == "IC") /
                  n_distinct(gid, period),
                "INC per period" = sum(decision_3 %in% c("INC", "inc")) /
                  n_distinct(gid, period),
                "Correct Call Percentage" = sum(decision_3 == "CC") /
                  sum(decision_3 %in% c("CC", "IC")),
                "Bad Calls Percentage" = sum(decision_3 %in%
                                               c("IC", "INC", "inc")) /
                  sum(decision_3 %in% c("CC", "IC", "INC", "inc")),
                # "Good Graded Percentage" = sum(decision_3 %in%
                #                                  c("CC", "CNC")) /
                #   sum(decision_3 %in% c("CC", "CNC", "IC", "INC", "inc")),
                "Bad Graded Percentage" = sum(decision_3 %in%
                                                c("IC", "INC", "inc")) /
                  sum(decision_3 %in% c("CC", "CNC", "IC", "INC", "inc"))) |> 
      arrange(desc(Games)) |> 
      select(Referee, Games, !!!input$vrbls)
  })
  
  sum_table <- reactive({ 
    l2m_data() |> 
      # Filter by the number of games refereed
      filter(games >= input$obs[1], games <= input$obs[2]) |> 
      # # Filter for the season
      # filter(season %in% input$season) |> 
      # Filter for the Team
      filter(away_team %in% input$team | home_team %in% input$team) |> 
      # Filter for the away teams
      filter(away_team %in% input$away_team) |> 
      # Filter for the home teams
      filter(home_team %in% input$home_team) |> 
      ungroup() |> 
      summarise(placeholder = "All Refs",
                Games = n_distinct(gid),
                "Calls per period" = sum(decision_3 %in% c("CC", "IC")) /
                  n_distinct(gid, period, Referee),
                "CC per period" = sum(decision_3 == "CC") /
                  n_distinct(gid, period, Referee),
                "CNC per period" = sum(decision_3 == "CNC") /
                  n_distinct(gid, period, Referee),
                "IC per period" = sum(decision_3 == "IC") /
                  n_distinct(gid, period, Referee),
                "INC per period" = sum(decision_3 %in% c("INC", "inc")) /
                  n_distinct(gid, period, Referee),
                "Correct Call Percentage" = sum(decision_3 == "CC") /
                  sum(decision_3 %in% c("CC", "IC")),
                "Bad Calls Percentage" = sum(decision_3 %in%
                                               c("IC", "INC", "inc")) /
                  sum(decision_3 %in% c("CC", "IC", "INC", "inc")),
                # "Good Graded Percentage" = sum(decision_3 %in%
                #                                  c("CC", "CNC")) /
                #   sum(decision_3 %in% c("CC", "CNC", "IC", "INC", "inc")),
                "Bad Graded Percentage" = sum(decision_3 %in%
                                                c("IC", "INC", "inc")) /
                  sum(decision_3 %in% c("CC", "CNC", "IC", "INC", "inc"))) |> 
      select(Referee = placeholder, Games, !!!input$vrbls)
  })
  
  
  output$table <- DT::renderDataTable(
    DT::datatable(ref_table(),
                  options = list(lengthMenu = list(c(15, 25, 50, -1),
                                                   c("15", "25", "50", "All")),
                                 pageLength = -1),
                  callback = JS(js),
                  escape = F
    ) |> 
      formatPercentage(intersect(input$vrbls, vrbls_pct), 2) |> 
      formatSignif(intersect(input$vrbls, vrbls_signif), 3)
  )
  
  output$summary_table <- renderTable(
    sum_table() |> 
      mutate_at(intersect(input$vrbls, vrbls_pct),
                ~scales::percent(., 0.01)) |> 
      mutate_at(intersect(input$vrbls, vrbls_signif), ~signif(., 3)) |> 
      mutate(Games = prettyNum(Games, big.mark = ","))
  )
  
  # Downloadable csv of selected dataset ----
  output$download_current <- downloadHandler(
    filename = function() {
      paste("L2M-Referees-custom", max(l2m$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ref_table(), file, row.names = FALSE)
    }
  )
  
  output$download_raw <- downloadHandler(
    filename = function() {
      paste("L2M-", max(l2m$date), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(l2m, file, row.names = FALSE)
    }
  )
}


# ---- create -------------------------------------------------------------

shinyApp(ui = ui, server = server)
