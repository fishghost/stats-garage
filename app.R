#### OWL Stats Garage ####

### Load libraries ###
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(tibble)
  
  library(shiny)
  library(shinyjs)
  library(ggplot2)
  library(ggimage)
})

#### Sheets and Constants ####
owl_colours <- c("wins" = "#FA9C1D", "upset_wins" = "#FFC360", "favoured_loses" = "#868A8E", "loses" = "#4A4C4E")
team_colours <- readr::read_csv("www/TeamLogos/TeamColours.csv", col_types = "cffcccc")
# logos_path <- "Shiny/www/TeamLogos"
logos_path <- "www/TeamLogos"
team_colours <- team_colours %>%
  mutate(logo = paste(file.path(logos_path, abb),".png", sep = ""))

heroes <- readr::read_delim("www/Data/HeroRoles.csv", delim = ";", col_types = c("cccc"))

hero_roles <- list(
  tank = heroes$hero_name[heroes$role == "tank"], 
  damage = heroes$hero_name[heroes$role == "damage"], 
  support = heroes$hero_name[heroes$role == "support"]
)

hero_colours <- heroes %>% 
  select(hero_name, colour) %>%
  deframe()

player_cols <- "Ticffcfffd"
match_cols <- "TTfiiffffiiifffffddddddii"


old_data <- read_rds(file.path("www", "Data", "RDS", "old_data.rds"))
new_data <- read_rds(file.path("www", "Data", "RDS", "new_data.rds"))
player <- bind_rows(old_data, new_data) %>%
  # Ensure hero names levels are in alphabetical order
  mutate(hero_name = reorder(hero_name, match(hero_name, sort(levels(hero_name)))))
map <- read_rds(file.path("www", "Data", "RDS", "map_data.rds"))

stage_dates <- as.Date(c(
    "2018-01-08",
    "2018-02-20", 
    "2018-04-04",
    "2018-05-06", 
    "2018-07-10", 
    "2019-02-13", 
    "2019-04-04",
    "2019-06-05", 
    "2019-07-24",
    "2019-08-29", 
    "2020-02-07",
    "2020-09-02", 
    "2021-04-15",
    "2021-05-20", 
    "2021-06-24", 
    "2021-07-29", 
    "2021-09-03",
    as.character(Sys.Date())
))
stage_names <- c(
  "2018 Stage 1", 
  "2018 Stage 2", 
  "2018 Stage 3", 
  "2018 Stage 4", 
  "2018 Playoffs", 
  "2019 Stage 1", 
  "2019 Stage 2", 
  "2019 Stage 3", 
  "2019 Stage 4", 
  "2019 Playoffs", 
  "2020 Regular Season", 
  "2020 Playoffs", 
  "2021 May Melee", 
  "2021 June Joust", 
  "2021 Summer Showdown", 
  "2021 Countdown Cup", 
  "2021 Playoffs"
)

#### UI Input/Output ####
## Defaults
default_match <- c("Select Team")
default_stat_1 <- c("Select Match", "Select Stat", "All Heroes", "Hero Specifc")
default_stat_2 <- c("None", "All Heroes", "Hero Specifc")
years <- c(2018, 2019, 2020, 2021)
max_match_per_team <- 10
all_team_text <- "All Teams"
display_stat <- c("Per 10" = "per_10", "Sum" = "sum", "Mean" = "mean")

## Inputs
ui_year_select <- checkboxGroupInput("year_select", "Years:", years, selected = 2021, inline = T)
ui_team_select <- selectInput("team_select", "Team filter:", c(all_team_text, team_colours$team))
ui_match_select <- selectInput("match_select", "2021 Match (can select multiple):", default_match, 
                               multiple = T, size = 8, selectize = F)
ui_match_reset <- actionButton("match_reset_button", "clear", icon = icon("redo"))
ui_match_all <- actionButton("match_all_button", "all", icon = icon("tasks"))
ui_match_display <- radioButtons("match_display", "Display as: (TBD)", 
                                 c("All Maps" = "all_maps", "By Map" = "by_map"), 
                                 inline = TRUE)

ui_collapse_heroes <- actionButton("button_collapse_heroes", label = " Filter Heroes", icon = icon("toggle-off"), 
                                   `data-toggle` = "collapse", `data-target` = "#collapsable_heroes",
                                   width = "100%")
ui_tank_button <- actionButton("tank_button", "T", icon = icon("shield"))
ui_damage_button <- actionButton("damage_button", "D", icon = icon("rocket"))
ui_support_button <- actionButton("support_button", "S", icon = icon("band-aid"))
ui_hero_reset <- actionButton("hero_reset_button", "All", icon = icon("redo"))

ui_tank_select <- checkboxGroupInput("tank_select", "", inline = TRUE, width = "100%")
ui_damage_select <- checkboxGroupInput("damage_select", "", inline = TRUE)
ui_support_select <- checkboxGroupInput("support_select", "", inline = TRUE)

ui_stat_1_select <- selectInput("stat_1", "Stat 1", choices = default_stat_1[1])
ui_stat_1_display <- radioButtons("stat_1_display", "Display as:", 
                                  display_stat, 
                                  inline = TRUE)
  
ui_stat_2_select <- selectInput("stat_2", "Stat 2 (Optional)", choices = default_stat_2[1])
ui_stat_2_display <- radioButtons("stat_2_display", "Display as:", 
                                  display_stat, 
                                  inline = TRUE)
ui_stat_2_clear <- actionButton("stat_2_clear", "Clear Stat 2", icon = icon("redo"))

ui_top_bot <- radioButtons("top_bot", "Show:", 
                           choices = c("Top" = "top", "Bottom" = "bottom"), 
                           inline = TRUE, selected = "top")
ui_filter_n <- sliderInput("filter_n", "",
                           min = 2, max = 30, value = 12)

ui_min_time <- sliderInput("min_time", "Minimum Time Played (min) (WIP)", 
                           min = 0, max = 90, value = 45)

## Outputs
ui_season_summary <- plotOutput("season_summary",
                                click = "clicked_match", dblclick = "summary_clicked_opponent", 
                                height = "414px")
ui_opponent_freq <- plotOutput("opponent_freq", 
                               click = "clicked_matchup", dblclick = "freq_clicked_opponent", )

ui_player_stats <- plotOutput("player_stats", height = "600px")

#### Shiny UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("OWL Stats Garage (Beta)"), 
  
  sidebarLayout(
    sidebarPanel(
      ui_year_select, 
      ui_team_select, 
      br(), 
      ui_match_select, 
      fluidRow(
        style = "text-align:center;", 
        ui_match_all,
        ui_match_reset
      ),
      # ui_match_display, 
      tags$hr(), 
      # tags$text("Filter Heroes:"),
      fluidRow(
        style = "text-align:center;", 
        ui_tank_button, 
        ui_damage_button, 
        ui_support_button, 
        ui_hero_reset
      ),      fluidRow(ui_collapse_heroes), 
      tags$div(
        id = "collapsable_heroes", class = "collapse",
        ui_tank_select, 
        ui_damage_select, 
        ui_support_select
      ), 
 
      tags$hr(), 
      ui_stat_1_select, 
      ui_stat_1_display,
      ui_top_bot,
      ui_filter_n,
      ui_stat_2_select,
      ui_stat_2_display, 
      ui_stat_2_clear, 
      ui_min_time
    ), 
    mainPanel(
      fluidRow(
        column(8, 
          tags$text("Seaons Summary (click to select match, double click to switch team)"),      
          ui_season_summary
        ), 
        column(4, 
          tags$text("Opponent Frequency (click for season matchup)"), 
          ui_opponent_freq
        )
      ), 
      tags$br(), 
      fluidRow(
        column(12, 
          tags$text("Player Statistics"),
          ui_player_stats
        )
      )
    )
  )
)

#### Shiny Server ####
server <- function(input, output, session) {
  #### Memory and Session ####
  rV <- reactiveValues(map_list = NULL, 
                       team_matches = NULL, 
                       match_list = NULL, 
                       filtered_matches = NULL, 
                       opponent_summary = NULL, 
                       
                       last_stat_1 = NULL,
                       last_stat_2 = NULL,
                       
                       filter_heroes = NULL, 
                       last_filter_heroes = NULL, 
                       heroes_played = NULL)
  
  summary_width <- reactive({session$clientData[["output_season_summary_width"]]})
  summary_width_b <- summary_width %>% debounce(500)
  freq_width <- reactive({session$clientData[["output_opponent_freq_width"]]})
  freq_width_b <- freq_width %>% debounce(500)
  stats_width <- reactive({session$clientData[["output_player_stats_width"]]})
  stats_width_b <- stats_width %>% debounce(500)
  
  #### Input interactivity ####
  observeEvent(c(rV$team_matches, rV$map_list),{
    if (dim(rV$team_matches)[1] == 0) {
      all_maps <- rV$map_list %>%
        mutate(date = ifelse(game_number == 1 & map_round == 1, round_start_time, NA)) %>%
        fill(date) %>% mutate(date = as_datetime(date)) %>%
        group_by(match_id) %>%
        summarise(date_played = unique(date),
                  teams = unique(paste(word(team_one_name, -1), "v", word(team_two_name, -1))),
                  .groups = "drop") %>%
        mutate(date_played = as_date(date_played)) %>%
        mutate(stage = cut(date_played, stage_dates, labels = stage_names)) %>%
        droplevels() %>%
        arrange(date_played) %>%
        distinct(match_id, .keep_all = T)
        
      rV$match_list <- all_maps %>%
        mutate(match_name = paste(
          sprintf("%4d %s %02d", 
                  year(date_played), 
                  month(date_played, T), 
                  mday(date_played)), "|",
          teams)
        ) %>%
        select(match_name, match_id) %>% 
        deframe() %>%
        split(all_maps$stage)
        
    } else {
      rV$match_list <- rV$team_matches %>%
        mutate(match_name = paste(#year(date_played), 
          #month(date_played, F), 
          # sprintf("%2d", mday(date_played)), 
          sprintf("%4d %s %02d", 
                  year(date_played), 
                  month(date_played, T), 
                  mday(date_played)), "|",
          opponent)) %>%
        select(match_name, match_id) %>%
        deframe() %>%
        split(rV$team_matches$stage)
    }
    
    updateSelectInput(session, "match_select", choices = rV$match_list)
  })
  
  observeEvent(input$match_reset_button, {
    updateSelectInput(session, "match_select", selected = character(0))
  })
  
  observeEvent(input$match_all_button, {
    if (!is.null(rV$match_list)) {
      updateSelectInput(session, "match_select", selected = unlist(rV$match_list))
    }
  })
  
  #### Top Plots ####
  observeEvent(input$year_select, {
    rV$map_list <- map %>%
      filter(year(round_start_time) %in% input$year_select)
    
  })
  
  ## Update team-specific match lists (empty for All Teams)
  observeEvent(c(input$team_select, rV$map_list), {
    if (input$team_select == all_team_text) {
      rV$team_matches <- data.frame()
      rV$opponent_summary <- data.frame()
    } else {
      rV$team_matches <- rV$map_list %>%
        # filter(stringr::str_detect(paste(team_one_name, team_two_name), input$team_select)) %>% 
        filter_at(vars(starts_with("team_")), any_vars(str_detect(.,input$team_select))) %>%
        mutate(date = ifelse(game_number == 1 & map_round == 1, round_start_time, NA)) %>%
        fill(date) %>% mutate(date = as_datetime(date)) %>% 
        group_by(match_id) %>%
        summarise(date_played = unique(date), 
                  maps_played = max(game_number), 
                  opponent = setdiff(unique(c(team_one_name, team_two_name)), input$team_select), 
                  maps_won = sum(map_winner == input$team_select & map_round == 1), 
                  maps_lost = sum(map_winner == opponent & map_round == 1),
                  .groups = "drop") %>% 
        mutate(date_played = as_date(date_played)) %>%
        mutate(stage = cut(date_played, stage_dates, labels = stage_names)) %>%
        droplevels() %>%
        arrange(date_played)
      
      rV$opponent_summary <- rV$team_matches %>%
        group_by(opponent) %>%
        summarize(faced = n(), wins = sum(maps_won>abs(maps_lost)), .groups = "drop") %>%
        arrange(faced, desc(opponent))
    }
  })
  
  output$season_summary <- renderPlot({
    aspr <- (summary_width_b()-0)/414
    if (dim(rV$team_matches)[1] == 0) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Select a single team for match history"), 
           cex = 1.6, col = "black")
    } else {
      rV$team_matches %>% 
        # View()
        ## Plotting
        mutate(match_num = 1:n(),
               map_diff = maps_won - maps_lost) %>%
        left_join(team_colours, by = c("opponent" = "team")) %>%
        mutate(won = ifelse(maps_won > maps_lost, TRUE, FALSE)) %>%
        mutate(maps_lost = -1*maps_lost) %>%
        ggplot(aes(x = match_num)) +
        geom_hline(yintercept = c(-4:4), color = "gray") +
        geom_bar(aes(fill = ifelse(map_diff > 0, "won", "lost"), y = map_diff-0.25*sign(map_diff)), stat="identity", position = "identity") +
        geom_image(aes(image = logo,
                       y = map_diff), asp = aspr) +
        scale_fill_manual(name = "Matches", values = c("won" = owl_colours[[1]],"lost" = owl_colours[[4]]),
                          labels = c("won" = "won","lost" = "lost")) +
        labs(x = "Appearance in OWL", y = "match score") +
        scale_y_continuous(limits = c(-4, 4), n.breaks = 9,
                           labels = c("0-4", "0-3", "1-3", "2-3", "", "3-2", "3-1", "3-0", "4-0")) + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"),
              legend.position = "bottom",
              axis.title.y = element_blank(), axis.ticks.y = element_blank(),
              axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
  })
  
  output$opponent_freq <- renderPlot({
    aspr <- (freq_width_b()-0)/400
    
    # rV$team_matches %>%
    #   group_by(opponent) %>%
    #   summarize(faced = n(), wins = sum(maps_won>abs(maps_lost)), .groups = "drop") %>%
    #   arrange(faced, desc(opponent)) %>%
    if (dim(rV$opponent_summary)[1] == 0) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Select a team"), 
           cex = 1.6, col = "black")
    } else {
      rV$opponent_summary %>%
        left_join(select(team_colours,c(1,8)),by = c("opponent" = "team")) %>%
        mutate(opponent = reorder(opponent, desc(opponent)), opponent = reorder(opponent, faced)) %>%
        pivot_longer(cols = c("faced", "wins")) %>%
        
        ggplot(aes(y=opponent)) +
        geom_bar(aes(x = value, fill = name), stat = "identity", position = "nudge") +
        geom_text(aes(x = value+0.1, colour = name, label = value), fontface = "bold") +
        geom_image(aes(image = logo), x = -0.3, size = 0.08, asp = aspr) +
        
        # Format
        scale_x_continuous(limits = c(-0.35,NaN)) +
        scale_fill_manual(name = "", values = c("faced" = owl_colours[[4]], "wins" = owl_colours[[1]]),
                          labels = c("Times played", "Matches won")) +
        scale_color_manual(name = "", values = c("faced" = owl_colours[[4]], "wins" = owl_colours[[1]]), guide = "none") +
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "#F3F8FE", color = "#f9c455"),
              axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
              legend.position = "bottom")
      
    }
  }, bg="transparent")
  
  #### Player Stats ####
  observeEvent(c(input$match_select), {
    rV$filtered_matches <- player %>%
      filter(esports_match_id %in% input$match_select) 
  })
    
  observeEvent(rV$filtered_matches, {
    rV$heroes_played <- rV$filtered_matches %>%
      distinct(hero_name) %>%
      arrange(hero_name) %>%
      deframe()
    
    updateCheckboxGroupInput(session, "tank_select", 
                             choices = intersect(rV$heroes_played, hero_roles$tank), 
                             selected = intersect(rV$last_filter_heroes, intersect(rV$heroes_played, hero_roles$tank)))
    updateCheckboxGroupInput(session, "damage_select", 
                             choices = intersect(rV$heroes_played, hero_roles$damage), 
                             selected = intersect(rV$last_filter_heroes, intersect(rV$heroes_played, hero_roles$damage)))
    updateCheckboxGroupInput(session, "support_select", 
                             choices = intersect(rV$heroes_played, hero_roles$support), 
                             selected = intersect(rV$last_filter_heroes, intersect(rV$heroes_played, hero_roles$support)))
  })
  
  observeEvent(c(input$tank_select, input$damage_select, input$support_select), {
    rV$filter_heroes <- c(input$tank_select, input$damage_select, input$support_select)
  })
  
  observeEvent(c(rV$filter_heroes, rV$filtered_matches), {
    # filter_heroes <- c(input$tank_select, input$damage_select, input$support_select)
    common_stat_names <- rV$filtered_matches %>%
      filter(hero_name == "All Heroes") %>% 
      distinct(stat_name) %>%
      # arrange(stat_name) %>%
      deframe() %>%
      as.character() %>% 
      sort()
    
    specific_stat_names <- rV$filtered_matches %>%
      {if(is.null(rV$filter_heroes)) . else filter(., hero_name %in% rV$filter_heroes)} %>%
      distinct(stat_name) %>%
      arrange(stat_name) %>%
      deframe() %>%
      as.character() %>% 
      sort()
      
    
    choices <- vector(mode = "list", length = 3)
    names(choices) <- c(default_stat_1[3], default_stat_1[4], "Default")
    choices$Default <- list(default_stat_1[2])
    choices[[default_stat_1[3]]] <- intersect(common_stat_names, specific_stat_names)
    choices[[default_stat_1[4]]] <- setdiff(specific_stat_names, common_stat_names)
    #   c(
    #   default_stat_1[3], 
    #   intersect(common_stat_names, specific_stat_names), 
    #   default_stat_1[4], 
    #   setdiff(specific_stat_names, common_stat_names)
    # )
    
    updateSelectInput(session, "stat_1", choices = choices)
    if(!is.null(isolate(rV$last_stat_1))) {
      if(rV$last_stat_1 %in% unlist(choices)) {
        updateSelectInput(session, "stat_1", selected = rV$last_stat_1)
      }
    }
    
    choices$Default <- list(default_stat_2[1])
    choices[[default_stat_1[3]]] <- setdiff(choices[[default_stat_1[3]]], "Time Played")
    choices <- choices[c("Default", default_stat_1[3], default_stat_1[4])]
    # choices <- setdiff(choices, "Time Played")
    updateSelectInput(session, "stat_2", choices = choices,
                      selected = default_stat_2[1])
    if(!is.null(isolate(rV$last_stat_2))) {
      if(rV$last_stat_2 %in% unlist(choices)) {
        updateSelectInput(session, "stat_2", selected = rV$last_stat_2)
      }
    }
  })
  
  observeEvent(input$stat_2_clear, {
    updateSelectInput(session, "stat_2", selected = default_stat_2[1])
  })
  
  observeEvent(input$stat_1, {
    if(!(input$stat_1 %in% default_stat_1)) {
      rV$last_stat_1 <- input$stat_1
    }
    # print(rV$last_stat_1)
  })
  
  observeEvent(input$stat_2, {
    if(!(input$stat_2 %in% default_stat_2[-1])) {
      rV$last_stat_2 <- input$stat_2
    }
    # print(rV$last_stat_1)
  })
  
  observeEvent(rV$filter_heroes, {
    if(!is.null(rV$filter_heroes)) {
      rV$last_filter_heroes <- rV$filter_heroes
    }
  })
  #### Player Plot ####
  ## Debounce: 
  #input$stat_1
  #input$stat_2
  #rV$filter_heroes
  
  heroes_selected_title <- function() {
    fh <- rV$filter_heroes
    title <- case_when(
      is.null(fh) ~ "All Heroes", 
      identical(fh, intersect(rV$heroes_played, hero_roles$tank)) ~ "All Tanks", 
      identical(fh, intersect(rV$heroes_played, hero_roles$damage)) ~ "All DPS", 
      identical(fh, intersect(rV$heroes_played, hero_roles$support)) ~ "All Supports", 
      length(fh) < 6 ~ paste(fh, collapse = ", "), 
      TRUE ~ paste(length(fh), "Selected Heroes")
    )
    return(title)
  }
  
  output$player_stats <- renderPlot({
    if(input$stat_1 %in% default_stat_1) {
      # Check if 1 or 2 stats are selected
      return(data.frame())
    } else if (input$stat_2 %in% default_stat_2) {
      filter_stats <- c("Time Played", input$stat_1)
      # print(filter_stats)
      # filter_heroes <- c(input$tank_select, input$damage_select, input$support_select)
      # print(filter_heroes)
    } else {
      filter_stats <- c("Time Played", input$stat_1, input$stat_2)
    }
      
    filtered_stats <- player %>% 
      filter(esports_match_id %in% input$match_select) %>%
      filter(stat_name %in% filter_stats) %>%
      filter(hero_name != "All Heroes") %>%
      {if(!is.null(rV$filter_heroes)) filter(., hero_name %in% rV$filter_heroes) else .} 
    
    aspr <- (stats_width_b()-0)/600
    display_stat_1 <- ifelse(input$stat_1_display == "sum" | input$stat_1 == "Time Played", 
                             "stat_1", 
                             paste0(input$stat_1_display, "_1"))
                             # "per_10_1")
    display_stat_2 <- ifelse(input$stat_2_display == "sum" | input$stat_2 == "Time Played", 
                             "stat_2", 
                             paste0(input$stat_2_display, "_2"))
                             # "per_10_2")
    
    sort_top_bot <- ifelse(input$top_bot == "top", T, F)
    op_top_bot <- ifelse(input$top_bot == "top", desc, function(x) x)
    # op_top_bot <- if(sort_top_bot) `>=` else `<=`
    # No. of Applicable players: 
    
    filter_n <- 0
    if (dim(filtered_stats)[1] != 0) {  
      filter_n <- filtered_stats %>% 
        group_by(player_name, stat_name) %>% summarize(n = n(), .groups = "drop") %>% 
        group_by(stat_name) %>% summarise(sum = n(), .groups = "drop") %>% 
        .$sum %>% min()    
    }

    # Choose between minimum of input selection and applicable players
    filter_n <- min(input$filter_n, filter_n)
    
    if (any(!(filter_stats[-1] %in% filtered_stats$stat_name))) { # same as: all(input %in% filtered)
      ## Make sure selected stats are applicable to filtered heroes
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("This stat not avialable"), 
           cex = 1.6, col = "black")
    } else if (input$stat_2 %in% default_stat_2) {
      
      
      ## Only stat_1 selected ##
      plot_data <- filtered_stats %>%
        # Only look at selected team if many matches are selected:
        {if(length(input$match_select) > max_match_per_team & dim(rV$team_matches)[1] != 0) filter(., team_name == input$team_select) else .} %>%
        pivot_wider(id_cols = c(team_name, player_name, hero_name), names_from = stat_name, values_from = stat_amount, values_fn = sum) %>% # all maps
        # pivot_wider(id_cols = c(map_name, team_name, player_name, hero_name), names_from = stat_name, values_from = stat_amount, values_fn = sum) %>%
        filter(`Time Played` >= input$min_time*60) %>%
        rename(stat_1 = as.name(filter_stats[2])) %>%
        filter(!is.na(stat_1)) %>%
        group_by(player_name) %>%
        {if (input$stat_1 != "Time Played") mutate(
          ., 
          per_10_1 = replace_na(stat_1/(sum(`Time Played`)/600), 0), 
          mean_1 = mean(stat_1, na.rm = T)
        ) else .} %>%
        mutate(plot_stat_1 = !!sym(display_stat_1)) %>%
        mutate(total_plot_stat_1 = sum(plot_stat_1), 
               total_time_played = sum(`Time Played`)) %>% 
        ungroup() %>%
        ## Filter top or bottom N
        mutate(n = dense_rank(op_top_bot(total_plot_stat_1))) %>% filter(n %in% 1:filter_n) %>%

        ## Rearrange data frame by hero name and stat for plot 
        arrange(hero_name) %>%
        # arrange(desc(total_plot_stat_1)) %>%
        mutate(player_name = reorder(player_name, total_plot_stat_1)) 
      
      # print(plot_data$per_10)
      if (dim(plot_data)[1] == 0) {
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("Error. Try reducing Min time played"), 
             cex = 1.6, col = "black")
        return()
      }
      
      # print("--------")
      # print(plot_data %>% filter(player_name == "Creative"))
      max_stat_1 <- max(plot_data$total_plot_stat_1)
      
      plot_data %>%
        group_by(player_name) %>%
        mutate(player_row = 1:n()) %>%
        ungroup() %>%
        left_join(select(team_colours, team, logo), by = c("team_name" = "team")) %>%
        left_join(select(heroes, hero_name, short), by = "hero_name")%>%
        ## Plotting
        ggplot(aes(y = player_name)) +
        geom_bar(aes(x = plot_stat_1, fill = hero_name), 
                 stat = "identity", position = position_stack(reverse = T)) +
        geom_image(aes(image = ifelse(player_row == 1, logo, NA)), 
                   x = -0.1*max_stat_1, asp = aspr, na.rm = T) + 
        # geom_label(aes(label = player_name, fill = team_name), x = -0.1*max(plot_data$total_stat_1)) + 
        geom_text(aes(x = 1.05*total_plot_stat_1,
                      label = ifelse(
                        player_row == 1, 
                        paste0(signif(total_plot_stat_1, digits = 4), 
                               "//", 
                               round(total_time_played/60), 
                               " min"
                               ),
                        # , 
                        NA
                      )), 
                  hjust = "left", na.rm = T) +
        geom_text(aes(x = plot_stat_1, 
                      label = ifelse(plot_stat_1 > 0.15*max_stat_1, short, NA)), 
                  position = position_stack(vjust = 0.5), 
                  fontface = "bold", color = "white",
                  na.rm = T) + 
        ggtitle(sprintf("%s (%s %d) on %s (%d+ min) from %d matches", 
                        filter_stats[2], 
                        input$top_bot, filter_n, 
                        heroes_selected_title(),
                        input$min_time,
                        length(unlist(input$match_select)))) + 
        xlim(-0.15*max_stat_1,1.25*max_stat_1) + 
        xlab(paste0(input$stat_1,
                    " (", names(display_stat[display_stat == input$stat_1_display]), ")")) + 
        scale_fill_manual(values = hero_colours) + 
        theme(axis.title.y = element_blank(), 
              legend.position = "bottom", legend.title = element_blank()) + 
        guides(fill = guide_legend(ncol = 8))
      
    } else if (input$stat_2 != input$stat_1) {
      
      
      ## both stat_1 and stat_2 are selected and valid ##
      plot_data <- filtered_stats %>%
        {if(length(input$match_select) > max_match_per_team & dim(rV$team_matches)[1] != 0) filter(., team_name == input$team_select) else .} %>%
        pivot_wider(id_cols = c(team_name, player_name, hero_name), names_from = stat_name, values_from = stat_amount, values_fn = sum) %>% # all maps
        # pivot_wider(id_cols = c(map_name, team_name, player_name, hero_name), names_from = stat_name, values_from = stat_amount, values_fn = sum) %>%
        filter(`Time Played` >= input$min_time*60) %>%
        ungroup() %>%
        rename(stat_1 = as.name(filter_stats[2]), 
               stat_2 = as.name(filter_stats[3])) %>%
        filter(!is.na(stat_1), !is.na(stat_2)) %>%
        group_by(player_name) %>%
        {if (input$stat_1 == "Time Played") {
          mutate(
            ., 
            `Time Played` = stat_1, 
            per_10_2 = replace_na(stat_2/(sum(`Time Played`)/600), 0), 
            mean_2 = mean(stat_2, na.rm = T)
          )
        } else {
          mutate(
            .,
            per_10_1 = replace_na(stat_1/(sum(`Time Played`)/600), 0), 
            mean_1 = mean(stat_1, na.rm = T),
            per_10_2 = replace_na(stat_2/(sum(`Time Played`)/600), 0), 
            mean_2 = mean(stat_2, na.rm = T)
          )
        }} %>%
        mutate(plot_stat_1 = !!sym(display_stat_1), 
               plot_stat_2 = !!sym(display_stat_2)) %>%
        mutate(total_plot_stat_1 = sum(plot_stat_1), 
               total_plot_stat_2 = sum(plot_stat_2)) %>% 
        ungroup() %>% 
        
        ## Filter top or bottom N
        mutate(n = dense_rank(op_top_bot(total_plot_stat_1))) %>% filter(n %in% 1:filter_n) %>%

        ## Rearrange data frame by hero name and stat for plot 
        # arrange(desc(total_plot_stat_1)) %>%
        arrange(hero_name) %>%
        mutate(player_name = reorder(player_name, total_plot_stat_1)) 
      
      if (dim(plot_data)[1] == 0) {
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("Error. Try reducing Min time played"), 
             cex = 1.6, col = "black")
        return()
      }
      
      # print("-------")
      # print(plot_data %>% filter(player_name == "Creative"))
      max_stat_1 <- max(plot_data$total_plot_stat_1)
      max_stat_2 <- max(plot_data$total_plot_stat_2)
      
      plot_data %>%
        group_by(player_name) %>%
        mutate(player_row = 1:n()) %>%
        ungroup() %>%
        left_join(select(team_colours, team, logo), by = c("team_name" = "team")) %>%
        left_join(select(heroes, hero_name, short), by = "hero_name") %>%
        distinct(player_name, .keep_all = TRUE) %>% 
        ## Plotting
        ggplot(aes(x = total_plot_stat_1, y = total_plot_stat_2)) +
        geom_smooth(method = "lm", formula = y ~ x) + 
        geom_image(aes(image = ifelse(player_row == 1, logo, NA)), 
                   asp = aspr, na.rm = T) + 
        geom_label(aes(label = player_name, fill = team_name, color = team_name), 
                  fontface = "bold", size = 5, na.rm = T) + 
        ggtitle(sprintf("%s (%s %d) vs %s on %s (%d+ min) from %d matches", 
                        filter_stats[2], input$top_bot, filter_n, 
                        filter_stats[3], 
                        heroes_selected_title(),
                        input$min_time,
                        length(unlist(input$match_select)))) + 
        xlab(paste0(input$stat_1,
                    " (", names(display_stat[display_stat == input$stat_1_display]), ")")) + 
        ylab(paste0(input$stat_2,
                    " (", names(display_stat[display_stat == input$stat_2_display]), ")")) + 
        scale_fill_manual(values = deframe(team_colours[c(1,4)])) + 
        scale_color_manual(values = deframe(team_colours[c(1,5)])) +
        theme(legend.position = "none") + 
        guides(fill = guide_legend(ncol = 8))
                              
    } else {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Invalid selection"), 
           cex = 1.6, col = "black")
    }
  })
  
  #### Heroes ####
  observeEvent(input$tank_button, {
    updateCheckboxGroupInput(session, "tank_select",
                             selected = intersect(rV$heroes_played, hero_roles$tank))
  })
  
  observeEvent(input$damage_button, {
    updateCheckboxGroupInput(session, "damage_select",
                             selected = intersect(rV$heroes_played, hero_roles$damage))
  })

  observeEvent(input$support_button, {
    updateCheckboxGroupInput(session, "support_select",
                             selected = intersect(rV$heroes_played, hero_roles$support))
  })
  
  observeEvent(input$hero_reset_button, {
    clearHeroes()
  })

  clearHeroes <- function(){
    updateCheckboxGroupInput(session, "tank_select", 
                             selected = character(0))
    updateCheckboxGroupInput(session, "damage_select", 
                             selected = character(0))
    updateCheckboxGroupInput(session, "support_select", 
                             selected = character(0))
    rV$filter_heroes <- NULL
  }
  
  #### Click Events ####
  observeEvent(input$clicked_match, {
    clicked_match_id <- rV$team_matches[round(input$clicked_match$x),]$match_id
    updateSelectInput(session, "match_select", selected = clicked_match_id)
  })
  
  observeEvent(input$summary_clicked_opponent, {
    clicked_team <- rV$team_matches[round(input$summary_clicked_opponent$x),]$opponent
    updateSelectInput(session, "team_select", selected = clicked_team)
  })
  
  observeEvent(input$clicked_matchup, {
    clicked_opponent <- rV$opponent_summary[round(input$clicked_matchup$y),]$opponent

    faced_match_ids <- rV$team_matches %>% 
      filter(opponent == clicked_opponent) %>%
      distinct(match_id) %>%
      deframe()

    updateSelectInput(session, "match_select", selected = faced_match_ids)
  })
  
  observeEvent(input$freq_clicked_opponent, {
    clicked_team <- rV$opponent_summary[round(input$freq_clicked_opponent$y),]$opponent
    updateSelectInput(session, "team_select", selected = clicked_team)
  })
  
  observeEvent(input$button_collapse_heroes, {
    ## Starts on 0 and already collapsed
    if (input$button_collapse_heroes%%2) {
      updateActionButton(session, "button_collapse_heroes", icon = icon("toggle-on"))
    } else {
      updateActionButton(session, "button_collapse_heroes", icon = icon("toggle-off"))
    }
  })
  
}

#### Shiny App ####
shinyApp(ui = ui, server = server)
