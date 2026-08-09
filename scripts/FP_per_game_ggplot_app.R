library(shiny)
library(nflreadr)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- 1. PRE-LOAD AND PRE-PROCESS GLOBAL DATA ---

raw_stats <- load_player_stats(seasons = 2025) |> 
  filter(position %in% c("QB", "RB", "WR", "TE"), season_type == "REG")

# Load total team games to calculate inactive metrics (ATL/others played 17 games in 2024)
raw_schedules <- load_schedules(seasons = 2025) |> 
  filter(game_type == "REG")

# Unique player list for reactive searching/sorting alphabetized
player_choices <- raw_stats |> 
  distinct(player_display_name) |> 
  pull(player_display_name) |> 
  sort()


# --- 2. USER INTERFACE (UI) ---
ui <- fluidPage(
  titlePanel("NFL Fantasy Performance Tracker (2025 Regular Season)"),
  
  sidebarLayout(
    sidebarPanel(
      p("Select or type any offensive player name below to analyze their PPR performance thresholds against their specific position group."),
      hr(),
      
      # Selectize input allows typing and auto-completing search on thousands of players efficiently
      selectizeInput(
        inputId = "player_name",
        label = "Search Player Name:",
        choices = player_choices,
        selected = "Drake London",
        options = list(placeholder = 'Type to search a player...', maxOptions = 500)
      ),
      
      br(),
      # Add download button to save the plots locally
      downloadButton(outputId = "download_plot", label = "Download Plot as PNG", class = "btn-success"),
      hr(),
      p(HTML("<b>Legend note:</b> The threshold categories adjust automatically based on whether the selected player is a QB, RB, WR, or TE."))
    ),
    
    mainPanel(
      plotOutput(outputId = "player_plot", height = "550px")
    )
  )
)


# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive function to generate the plot based on user selection
  plot_generator <- reactive({
    req(input$player_name)
    
    # 1. Identify chosen player's position and team (TYPO FIXED HERE)
    player_info <- raw_stats |> 
      filter(player_display_name == input$player_name) |> 
      slice(1)
    
    # Validation error if name entered has no logs
    if (nrow(player_info) == 0) {
      return(NULL)
    }
    
    current_position <- player_info$position
    current_team <- player_info$team
    
    # Dynamic thresholds (Top 12 and Top 24) tailored specifically to the player's position
    weekly_thresholds <- raw_stats |> 
      filter(position == current_position) |> 
      group_by(week) |> 
      arrange(desc(fantasy_points_ppr)) |> 
      mutate(rank = row_number()) |> 
      filter(rank %in% c(12, 24)) |> 
      select(week, rank, fantasy_points_ppr) |> 
      tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "PosRank") |> 
      arrange(week)
    
    # Get chosen player's weekly stats
    player_stats <- raw_stats |> 
      filter(player_display_name == input$player_name) |> 
      select(week, opponent_team, ppr = fantasy_points_ppr) |> 
      arrange(week)
    
    # Compute chosen player's rank each week among their position group
    player_ranks <- raw_stats |> 
      filter(position == current_position) |> 
      group_by(week) |> 
      arrange(desc(fantasy_points_ppr)) |> 
      mutate(rank = row_number()) |> 
      filter(player_display_name == input$player_name) |> 
      select(week, rank)
    
    # Merge ranks and categorize performance dynamically using Tier labels
    tier1_label <- paste0(current_position, "1")
    tier2_label <- paste0(current_position, "2")
    tier3_label <- paste0(current_position, "3+")
    
    player_stats <- player_stats |> 
      left_join(player_ranks, by = "week") |> 
      mutate(category = case_when(
        rank <= 12 ~ tier1_label,
        rank <= 24 ~ tier2_label,
        TRUE ~ tier3_label
      )) |> 
      mutate(category = factor(category, levels = c(tier1_label, tier2_label, tier3_label)))
    
    # Metadata calculations for subtitle/legend
    n_active <- nrow(player_stats)
    avg_ppr <- round(mean(player_stats$ppr, na.rm = TRUE), 1)
    
    # Safely handle category counts if a tier isn't hit
    counts <- player_stats |> 
      count(category, .drop = FALSE) |> 
      mutate(pct = round(n / n_active * 100))
    
    labs_fill <- paste(levels(player_stats$category), " (", counts$pct, "%)", sep = "")
    
    # Calculate inactive rate based on team's true team context schedule
    team_schedule <- raw_schedules |> 
      filter((home_team == current_team | away_team == current_team))
    
    n_games <- nrow(team_schedule)
    n_inactive <- max(0, n_games - n_active)
    pct_inactive <- round(n_inactive / max(1, n_games) * 100)
    
    # Join thresholds back to stats
    player_stats <- player_stats |> left_join(weekly_thresholds, by = "week")
    
    # Create combined categorical X axis
    x_labels <- paste(player_stats$opponent_team, player_stats$week, sep = "\n")
    
    # Explicit threshold nomenclature for dynamic legends
    t12_name <- paste0("Weekly ", current_position, "12")
    t24_name <- paste0("Weekly ", current_position, "24")
    
    # Generate the ggplot object
    p <- ggplot(player_stats, aes(x = factor(week), y = ppr)) +
      geom_col(aes(fill = category), width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", ppr)), vjust = -0.5, size = 3) + 
      geom_line(aes(y = PosRank12, group = 1, linetype = t12_name), color = "gray50", linewidth = 0.8) + 
      geom_line(aes(y = PosRank24, group = 1, linetype = t24_name), color = "red", linewidth = 0.8) + 
      scale_fill_manual(
        values = c("#31a354", "#636363", "#e41a1c"), 
        labels = labs_fill, 
        name = paste0("Performance\nInactive: ", pct_inactive, "%")
      ) + 
      scale_linetype_manual(
        values = c("solid", "dashed"),
        name = NULL
      ) + 
      scale_x_discrete(labels = x_labels) + 
      scale_y_continuous(limits = c(0, max(c(player_stats$ppr, player_stats$PosRank12), na.rm = TRUE) * 1.2), breaks = seq(0, 60, 5)) + 
      labs(
        title = paste0(input$player_name, ": Week 1 Through Week 18, 2024"),
        subtitle = paste(n_active, "Active Games:", avg_ppr, "PPR/Game | Position Group:", current_position),
        y = "PPR Fantasy Points",
        x = "Opponent / Game Week",
        caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
      ) + 
      theme_minimal() + 
      theme(
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 9),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
        plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
      )
    
    return(p)
  })
  
  # Render the plot inside the app UI
  output$player_plot <- renderPlot({
    p <- plot_generator()
    if(is.null(p)) {
      ggplot() + labs(title = "Please verify player search criteria.") + theme_void()
    } else {
      p
    }
  })
  
  # Download handler script for exporting plots cleanly
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", tolower(input$player_name)), "_fantasy_performance_2024.png")
    },
    content = function(file) {
      ggsave(file, plot = plot_generator(), device = "png", width = 11, height = 7, dpi = 300)
    }
  )
}

# --- 4. LAUNCH THE APPLICATION ---
shinyApp(ui = ui, server = server)