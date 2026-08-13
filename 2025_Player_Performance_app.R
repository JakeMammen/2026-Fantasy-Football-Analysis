# app.R
# Fantasy Sports Pack Player Performance Tool – production-ready for Posit Connect

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggtext)
library(bslib)
library(thematic)
library(gt)
library(htmltools)
# library(ggpath)   # uncomment if ggpath is required for element_path

# ------------------------------------------------------------------
# 1. LOAD PRE-SAVED DATA (no network calls)
# ------------------------------------------------------------------
all_stats        <- readRDS("data/all_stats.rds")
raw_schedules    <- readRDS("data/raw_schedules.rds")
team_colors_db   <- readRDS("data/team_colors_db.rds")
rb_final_stats   <- readRDS("data/rb_final_stats.rds")
wr_final_stats   <- readRDS("data/wr_final_stats.rds")
te_final_stats   <- readRDS("data/te_final_stats.rds")
qb_final_stats   <- readRDS("data/qb_final_stats.rds")

# Derive the stricter filter used by the interactive plot
raw_stats <- all_stats |>
  group_by(player_id, player_display_name, team) |>
  mutate(active_games = n()) |>
  ungroup() |>
  filter(active_games >= 7)

player_choices <- raw_stats |>
  distinct(player_display_name) |>
  pull(player_display_name) |>
  sort()

# Team colour helper (used by both plot and tables)
team_info <- team_colors_db |>
  select(team_abbr, team_color) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

# ------------------------------------------------------------------
# Helper: build styled gt ranking table
# ------------------------------------------------------------------
build_rankings_gt <- function(final_df, pos) {
  pct_cols <- paste0(tolower(pos), c("1_pct", "2_pct", "3_pct"))
  labels <- c(
    rank = "Rank",
    player_display_name = "Player",
    team = "Team",
    ppr_per_game = "PPR/Game",
    setNames(paste0(pos, c("1 %", "2 %", "3+ %")), pct_cols)
  )
  
  final_df |>
    left_join(team_info, by = c("team" = "team_abbr")) |>
    mutate(
      team = paste0(
        "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
      )
    ) |>
    select(rank, player_display_name, team, ppr_per_game, all_of(pct_cols)) |>
    gt() |>
    cols_label(.list = labels) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) |>
    fmt_number(
      columns = c(ppr_per_game, all_of(pct_cols)),
      decimals = 1
    ) |>
    fmt_markdown(columns = team) |>
    tab_style(
      style = cell_fill(color = "#31a354"),
      locations = cells_body(columns = all_of(pct_cols[1]), rows = .data[[pct_cols[1]]] > 0)
    ) |>
    tab_style(
      style = cell_fill(color = "#636363"),
      locations = cells_body(columns = all_of(pct_cols[2]), rows = .data[[pct_cols[2]]] > 0)
    ) |>
    tab_style(
      style = cell_fill(color = "#e41a1c"),
      locations = cells_body(columns = all_of(pct_cols[3]), rows = .data[[pct_cols[3]]] > 0)
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_header(
      title = md("**2025 Fantasy Football PPR Points Per Game Rankings**"),
      subtitle = paste0(
        "Top 24 ",
        switch(pos,
               "WR" = "Receivers",
               "TE" = "Tight Ends",
               "QB" = "Quarterbacks",
               "Running Backs"),
        " (Min. 4 games played)"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_title(groups = "subtitle")
    ) |>
    opt_table_font(font = "Arial") |>
    tab_options(
      table.font.color = "black",
      table_body.border.top.color = "black",
      row_group.border.top.color = "#999999",
      table_body.border.bottom.color = "#999999",
      row_group.border.bottom.color = "black",
      table.border.top.color = "transparent",
      table.background.color = "#F2F2F2",
      table.border.bottom.color = "transparent",
      source_notes.background.color = "#F2F2F2",
      row.striping.background_color = "#FFFFFF",
      row.striping.include_table_body = TRUE
    ) |>
    tab_source_note(
      source_note = html(
        paste0(
          "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
           <div style='font-size: 12px;'>
             <b>Data:</b> nflfastR | <b>Created by:</b> @FantasySPack & @jakemammen
           </div>",
          local_image(filename = "logos/Graph_logo2.png", height = 30),
          "</div>"
        )
      )
    )
}

# ------------------------------------------------------------------
# 2. USER INTERFACE
# ------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Oswald"),
    heading_font = font_google("Silkscreen")
  ),
  
  tags$head(
    tags$style(HTML("
      .title-header-banner {
        background: linear-gradient(135deg, #112233 0%, #1f3a60 100%);
        color: #ffffff !important;
        padding: 24px;
        margin: -20px -20px 24px -20px;
        border-bottom: 4px solid #31a354;
      }
      .title-header-banner h2 {
        margin: 0;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      .sidebar-panel-styled {
        background-color: #ffffff;
        color: #212529;
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        padding: 24px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03);
      }
      [data-bs-theme='dark'] .sidebar-panel-styled {
        background-color: #1e293b !important;
        color: #f8fafc !important;
        border-color: #334155;
      }
    "))
  ),
  
  div(class = "title-header-banner",
      tags$h2("Fantasy Sports Pack Player Performance Tool (2025 Regular Season)")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel-styled",
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          p("Theme Toggle:", style = "margin: 0; font-weight: bold;"),
          input_dark_mode()
      ),
      hr(),
      
      # Controls for Player Performance tab
      conditionalPanel(
        condition = "input.main_tabs == 'Player Performance'",
        p("Select a position and player name below to analyze their PPR performance thresholds against their specific position group."),
        hr(),
        selectInput(
          inputId = "position_filter",
          label = "Filter by Position:",
          choices = c("All", "QB", "RB", "WR", "TE"),
          selected = "All"
        ),
        br(),
        selectizeInput(
          inputId = "player_name",
          label = "Search Player Name:",
          choices = NULL,
          selected = "",
          options = list(placeholder = "Type to search a player...", maxOptions = 500)
        ),
        br(),
        downloadButton(outputId = "download_plot", label = "Download Plot as PNG", class = "btn-success"),
        hr(),
        p(HTML("<b>Legend note:</b> The threshold categories adjust automatically based on whether the selected player is a QB, RB, WR, or TE.")),
        br()
      ),
      
      # Controls for Position Rankings tab
      conditionalPanel(
        condition = "input.main_tabs == 'Position Rankings'",
        p("Select a position to view the top-24 PPR-per-game rankings with weekly tier percentages."),
        hr(),
        selectInput(
          inputId = "rank_position",
          label = "Select Position:",
          choices = c("QB", "RB", "WR", "TE"),
          selected = "RB"
        ),
        hr()
      ),
      
      p(HTML("<b>Data:</b> nflfastR"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Player Performance",
          plotOutput(outputId = "player_plot", height = "550px")
        ),
        tabPanel(
          "Position Rankings",
          gt_output("rankings_table")
        )
      )
    )
  )
)

# ------------------------------------------------------------------
# 3. SERVER LOGIC
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$position_filter, {
    filtered_stats <- raw_stats
    if (input$position_filter != "All") {
      filtered_stats <- filtered_stats |> filter(position == input$position_filter)
    }
    updated_choices <- filtered_stats |>
      distinct(player_display_name) |>
      pull(player_display_name) |>
      sort()
    current_selection <- input$player_name
    updateSelectizeInput(
      session,
      inputId = "player_name",
      choices = updated_choices,
      server = TRUE,
      selected = if (current_selection %in% updated_choices) current_selection else ""
    )
  })
  
  plot_generator <- reactive({
    req(input$player_name)
    
    player_info <- raw_stats |>
      filter(player_display_name == input$player_name) |>
      slice(1)
    
    if (nrow(player_info) == 0) return(NULL)
    
    current_position <- player_info$position
    current_team     <- player_info$team
    
    player_colors <- team_colors_db |>
      filter(team_abbr == current_team) |>
      slice(1)
    
    primary_color   <- if (nrow(player_colors) > 0) player_colors$team_color  else "#112233"
    secondary_color <- if (nrow(player_colors) > 0) player_colors$team_color2 else "#636363"
    
    weekly_thresholds <- raw_stats |>
      filter(position == current_position) |>
      group_by(week) |>
      arrange(desc(fantasy_points_ppr)) |>
      mutate(rank = row_number()) |>
      filter(rank %in% c(12, 24)) |>
      select(week, rank, fantasy_points_ppr) |>
      tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "PosRank") |>
      arrange(week)
    
    player_stats <- raw_stats |>
      filter(player_display_name == input$player_name) |>
      select(week, opponent_team, ppr = fantasy_points_ppr) |>
      arrange(week)
    
    player_ranks <- raw_stats |>
      filter(position == current_position) |>
      group_by(week) |>
      arrange(desc(fantasy_points_ppr)) |>
      mutate(rank = row_number()) |>
      filter(player_display_name == input$player_name) |>
      select(week, rank)
    
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
    
    n_active <- nrow(player_stats)
    avg_ppr  <- round(mean(player_stats$ppr, na.rm = TRUE), 1)
    
    counts <- player_stats |>
      count(category, .drop = FALSE) |>
      mutate(pct = round(n / n_active * 100))
    labs_fill <- paste(levels(player_stats$category), " (", counts$pct, "%)", sep = "")
    
    team_schedule <- raw_schedules |>
      filter(home_team == current_team | away_team == current_team)
    n_games     <- nrow(team_schedule)
    n_inactive  <- max(0, n_games - n_active)
    pct_inactive <- round(n_inactive / max(1, n_games) * 100)
    
    player_stats <- player_stats |>
      left_join(weekly_thresholds, by = "week")
    
    x_labels <- paste(player_stats$opponent_team, player_stats$week, sep = "\n")
    t12_name <- paste0("Weekly ", current_position, "12")
    t24_name <- paste0("Weekly ", current_position, "24")
    
    ggplot(player_stats, aes(x = factor(week), y = ppr)) +
      geom_col(aes(fill = category), width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", ppr)), vjust = -0.5, size = 3) +
      geom_line(aes(y = PosRank12, group = 1, linetype = t12_name), color = "gray50", linewidth = 0.8) +
      geom_line(aes(y = PosRank24, group = 1, linetype = t24_name), color = "red", linewidth = 0.8) +
      scale_fill_manual(
        values = c("#31a354", "#636363", "#e41a1c"),
        labels = labs_fill,
        name = paste0("Performance\nInactive: ", pct_inactive, "%")
      ) +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      scale_x_discrete(labels = x_labels) +
      scale_y_continuous(
        limits = c(0, max(c(player_stats$ppr, player_stats$PosRank12), na.rm = TRUE) * 1.2),
        breaks = seq(0, 60, 5)
      ) +
      labs(
        title = paste0(input$player_name, ": Week 1 Through Week 18, 2025"),
        subtitle = paste0(
          "<b>", n_active, " Active Games</b>: ", avg_ppr,
          " PPR/Game | <b>Position Group</b>: ", current_position,
          " | <b>Data:</b> nflfastR"
        ),
        y = "PPR Fantasy Points",
        x = "Opponent / Game Week",
        caption = "logos/Graph_logo2.png",
        tag = paste0(
          "<span style='color:", primary_color,
          "; font-weight:900; font-size:20px; font-family:Oswald;'>",
          current_team, "</span>"
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 9),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 12),
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.caption = ggpath::element_path(hjust = 1, size = 1.0),  # enable if ggpath is loaded
        plot.tag = ggtext::element_markdown(vjust = 1, hjust = 1),
        plot.tag.position = c(0.98, 0.98)
      )
  })
  
  output$player_plot <- renderPlot({
    p <- plot_generator()
    if (is.null(p)) {
      ggplot() + labs(title = "Please verify player search criteria.") + theme_void()
    } else {
      p
    }
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", tolower(input$player_name)), "_fantasy_performance_2025.png")
    },
    content = function(file) {
      ggsave(file, plot = plot_generator(), device = "png", width = 11, height = 7, dpi = 300)
    }
  )
  
  output$rankings_table <- render_gt({
    req(input$rank_position)
    pos <- input$rank_position
    final_df <- switch(pos,
                       "RB" = rb_final_stats,
                       "WR" = wr_final_stats,
                       "TE" = te_final_stats,
                       "QB" = qb_final_stats)
    build_rankings_gt(final_df, pos)
  })
}

# ------------------------------------------------------------------
# 4. LAUNCH
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)