# app.R
# Fantasy Sports Pack Player Performance Tool – with Team Filter + Two-Player Comparison
# Production-ready for Posit Connect

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggtext)
library(bslib)
library(thematic)
library(gt)
library(htmltools)
library(ggpath)  # uncomment if you use element_path for the logo

# ------------------------------------------------------------------
# 1. LOAD PRE-SAVED DATA
# ------------------------------------------------------------------
all_stats        <- readRDS("data/all_stats.rds")
raw_schedules    <- readRDS("data/raw_schedules.rds")
team_colors_db   <- readRDS("data/team_colors_db.rds")
rb_final_stats   <- readRDS("data/rb_final_stats.rds")
wr_final_stats   <- readRDS("data/wr_final_stats.rds")
te_final_stats   <- readRDS("data/te_final_stats.rds")
qb_final_stats   <- readRDS("data/qb_final_stats.rds")

# Stricter filter used by the interactive plot
raw_stats <- all_stats |>
  group_by(player_id, player_display_name, team) |>
  mutate(active_games = n()) |>
  ungroup() |>
  filter(active_games >= 7)

# Pre-compute choices
team_choices <- c("All", sort(unique(raw_stats$team)))

team_info <- team_colors_db |>
  select(team_abbr, team_color) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

# ------------------------------------------------------------------
# Helper: build styled gt ranking table (unchanged)
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
    fmt_number(columns = c(ppr_per_game, all_of(pct_cols)), decimals = 1) |>
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
        switch(pos, "WR" = "Receivers", "TE" = "Tight Ends", "QB" = "Quarterbacks", "Running Backs"),
        " (Min. 4 games played)"
      )
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_title(groups = "title")) |>
    tab_style(style = cell_text(style = "italic"), locations = cells_title(groups = "subtitle")) |>
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
      
      # ---------------- Player Performance controls ----------------
      conditionalPanel(
        condition = "input.main_tabs == 'Player Performance'",
        
        p("Select filters and player(s) to analyze weekly PPR performance against position thresholds."),
        hr(),
        
        # Team filter
        selectInput(
          inputId = "team_filter",
          label = "Filter by Team:",
          choices = team_choices,
          selected = "All"
        ),
        br(),
        
        # Position filter
        selectInput(
          inputId = "position_filter",
          label = "Filter by Position:",
          choices = c("All", "QB", "RB", "WR", "TE"),
          selected = "All"
        ),
        br(),
        
        # Compare mode
        checkboxInput(
          inputId = "compare_mode",
          label = "Compare two players",
          value = FALSE
        ),
        br(),
        
        # Player 1
        selectizeInput(
          inputId = "player_name",
          label = "Player 1:",
          choices = NULL,
          selected = "",
          options = list(placeholder = "Type to search...", maxOptions = 500)
        ),
        
        # Player 2 (only when compare mode is on)
        conditionalPanel(
          condition = "input.compare_mode == true",
          br(),
          selectizeInput(
            inputId = "player_name2",
            label = "Player 2 (same position recommended):",
            choices = NULL,
            selected = "",
            options = list(placeholder = "Type to search...", maxOptions = 500)
          )
        ),
        
        br(),
        downloadButton(outputId = "download_plot", label = "Download Plot as PNG", class = "btn-success"),
        hr(),
        p(HTML("<b>Note:</b> When comparing two players, the second player list is limited to the same position as Player 1 so that the weekly threshold lines remain meaningful.")),
        br()
      ),
      
      # ---------------- Rankings controls ----------------
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
        tabPanel("Player Performance", plotOutput(outputId = "player_plot", height = "600px")),
        tabPanel("Position Rankings", gt_output("rankings_table"))
      )
    )
  )
)

# ------------------------------------------------------------------
# 3. SERVER LOGIC
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ----- Reactive: available players based on Team + Position -----
  available_players <- reactive({
    df <- raw_stats
    if (!is.null(input$team_filter) && input$team_filter != "All") {
      df <- df |> filter(team == input$team_filter)
    }
    if (!is.null(input$position_filter) && input$position_filter != "All") {
      df <- df |> filter(position == input$position_filter)
    }
    df |> distinct(player_display_name) |> pull(player_display_name) |> sort()
  })
  
  # Update Player 1 choices only when team or position changes
  observeEvent(
    list(input$team_filter, input$position_filter),
    {
      choices <- available_players()
      current <- isolate(input$player_name)
      updateSelectizeInput(
        session,
        inputId = "player_name",
        choices = choices,
        selected = if (!is.null(current) && current %in% choices) current else "",
        server = TRUE
      )
    },
    ignoreInit = FALSE
  )
  
  # Update Player 2 choices only when relevant inputs change
  observeEvent(
    list(input$compare_mode, input$player_name, input$team_filter, input$position_filter),
    {
      if (!isTRUE(input$compare_mode)) return()
      
      df <- raw_stats
      
      # Apply team filter
      if (!is.null(input$team_filter) && input$team_filter != "All") {
        df <- df |> filter(team == input$team_filter)
      }
      
      # Restrict to same position as Player 1 (preferred)
      if (!is.null(input$player_name) && input$player_name != "") {
        pos1 <- raw_stats |>
          filter(player_display_name == input$player_name) |>
          slice(1) |>
          pull(position)
        if (length(pos1) == 1 && !is.na(pos1)) {
          df <- df |> filter(position == pos1)
        }
      } else if (!is.null(input$position_filter) && input$position_filter != "All") {
        df <- df |> filter(position == input$position_filter)
      }
      
      choices <- df |>
        distinct(player_display_name) |>
        pull(player_display_name) |>
        sort()
      
      # Never allow comparing a player to himself
      choices <- setdiff(choices, input$player_name)
      
      current2 <- isolate(input$player_name2)
      updateSelectizeInput(
        session,
        inputId = "player_name2",
        choices = choices,
        selected = if (!is.null(current2) && current2 %in% choices) current2 else "",
        server = TRUE
      )
    },
    ignoreInit = TRUE
  )
  
  plot_generator <- reactive({
    # Basic requirements
    req(input$player_name)
    req(input$player_name != "")
    
    # ---------- SINGLE PLAYER ----------
    if (!isTRUE(input$compare_mode) || is.null(input$player_name2) || input$player_name2 == "") {
      
      player_info <- raw_stats |> filter(player_display_name == input$player_name) |> slice(1)
      if (nrow(player_info) == 0) return(NULL)
      
      current_position <- player_info$position
      current_team     <- player_info$team
      
      player_colors <- team_colors_db |> filter(team_abbr == current_team) |> slice(1)
      primary_color <- if (nrow(player_colors) > 0) player_colors$team_color else "#112233"
      
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
      
      counts <- player_stats |> count(category, .drop = FALSE) |> mutate(pct = round(n / n_active * 100))
      labs_fill <- sprintf("%s (%s%%)", levels(player_stats$category), counts$pct)
      
      team_schedule <- raw_schedules |> filter(home_team == current_team | away_team == current_team)
      n_games      <- nrow(team_schedule)
      n_inactive   <- max(0, n_games - n_active)
      pct_inactive <- round(n_inactive / max(1, n_games) * 100)
      
      player_stats <- player_stats |> left_join(weekly_thresholds, by = "week")
      x_labels <- paste(player_stats$opponent_team, player_stats$week, sep = "\n")
      t12_name <- paste0("Weekly ", current_position, "12")
      t24_name <- paste0("Weekly ", current_position, "24")
      
      p <- ggplot(player_stats, aes(x = factor(week), y = ppr)) +
        geom_col(aes(fill = category), width = 0.7) +
        geom_text(aes(label = sprintf("%.1f", ppr)), vjust = -0.5, size = 3) +
        geom_line(aes(y = PosRank12, group = 1, linetype = t12_name), color = "gray50", linewidth = 0.8) +
        geom_line(aes(y = PosRank24, group = 1, linetype = t24_name), color = "red", linewidth = 0.8) +
        scale_fill_manual(values = c("#31a354", "#636363", "#e41a1c"), labels = labs_fill,
                          name = paste0("Performance\nInactive: ", pct_inactive, "%")) +
        scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
        scale_x_discrete(labels = x_labels) +
        scale_y_continuous(limits = c(0, max(c(player_stats$ppr, player_stats$PosRank12), na.rm = TRUE) * 1.2),
                           breaks = seq(0, 60, 5)) +
        labs(
          title = paste0(input$player_name, ": Week 1–18, 2025"),
          subtitle = paste0("<b>", n_active, " Active Games</b>: ", avg_ppr, " PPR/Game | <b>Position</b>: ", current_position),
          y = "PPR Fantasy Points", x = "Opponent / Week",
          caption = "logos/Graph_logo2.png",
          tag = paste0("<span style='color:", primary_color, "; font-weight:900; font-size:18px; font-family:Oswald;'>", current_team, "</span>")
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 0, vjust = 0.5, size = 8),
          legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = ggtext::element_markdown(size = 11),
          plot.background = element_rect(fill = "#F0F0F0"),
          plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
          plot.tag = ggtext::element_markdown(vjust = 1, hjust = 1),
          plot.tag.position = c(0.98, 0.98),
          legend.text = element_text(family = "sans", size = 9),
          legend.title = element_text(family = "sans", size = 10),
        )
      return(p)
    }
    
    # ---------- TWO-PLAYER COMPARISON ----------
    req(input$player_name2)
    req(input$player_name2 != "")
    req(input$player_name != input$player_name2)   # extra safety
    
    p1_info <- raw_stats |> filter(player_display_name == input$player_name)  |> slice(1)
    p2_info <- raw_stats |> filter(player_display_name == input$player_name2) |> slice(1)
    
    if (nrow(p1_info) == 0 || nrow(p2_info) == 0) return(NULL)
    
    pos1 <- p1_info$position
    pos2 <- p2_info$position
    
    # Warn if positions differ
    if (pos1 != pos2) {
      return(
        ggplot() +
          labs(title = "Position mismatch",
               subtitle = "Please select two players from the same position for a meaningful comparison.") +
          theme_void()
      )
    }
    
    current_position <- pos1
    
    # Weekly thresholds for the shared position
    weekly_thresholds <- raw_stats |>
      filter(position == current_position) |>
      group_by(week) |>
      arrange(desc(fantasy_points_ppr)) |>
      mutate(rank = row_number()) |>
      filter(rank %in% c(12, 24)) |>
      select(week, rank, fantasy_points_ppr) |>
      tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "PosRank") |>
      arrange(week)
    
    # Player 1 data
    p1_stats <- raw_stats |>
      filter(player_display_name == input$player_name) |>
      select(week, opponent_team, ppr = fantasy_points_ppr) |>
      mutate(player = input$player_name) |>
      arrange(week)
    
    # Player 2 data
    p2_stats <- raw_stats |>
      filter(player_display_name == input$player_name2) |>
      select(week, opponent_team, ppr = fantasy_points_ppr) |>
      mutate(player = input$player_name2) |>
      arrange(week)
    
    combined <- bind_rows(p1_stats, p2_stats) |>
      left_join(weekly_thresholds, by = "week")
    
    # Average lines for subtitle
    avg1 <- round(mean(p1_stats$ppr, na.rm = TRUE), 1)
    avg2 <- round(mean(p2_stats$ppr, na.rm = TRUE), 1)
    
    t12_name <- paste0("Weekly ", current_position, "12")
    t24_name <- paste0("Weekly ", current_position, "24")
    
    p <- ggplot(combined, aes(x = factor(week), y = ppr, fill = player)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.9) +
      geom_text(aes(label = sprintf("%.1f", ppr)),
                position = position_dodge(width = 0.75), vjust = -0.4, size = 2.7) +
      geom_line(aes(y = PosRank12, group = 1, linetype = t12_name), color = "gray40", linewidth = 0.8) +
      geom_line(aes(y = PosRank24, group = 1, linetype = t24_name), color = "red", linewidth = 0.8) +
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), name = "Player") +
      scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
      scale_y_continuous(limits = c(0, max(combined$ppr, combined$PosRank12, na.rm = TRUE) * 1.25),
                         breaks = seq(0, 60, 5)) +
      labs(
        title = paste0(input$player_name, " vs ", input$player_name2, " (", current_position, ")"),
        subtitle = paste0(
          "<b>", input$player_name, "</b>: ", avg1, " PPR/G &nbsp;&nbsp;|&nbsp;&nbsp; <b>",
          input$player_name2, "</b>: ", avg2, " PPR/G"
        ),
        y = "PPR Fantasy Points", x = "Week",
        caption = "logos/Graph_logo2.png"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 9),
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 12),
        plot.background = element_rect(fill = "#F0F0F0"),
        plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
        legend.text = element_text(family = "sans", size = 9),
        legend.title = element_text(family = "sans", size = 10),
      )
    return(p)
  })
  
  # Render plot
  output$player_plot <- renderPlot({
    p <- plot_generator()
    if (is.null(p)) {
      ggplot() + labs(title = "Select at least one player.") + theme_void()
    } else {
      p
    }
  })
  
  # Download
  output$download_plot <- downloadHandler(
    filename = function() {
      if (isTRUE(input$compare_mode) && !is.null(input$player_name2) && input$player_name2 != "") {
        paste0(gsub(" ", "_", tolower(input$player_name)), "_vs_",
               gsub(" ", "_", tolower(input$player_name2)), "_2025.png")
      } else {
        paste0(gsub(" ", "_", tolower(input$player_name)), "_fantasy_performance_2025.png")
      }
    },
    content = function(file) {
      ggsave(file, plot = plot_generator(), device = "png", width = 12, height = 7, dpi = 300)
    }
  )
  
  # Rankings table (unchanged)
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