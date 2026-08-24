# =============================================================================
# Correlation analysis on season-long skill-position stats
# Position-specific relevant variables + clean plots + next-season tables
# Focus: PPR only | Restricted to 2022–2025
# =============================================================================

library(dplyr)
library(tidyr)
library(corrplot)
library(arrow)
library(gt)

# -----------------------------------------------------------------------------
# Create output directories
# -----------------------------------------------------------------------------
dir.create("output/graphs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Load the season-long data
# -----------------------------------------------------------------------------
skill_stats <- read_parquet("data/skill_stats_season_2015_2025.parquet")

glimpse(skill_stats)
count(skill_stats, season, position)

# -----------------------------------------------------------------------------
# 2. Position-specific variable lists (PPR only)
# -----------------------------------------------------------------------------
vars_by_position <- list(
  QB = c(
    "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
    "carries", "rushing_yards", "rushing_tds",
    "fantasy_points_ppr", "fantasy_points_ppr_per_game"
  ),
  RB = c(
    "carries", "rushing_yards", "rushing_tds",
    "targets", "receptions", "receiving_yards", "receiving_tds",
    "carries_per_game", "targets_per_game", "receptions_per_game",
    "fantasy_points_ppr", "fantasy_points_ppr_per_game"
  ),
  WR = c(
    "targets", "receptions", "receiving_yards", "receiving_yards_after_catch",
    "receiving_tds", "receiving_first_downs",
    "targets_per_game", "receptions_per_game",
    "fantasy_points_ppr", "fantasy_points_ppr_per_game"
  ),
  TE = c(
    "targets", "receptions", "receiving_yards", "receiving_yards_after_catch", "receiving_tds",
    "targets_per_game", "receptions_per_game", "receiving_first_downs",
    "fantasy_points_ppr", "fantasy_points_ppr_per_game"
  )
)

# -----------------------------------------------------------------------------
# 3. Correlation plot helper (safe version)
# -----------------------------------------------------------------------------
plot_position_cor <- function(data,
                              pos,
                              season_filter = NULL,
                              title_suffix = "",
                              min_obs = 30,
                              threshold = 0.50) {
  
  df <- data %>%
    filter(position == pos) %>%
    { if (!is.null(season_filter)) filter(., season %in% season_filter) else . }
  
  keep_vars <- vars_by_position[[pos]]
  
  num_cols <- df %>%
    select(any_of(keep_vars)) %>%
    select(where(is.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0))
  
  if (nrow(num_cols) < min_obs) {
    message("Insufficient observations for ", pos)
    return(invisible(NULL))
  }
  
  m <- cor(num_cols, use = "pairwise.complete.obs")
  
  # Order on full matrix, then blank weak correlations for display
  ord <- corrMatOrder(m, order = "hclust")
  m_ordered <- m[ord, ord]
  
  m_plot <- m_ordered
  m_plot[abs(m_plot) < threshold] <- NA
  diag(m_plot) <- NA
  
  corrplot(
    m_plot,
    method      = "number",
    order       = "original",
    tl.cex      = 0.80,
    number.cex  = 0.70,
    tl.col      = "black",
    col         = COL2("RdBu", 10),
    addCoef.col = "black",
    na.label    = " ",
    title       = paste0(pos, " – Correlations", title_suffix),
    mar         = c(0, 0, 2, 0)
  )
}

# -----------------------------------------------------------------------------
# 4. Next-season correlation helper
#    Current season stats → Next season fantasy_points_ppr / FPG
#    Restricted to base seasons 2022–2024 (so next season ≤ 2025)
# -----------------------------------------------------------------------------
get_next_season_cors <- function(data,
                                 pos,
                                 min_games = 6,
                                 min_obs = 30) {
  
  keep_vars <- vars_by_position[[pos]]
  
  needed_cols <- c(
    "player_id", "player_display_name", "season", "games",
    keep_vars
  )
  
  df <- data %>%
    filter(position == pos,
           season %in% 2022:2025) %>%          # only seasons that have a next year
    select(any_of(needed_cols)) %>%
    filter(games >= min_games)
  
  # Safety: create per-game if missing
  if (!"fantasy_points_ppr_per_game" %in% names(df)) {
    df <- df %>%
      mutate(fantasy_points_ppr_per_game = fantasy_points_ppr / games)
  }
  
  # Create next-season targets
  df_lagged <- df %>%
    arrange(player_id, season) %>%
    group_by(player_id) %>%
    mutate(
      next_fantasy_points_ppr          = lead(fantasy_points_ppr),
      next_fantasy_points_ppr_per_game = lead(fantasy_points_ppr_per_game)
    ) %>%
    ungroup() %>%
    filter(!is.na(next_fantasy_points_ppr))
  
  if (nrow(df_lagged) < min_obs) {
    message("Insufficient lagged observations for ", pos, " (n = ", nrow(df_lagged), ")")
    return(NULL)
  }
  
  # Predictors = everything except identifiers and the *next-season* targets
  # (we now KEEP current fantasy_points_ppr and fantasy_points_ppr_per_game)
  stats_to_cor <- setdiff(
    names(df_lagged),
    c("player_id", "player_display_name", "season", "games",
      "next_fantasy_points_ppr", "next_fantasy_points_ppr_per_game")
  )
  
  cors <- lapply(stats_to_cor, function(stat) {
    tibble(
      position        = pos,
      stat            = stat,
      cor_next_ppr    = cor(df_lagged[[stat]], df_lagged$next_fantasy_points_ppr,
                            use = "pairwise.complete.obs"),
      cor_next_ppr_pg = cor(df_lagged[[stat]], df_lagged$next_fantasy_points_ppr_per_game,
                            use = "pairwise.complete.obs"),
      n               = sum(!is.na(df_lagged[[stat]]) & !is.na(df_lagged$next_fantasy_points_ppr))
    )
  }) %>%
    bind_rows() %>%
    mutate(
      cor_next_ppr    = round(cor_next_ppr, 3),
      cor_next_ppr_pg = round(cor_next_ppr_pg, 3)
    ) %>%
    arrange(desc(abs(cor_next_ppr)))
  
  return(cors)
}

# -----------------------------------------------------------------------------
# 5. Generate next-season correlation tables (2022–2025 window)
# -----------------------------------------------------------------------------
next_cor_tables <- lapply(c("QB", "RB", "WR", "TE"), function(p) {
  get_next_season_cors(skill_stats, pos = p)
}) %>%
  bind_rows()

# View combined results
print(next_cor_tables)

# -----------------------------------------------------------------------------
# 6. Clean gt tables + save
# -----------------------------------------------------------------------------
make_next_gt <- function(pos) {
  
  # Clean / abbreviate statistic names for display
  label_map <- c(
    "completions"               = "Completions",
    "attempts"                  = "Attempts",
    "passing_yards"             = "Pass Yds",
    "passing_tds"               = "Pass TDs",
    "passing_interceptions"     = "INTs",
    "carries"                   = "Carries",
    "rushing_yards"             = "Rush Yds",
    "rushing_tds"               = "Rush TDs",
    "carries_per_game"          = "Carries / G",
    "targets"                   = "Targets",
    "receptions"                = "Receptions",
    "receiving_yards"           = "Rec Yds",
    "receiving_yards_after_catch" = "YAC",
    "receiving_tds"             = "Rec TDs",
    "receiving_first_downs"     = "Rec 1st Downs",
    "targets_per_game"          = "Targets / G",
    "receptions_per_game"       = "Receptions / G",
    "fantasy_points_ppr"        = "FP (PPR)",
    "fantasy_points_ppr_per_game" = "FP / G (PPR)"
  )
  
  tbl <- next_cor_tables %>%
    filter(position == pos) %>%
    select(stat, cor_next_ppr) %>%
    mutate(
      stat = coalesce(label_map[stat], str_to_title(str_replace_all(stat, "_", " ")))
    ) %>%
    gt() %>%
    cols_label(
      stat         = "Statistic",
      cor_next_ppr = "Correlation"
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(columns = everything())
    ) %>%
    fmt_number(columns = cor_next_ppr, decimals = 3) %>%
    data_color(
      columns = cor_next_ppr,
      colors = scales::col_numeric(
        palette = c("#B2182B", "#F4A582", "#FDDBC7", "#D1E5F0", "#67A9CF", "#2166AC"),
        domain = c(0, 1)
      )
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_header(
      title = md(paste0("**", pos, " Stats – Correlation with NEXT-SEASON PPR Fantasy Points**")),
      subtitle = md("Next season total PPR points | Sorted by absolute correlation strength | <b>Base seasons:</b> 2022–2024")
    ) %>%
    tab_style(style = cell_text(weight = "bold"), locations = cells_title(groups = "title")) %>%
    tab_style(style = cell_text(style = "italic"), locations = cells_title(groups = "subtitle")) %>%
    opt_table_font(font = "Arial") %>%
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
      row.striping.include_table_body = TRUE,
      table.font.size = 14
    ) %>%
    tab_source_note(
      source_note = html(
        paste0(
          "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
           <div style='font-size: 12px;'>
             <b>Data:</b> nflreadR | <b>Created by:</b> @FantasySPack & @jakemammen
           </div>",
          local_image(
            filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
            height = 30
          ),
          "</div>"
        )
      )
    )
  
  # Save the table
  gtsave(tbl, filename = file.path("output/tables", paste0(pos, "_next_season_correlations.png")),
         vwidth = 800, vheight = 700)
  
  return(tbl)   # still prints in the viewer
}

# Generate + save all four tables
make_next_gt("QB")
make_next_gt("RB")
make_next_gt("WR")
make_next_gt("TE")