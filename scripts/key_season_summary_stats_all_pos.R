# =============================================================================
# Position Season Trends (QB / RB / WR / TE) – 2020–2025
# Matching aesthetics + logo
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(arrow)
library(ggtext)   # for element_markdown
library(ggpath)   # for logo in caption

# -----------------------------------------------------------------------------
# Create output directories
# -----------------------------------------------------------------------------
dir.create("output/graphs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 0. Load data
# -----------------------------------------------------------------------------
skill_stats <- read_parquet("data/skill_stats_season_2015_2025.parquet")

# -----------------------------------------------------------------------------
# 1. Metrics + labels (unchanged)
# -----------------------------------------------------------------------------
metrics_by_position <- list(
  QB = c(
    "attempts"                    = "Attempts",
    "completions"                 = "Completions",
    "passing_yards"               = "Passing Yards",
    "passing_tds"                 = "Passing TDs",
    "carries"                     = "Carries",
    "rushing_yards"               = "Rushing Yards",
    "rushing_tds"                 = "Rushing TDs",
    "passing_interceptions"       = "Passing Interceptions",
    "fantasy_points_ppr_per_game" = "Fantasy Points PPR / Game",
    "fantasy_points_ppr"          = "Fantasy Points PPR Total"
  ),
  RB = c(
    "carries"                     = "Carries",
    "rushing_yards"               = "Rushing Yards",
    "rushing_tds"                 = "Rushing TDs",
    "targets"                     = "Targets",
    "receptions"                  = "Receptions",
    "receiving_yards"             = "Receiving Yards",
    "receiving_tds"               = "Receiving TDs",
    "fantasy_points_ppr_per_game" = "Fantasy Points PPR / Game",
    "fantasy_points_ppr"          = "Fantasy Points PPR Total"
  ),
  WR = c(
    "targets"                     = "Targets",
    "receptions"                  = "Receptions",
    "receiving_yards"             = "Receiving Yards",
    "receiving_yards_after_catch" = "Receiving Yards After Catch",
    "receiving_tds"               = "Receiving TDs",
    "carries"                     = "Carries",
    "rushing_yards"               = "Rushing Yards",
    "rushing_tds"                 = "Rushing TDs",
    "fantasy_points_ppr_per_game" = "Fantasy Points PPR / Game",
    "fantasy_points_ppr"          = "Fantasy Points PPR Total"
  ),
  TE = c(
    "targets"                     = "Targets",
    "receptions"                  = "Receptions",
    "receiving_yards"             = "Receiving Yards",
    "receiving_yards_after_catch" = "Receiving Yards After Catch",
    "receiving_tds"               = "Receiving TDs",
    "carries"                     = "Carries",
    "rushing_yards"               = "Rushing Yards",
    "rushing_tds"                 = "Rushing TDs",
    "fantasy_points_ppr_per_game" = "Fantasy Points PPR / Game",
    "fantasy_points_ppr"          = "Fantasy Points PPR Total"
  )
)

# -----------------------------------------------------------------------------
# 2. Volume filters
# -----------------------------------------------------------------------------
volume_filters <- list(
  QB = function(df) df %>% filter(attempts >= 300),
  RB = function(df) df %>% filter((carries + targets) >= 150),
  WR = function(df) df %>% filter(targets >= 70),
  TE = function(df) df %>% filter(targets >= 50)
)

# -----------------------------------------------------------------------------
# 3. Season summary helper
# -----------------------------------------------------------------------------
make_season_summary <- function(data, pos) {
  needed_metrics <- names(metrics_by_position[[pos]])
  
  data %>%
    filter(position == pos, season %in% 2020:2025) %>%
    volume_filters[[pos]]() %>%
    group_by(season) %>%
    summarise(
      across(any_of(needed_metrics), ~ mean(.x, na.rm = TRUE)),
      n_players = n(),
      .groups = "drop"
    ) %>%
    mutate(position = pos)
}

# -----------------------------------------------------------------------------
# 4. Build summaries
# -----------------------------------------------------------------------------
season_summaries <- map_dfr(c("QB", "RB", "WR", "TE"), ~ make_season_summary(skill_stats, .x))

# -----------------------------------------------------------------------------
# 5. Long format
# -----------------------------------------------------------------------------
season_long <- season_summaries %>%
  select(-n_players) %>%
  pivot_longer(cols = -c(season, position), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    metric_label = map2_chr(position, metric, ~ {
      lab <- metrics_by_position[[.x]][.y]
      if (is.na(lab)) str_to_title(str_replace_all(.y, "_", " ")) else lab
    }),
    metric_label = factor(metric_label, levels = unique(unlist(lapply(metrics_by_position, unname))))
  )

# -----------------------------------------------------------------------------
# 6. Position trend plots + save
# -----------------------------------------------------------------------------
plot_position_trends <- function(pos) {
  
  ordered_labels <- unname(metrics_by_position[[pos]])
  
  df <- season_long %>%
    filter(position == pos) %>%
    mutate(metric_label = factor(metric_label, levels = ordered_labels))
  
  p <- ggplot(df, aes(x = season, y = value)) +
    geom_line(color = "#1f77b4", linewidth = 1.1) +
    geom_point(color = "#1f77b4", size = 2.5) +
    facet_wrap(~ metric_label, scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = 2020:2025) +
    labs(
      title = paste0(pos, " Season Averages (2020–2025)"),
      subtitle = paste0(
        "<b>Higher-volume players only</b> | PPR scoring | ",
        "<b>Data:</b> nflreadR"
      ),
      x = "Season",
      y = NULL,
      caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "black"),
      plot.subtitle = element_markdown(size = 12, color = "gray30"),
      plot.background = element_rect(fill = "#F0F0F0", color = NA),
      panel.background = element_rect(fill = "#F0F0F0", color = NA),
      strip.text = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "#E8E8E8", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray85"),
      axis.text.x = element_text(angle = 0, size = 10),
      axis.text.y = element_text(size = 9),
      plot.caption = element_path(hjust = 1, size = 0.9),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # Save
  ggsave(
    filename = file.path("output/graphs", paste0(pos, "_season_trends_2020_2025.png")),
    plot = p,
    width = 14, height = 10, dpi = 300, bg = "#F0F0F0"
  )
  
  return(p)
}

# Generate + save the four position plots
plot_position_trends("QB")
plot_position_trends("RB")
plot_position_trends("WR")
plot_position_trends("TE")

# -----------------------------------------------------------------------------
# 7. Bonus multi-position plot + save
# -----------------------------------------------------------------------------
key_fantasy <- season_long %>%
  filter(metric %in% c("fantasy_points_ppr", "fantasy_points_ppr_per_game",
                       "targets", "carries", "receptions"))

p_key <- ggplot(key_fantasy, aes(x = season, y = value, color = position)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  facet_wrap(~ metric_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(
    "QB" = "#d62728",
    "RB" = "#2ca02c",
    "WR" = "#1f77b4",
    "TE" = "#ff7f0e"
  )) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "Key PPR Fantasy Metrics by Position (2020–2025)",
    subtitle = "<b>Higher-volume players only</b> | <b>Data:</b> nflreadR",
    x = "Season",
    y = NULL,
    color = "Position",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_markdown(size = 12, color = "gray30"),
    plot.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.background = element_rect(fill = "#F0F0F0", color = NA),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    legend.position = "top",
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    plot.caption = element_path(hjust = 1, size = 0.9),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave(
  filename = "output/graphs/key_ppr_metrics_by_position_2020_2025.png",
  plot = p_key,
  width = 12, height = 9, dpi = 300, bg = "#F0F0F0"
)

print(p_key)