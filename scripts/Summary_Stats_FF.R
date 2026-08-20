# Load required packages
library(nflreadr)
library(nflfastR)      # for calculate_player_stats()
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)

# 2. Aggregate to player-level weekly stats (matches the structure of load_player_stats)
stats <- calculate_stats(seasons = 2020:2025, summary_level = "week", stat_type = 'player', season_type = "REG")

# 3. Filter to regular-season skill-position players and select core columns
seasonal_stats <- stats %>%
  filter(
    season_type == "REG",
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    player_id,
    player_display_name,
    position,
    season,
    week,
    team,          # calculate_player_stats uses recent_team
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_yards_after_catch, receiving_tds,
    sack_fumbles_lost, rushing_fumbles_lost, receiving_fumbles_lost,                # or fumbles_total depending on version
    fantasy_points_ppr
  )

# Year-specific subsets (optional; the single filtered object above is usually sufficient)
seasonal_stats_2020 <- seasonal_stats %>% filter(season == 2020)
seasonal_stats_2021 <- seasonal_stats %>% filter(season == 2021)
seasonal_stats_2022 <- seasonal_stats %>% filter(season == 2022)
seasonal_stats_2023 <- seasonal_stats %>% filter(season == 2023)
seasonal_stats_2024 <- seasonal_stats %>% filter(season == 2024)
seasonal_stats_2025 <- seasonal_stats %>% filter(season == 2025)

# 4. Summaries
summary(seasonal_stats_2020$passing_yards)
summary(seasonal_stats_2021$passing_yards)
summary(seasonal_stats_2022$passing_yards)
summary(seasonal_stats_2023$passing_yards)
summary(seasonal_stats_2024$passing_yards)
summary(seasonal_stats_2025$passing_yards)

# Correlation matrix (numeric columns only)
numeric_cols <- seasonal_stats_2025 %>%
  select(where(is.numeric)) %>%
  select(-season, -week)   # drop identifiers if desired

m <- cor(numeric_cols, use = "pairwise.complete.obs")
corrplot(m, method = "number", type = "upper", tl.cex = 0.7)

library(dplyr)
library(corrplot)

# Helper function for consistent plotting
plot_position_cor <- function(data, pos, title_suffix = "") {
  numeric_data <- data %>%
    filter(position == pos) %>%
    select(
      completions, attempts, passing_yards, passing_tds, passing_interceptions,
      carries, rushing_yards, rushing_tds,
      receptions, targets, receiving_yards, receiving_yards_after_catch, receiving_tds,
      fantasy_points_ppr
    ) %>%
    select(where(is.numeric))
  
  if (nrow(numeric_data) < 30) {
    message("Insufficient observations for ", pos)
    return(invisible(NULL))
  }
  
  m <- cor(numeric_data, use = "pairwise.complete.obs")
  
  corrplot(
    m,
    method = "number",
    type = "upper",
    tl.cex = 0.75,
    number.cex = 0.65,
    title = paste0(pos, " – Correlations", title_suffix),
    mar = c(0, 0, 2, 0)
  )
}

# Generate one matrix per position (2025 example)
plot_position_cor(seasonal_stats_2025, "QB", " (2025 REG)")
plot_position_cor(seasonal_stats_2025, "RB", " (2025 REG)")
plot_position_cor(seasonal_stats_2025, "WR", " (2025 REG)")
plot_position_cor(seasonal_stats_2025, "TE", " (2025 REG)")

plot_position_cor(seasonal_stats_2024, "QB", " (2024 REG)")
plot_position_cor(seasonal_stats_2024, "RB", " (2024 REG)")
plot_position_cor(seasonal_stats_2024, "WR", " (2024 REG)")
plot_position_cor(seasonal_stats_2024, "TE", " (2024 REG)")

plot_position_cor(seasonal_stats_2023, "QB", " (2023 REG)")
plot_position_cor(seasonal_stats_2023, "RB", " (2023 REG)")
plot_position_cor(seasonal_stats_2023, "WR", " (2023 REG)")
plot_position_cor(seasonal_stats_2023, "TE", " (2023 REG)")

plot_position_cor(seasonal_stats_2022, "QB", " (2022 REG)")
plot_position_cor(seasonal_stats_2022, "RB", " (2022 REG)")
plot_position_cor(seasonal_stats_2022, "WR", " (2022 REG)")
plot_position_cor(seasonal_stats_2022, "TE", " (2022 REG)")

plot_position_cor(seasonal_stats_2021, "QB", " (2021 REG)")
plot_position_cor(seasonal_stats_2021, "RB", " (2021 REG)")
plot_position_cor(seasonal_stats_2021, "WR", " (2021 REG)")
plot_position_cor(seasonal_stats_2021, "TE", " (2021 REG)")

