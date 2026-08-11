# Load required packages
library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)

# Load weekly player stats for the most recent five seasons
stats <- load_player_stats(seasons = 2021:2025)

# ============================================================
# 2. Filter to skill positions
# ============================================================

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
    team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  ) %>%
  group_by(player_id)

seasonal_stats_2021 <- stats %>%
  filter(
    season == 2021,
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    player_id,
    player_display_name,
    position,
    season,
    week,
    team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  ) %>%
  group_by(player_id)

seasonal_stats_2022 <- stats %>%
  filter(
    season == 2022,
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    player_id,
    player_display_name,
    position,
    season,
    week,
    team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  ) %>%
  group_by(player_id)

seasonal_stats_2023 <- stats %>%
  filter(
    season == 2023,
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    player_id,
    player_display_name,
    position,
    season,
    week,
    team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  ) %>%
  group_by(player_id)

seasonal_stats_2024 <- stats %>%
  filter(
    season == 2024,
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    player_id,
    player_display_name,
    position,
    season,
    week,
    team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  ) %>%
  group_by(player_id)

seasonal_stats_2025 <- stats %>%
  filter(
    season == 2025,
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  select(
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds, fumbles_total,
    fantasy_points_ppr
  )


summary(seasonal_stats_2021$passing_yards)
summary(seasonal_stats_2022$passing_yards)
summary(seasonal_stats_2023$passing_yards)
summary(seasonal_stats_2024$passing_yards)
summary(seasonal_stats_2025$passing_yards)

cor.test(seasonal_stats_2025$rushing_yards, seasonal_stats_2025$fantasy_points_ppr, method = "pearson")

plot(seasonal_stats_2025$rushing_yards, seasonal_stats_2025$fantasy_points_ppr)

m <- cor(seasonal_stats_2025)
corrplot(m, method = 'number')
