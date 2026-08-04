# ============================================================
# 1. Load packages and data
# ============================================================

# Load required packages
library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)

# Load weekly player stats for the most recent five seasons
stats <- load_player_stats(seasons = 2021:2025)

# Quick inspection
glimpse(stats)

# ============================================================
# 2. Filter to skill positions
# ============================================================

skill_stats <- stats %>%
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
  group_by(player_id) %>%
  mutate(
    seasons_played = n_distinct(season),
    played_2025    = any(season == 2025)
  ) %>%
  ungroup() %>%
  filter(played_2025) %>%
  select(-played_2025) %>%
  group_by(player_id, player_display_name, position, season, seasons_played) %>%
  summarise(
    across(
      c(completions, attempts, passing_yards, passing_tds, passing_interceptions,
        carries, rushing_yards, rushing_tds,
        receptions, targets, receiving_yards, receiving_tds, fumbles_total,
        fantasy_points_ppr),
      \(x) sum(x, na.rm = TRUE)
    ),
    team = last(team),
    .groups = "drop"
  )

# Quick checks
dim(skill_stats)
table(skill_stats$position)
glimpse(skill_stats)


