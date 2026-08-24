# =============================================================================
# Season-long skill-position player stats (2015–2025) + correlation analysis
# Packages: nflreadr, tidyverse, corrplot, arrow
# =============================================================================

library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)
library(arrow)

# -----------------------------------------------------------------------------
# 1. Load weekly data
# -----------------------------------------------------------------------------
seasons <- 2015:2025

raw_weekly <- load_player_stats(
  seasons       = seasons,
  summary_level = "week"
)

# -----------------------------------------------------------------------------
# 2. Filter to skill positions + regular season, then aggregate to season level
# -----------------------------------------------------------------------------
skill_season <- raw_weekly %>%
  filter(
    season_type == "REG",
    position %in% c("QB", "RB", "WR", "TE")
  ) %>%
  group_by(player_id, player_display_name, player_name, position, position_group, season) %>%
  summarise(
    # Games played (important for per-game rates)
    games = n(),
    
    # Passing
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    passing_interceptions = sum(passing_interceptions, na.rm = TRUE),
    passing_first_downs = sum(passing_first_downs, na.rm = TRUE),
    passing_yards_after_catch = sum(passing_yards_after_catch, na.rm = TRUE),
    
    # Rushing
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    rushing_first_downs = sum(rushing_first_downs, na.rm = TRUE),
    
    # Receiving
    receptions = sum(receptions, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_yards_after_catch = sum(receiving_yards_after_catch, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    receiving_first_downs = sum(receiving_first_downs, na.rm = TRUE),
    
    # Fumbles
    sack_fumbles_lost = sum(sack_fumbles_lost, na.rm = TRUE),
    rushing_fumbles_lost = sum(rushing_fumbles_lost, na.rm = TRUE),
    receiving_fumbles_lost = sum(receiving_fumbles_lost, na.rm = TRUE),
    
    # Fantasy points (season totals)
    fantasy_points = sum(fantasy_points, na.rm = TRUE),
    fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Half-PPR season total
    fantasy_points_half_ppr = fantasy_points + 0.5 * receptions,
    
    # ---- True per-game rates (the key addition) ----
    fantasy_points_per_game       = fantasy_points / games,
    fantasy_points_ppr_per_game   = fantasy_points_ppr / games,
    fantasy_points_half_ppr_per_game = fantasy_points_half_ppr / games,
    
    # Optional useful rate stats
    targets_per_game     = targets / games,
    carries_per_game     = carries / games,
    receptions_per_game  = receptions / games,
    passing_attempts_per_game = attempts / games
  ) %>%
  # Optional: require a minimum number of games to reduce noise
  filter(games >= 6)

# Quick checks
glimpse(skill_season)
count(skill_season, season, position)

# -----------------------------------------------------------------------------
# 3. Save
# -----------------------------------------------------------------------------
dir.create("data", showWarnings = FALSE)

write_parquet(skill_season, "data/skill_stats_season_2015_2025.parquet")
# saveRDS(skill_season, "data/skill_stats_season_2015_2025.rds")