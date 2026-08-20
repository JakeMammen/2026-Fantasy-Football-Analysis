# prepare_data.R
# Multi-year version: 2020–2025
# Creates all data required by the Shiny app (including Top-40 rankings)

library(nflreadr)
library(dplyr)

dir.create("data", showWarnings = FALSE)

message("Downloading and processing 2020–2025 data...")

seasons <- 2020:2025

# ------------------------------------------------------------------
# Core data
# ------------------------------------------------------------------
all_stats <- load_player_stats(seasons = seasons) |>
  filter(position %in% c("QB", "RB", "WR", "TE"), season_type == "REG")

raw_schedules <- load_schedules(seasons = seasons) |>
  filter(game_type == "REG")

team_colors_db <- load_teams() |>
  select(team_abbr, team_color, team_color2)

# ------------------------------------------------------------------
# Ranking helper (per position, across all seasons)
# ------------------------------------------------------------------
compute_rankings <- function(pos, stats) {
  pos_stats <- stats |> filter(position == pos)
  
  # Season-level player summary
  player_stats <- pos_stats |>
    group_by(season, player_id, player_display_name, team) |>
    summarise(
      active_games = n(),
      ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Weekly ranks within each season
  weekly_ranks <- pos_stats |>
    group_by(season, week) |>
    arrange(desc(fantasy_points_ppr)) |>
    mutate(rank = row_number()) |>
    ungroup() |>
    select(season, player_id, player_display_name, week, rank) |>
    mutate(
      category = case_when(
        rank <= 12 ~ paste0(pos, "1"),
        rank <= 24 ~ paste0(pos, "2"),
        TRUE       ~ paste0(pos, "3+")
      )
    )
  
  # Tier percentages
  player_categories <- weekly_ranks |>
    group_by(season, player_id, player_display_name) |>
    summarise(
      total_games = n(),
      tier1_games = sum(category == paste0(pos, "1")),
      tier2_games = sum(category == paste0(pos, "2")),
      tier3_games = sum(category == paste0(pos, "3+")),
      tier1_pct   = round(tier1_games / total_games * 100, 1),
      tier2_pct   = round(tier2_games / total_games * 100, 1),
      tier3_pct   = round(tier3_games / total_games * 100, 1),
      .groups = "drop"
    )
  
  # Final rankings – Top 40 per season
  player_stats |>
    left_join(player_categories, by = c("season", "player_id", "player_display_name")) |>
    group_by(season) |>
    arrange(desc(ppr_per_game), .by_group = TRUE) |>
    mutate(rank = row_number()) |>
    filter(active_games >= 4, rank <= 40) |>
    ungroup() |>
    mutate(position = pos) |>
    select(
      season, position, rank, player_display_name, team,
      ppr_per_game, tier1_pct, tier2_pct, tier3_pct
    )
}

message("Computing rankings (this may take a minute)...")

rankings <- bind_rows(
  compute_rankings("RB", all_stats),
  compute_rankings("WR", all_stats),
  compute_rankings("TE", all_stats),
  compute_rankings("QB", all_stats)
)

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
saveRDS(all_stats,      "data/all_stats.rds")
saveRDS(raw_schedules,  "data/raw_schedules.rds")
saveRDS(team_colors_db, "data/team_colors_db.rds")
saveRDS(rankings,       "data/rankings.rds")

writeLines(as.character(Sys.time()), "data/data_version.txt")

message("Data preparation complete.")
message("Files written to data/:")
message("  - all_stats.rds")
message("  - raw_schedules.rds")
message("  - team_colors_db.rds")
message("  - rankings.rds")