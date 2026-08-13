# prepare_data.R
# Run this script locally to download, process, and save all data required by the Shiny app.
# After running, the entire application folder (including data/) is ready for deployment to Posit Connect.

library(nflreadr)
library(dplyr)

message("Downloading and processing 2025 data...")

# ------------------------------------------------------------------
# Core data
# ------------------------------------------------------------------
all_stats <- load_player_stats(seasons = 2025) |>
  filter(position %in% c("QB", "RB", "WR", "TE"), season_type == "REG")

raw_schedules <- load_schedules(seasons = 2025) |>
  filter(game_type == "REG")

team_colors_db <- load_teams() |>
  select(team_abbr, team_color, team_color2)

# ------------------------------------------------------------------
# Ranking computations (identical logic to original scripts)
# ------------------------------------------------------------------

# Helper for team colour join later (also saved)
team_info <- team_colors_db |>
  select(team_abbr, team_color) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

# ----- RB -----
rb_stats <- all_stats |> filter(position == "RB")

rb_player_stats <- rb_stats |>
  group_by(player_id, player_display_name, team) |>
  summarise(
    active_games = n(),
    ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
    .groups = "drop",
    team = max(case_when(
      player_display_name == "Travis Etienne" ~ "NO",
      player_display_name == "Kenny Gainwell" ~ "TB",
      player_display_name == "Rico Dowdle" ~ "PIT",
      TRUE ~ team
    ), na.rm = TRUE)
  )

rb_weekly_ranks <- rb_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  select(player_id, player_display_name, week, rank) |>
  mutate(category = case_when(
    rank <= 12 ~ "RB1",
    rank <= 24 ~ "RB2",
    TRUE ~ "RB3+"
  ))

rb_player_categories <- rb_weekly_ranks |>
  group_by(player_id, player_display_name) |>
  summarise(
    total_games = n(),
    rb1_games = sum(category == "RB1"),
    rb2_games = sum(category == "RB2"),
    rb3_games = sum(category == "RB3+"),
    rb1_pct = round(rb1_games / total_games * 100, 1),
    rb2_pct = round(rb2_games / total_games * 100, 1),
    rb3_pct = round(rb3_games / total_games * 100, 1),
    .groups = "drop"
  )

rb_final_stats <- rb_player_stats |>
  left_join(rb_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(rank, player_display_name, team, ppr_per_game, rb1_pct, rb2_pct, rb3_pct)

# ----- WR -----
wr_stats <- all_stats |> filter(position == "WR")

wr_player_stats <- wr_stats |>
  group_by(player_id, player_display_name, team) |>
  summarise(
    active_games = n(),
    ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
    .groups = "drop",
    team = max(case_when(
      player_display_name == "A.J. Brown" ~ "NE",
      player_display_name == "Wan'Dale Robinson" ~ "TEN",
      TRUE ~ team
    ), na.rm = TRUE)
  ) |>
  filter(player_display_name != "Tyreek Hill")

wr_weekly_ranks <- wr_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  select(player_id, player_display_name, week, rank) |>
  mutate(category = case_when(
    rank <= 12 ~ "WR1",
    rank <= 24 ~ "WR2",
    TRUE ~ "WR3+"
  ))

wr_player_categories <- wr_weekly_ranks |>
  group_by(player_id, player_display_name) |>
  summarise(
    total_games = n(),
    wr1_games = sum(category == "WR1"),
    wr2_games = sum(category == "WR2"),
    wr3_games = sum(category == "WR3+"),
    wr1_pct = round(wr1_games / total_games * 100, 1),
    wr2_pct = round(wr2_games / total_games * 100, 1),
    wr3_pct = round(wr3_games / total_games * 100, 1),
    .groups = "drop"
  )

wr_final_stats <- wr_player_stats |>
  left_join(wr_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(rank, player_display_name, team, ppr_per_game, wr1_pct, wr2_pct, wr3_pct)

# ----- TE -----
te_stats <- all_stats |> filter(position == "TE")

te_player_stats <- te_stats |>
  group_by(player_id, player_display_name, team) |>
  summarise(
    active_games = n(),
    ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
    .groups = "drop",
    team = max(case_when(
      player_display_name == "David Njoku" ~ "LAC",
      TRUE ~ team
    ), na.rm = TRUE)
  ) |>
  filter(player_display_name != "Darren Waller" & player_display_name != "Zach Ertz")

te_weekly_ranks <- te_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  select(player_id, player_display_name, week, rank) |>
  mutate(category = case_when(
    rank <= 12 ~ "TE1",
    rank <= 24 ~ "TE2",
    TRUE ~ "TE3+"
  ))

te_player_categories <- te_weekly_ranks |>
  group_by(player_id, player_display_name) |>
  summarise(
    total_games = n(),
    te1_games = sum(category == "TE1"),
    te2_games = sum(category == "TE2"),
    te3_games = sum(category == "TE3+"),
    te1_pct = round(te1_games / total_games * 100, 1),
    te2_pct = round(te2_games / total_games * 100, 1),
    te3_pct = round(te3_games / total_games * 100, 1),
    .groups = "drop"
  )

te_final_stats <- te_player_stats |>
  left_join(te_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(rank, player_display_name, team, ppr_per_game, te1_pct, te2_pct, te3_pct)

# ----- QB -----
qb_stats <- all_stats |> filter(position == "QB")

qb_player_stats <- qb_stats |>
  group_by(player_id, player_display_name, team) |>
  summarise(
    active_games = n(),
    ppr_per_game = mean(fantasy_points_ppr, na.rm = TRUE),
    .groups = "drop",
    team = max(case_when(
      player_display_name == "Justin Fields" ~ "KC",
      player_display_name == "Kyler Murray" ~ "MIN",
      TRUE ~ team
    ), na.rm = TRUE)
  )

qb_weekly_ranks <- qb_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  select(player_id, player_display_name, week, rank) |>
  mutate(category = case_when(
    rank <= 12 ~ "QB1",
    rank <= 24 ~ "QB2",
    TRUE ~ "QB3+"
  ))

qb_player_categories <- qb_weekly_ranks |>
  group_by(player_id, player_display_name) |>
  summarise(
    total_games = n(),
    qb1_games = sum(category == "QB1"),
    qb2_games = sum(category == "QB2"),
    qb3_games = sum(category == "QB3+"),
    qb1_pct = round(qb1_games / total_games * 100, 1),
    qb2_pct = round(qb2_games / total_games * 100, 1),
    qb3_pct = round(qb3_games / total_games * 100, 1),
    .groups = "drop"
  )

qb_final_stats <- qb_player_stats |>
  left_join(qb_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(rank, player_display_name, team, ppr_per_game, qb1_pct, qb2_pct, qb3_pct)

# ------------------------------------------------------------------
# Save all objects
# ------------------------------------------------------------------
saveRDS(all_stats,        "data/all_stats.rds")
saveRDS(raw_schedules,    "data/raw_schedules.rds")
saveRDS(team_colors_db,   "data/team_colors_db.rds")
saveRDS(rb_final_stats,   "data/rb_final_stats.rds")
saveRDS(wr_final_stats,   "data/wr_final_stats.rds")
saveRDS(te_final_stats,   "data/te_final_stats.rds")
saveRDS(qb_final_stats,   "data/qb_final_stats.rds")

writeLines(as.character(Sys.time()), "data/data_version.txt")

message("Data preparation complete. Files written to data/.")
message("You may now deploy the application folder to Posit Connect.")