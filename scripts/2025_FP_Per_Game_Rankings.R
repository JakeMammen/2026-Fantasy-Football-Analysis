library(nflreadr)
library(dplyr)
library(gt)
library(nflfastR)

# Load 2025 regular season player stats (includes fantasy_points_ppr)
stats <- load_player_stats(seasons = 2025) |>
  filter(position == "RB", season_type == "REG")

# Compute weekly RB12 and RB24 thresholds (12th and 24th highest PPR among RBs each week)
weekly_thresholds <- stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  filter(rank %in% c(12, 24)) |>
  select(week, rank, fantasy_points_ppr) |>
  tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "RB") |>
  arrange(week)

# Aggregate player stats: PPR points per game, active games
player_stats <- stats |>
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
  ) |>
  mutate(
    total_games = 9,  # Standard NFL regular season is 17 games in 2024
  )

# Calculate weekly ranks and categorize performance as RB1, RB2, or RB3+
weekly_ranks <- stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  select(player_id, player_display_name, week, rank) |>
  mutate(category = case_when(
    rank <= 12 ~ "RB1",
    rank <= 24 ~ "RB2",
    TRUE ~ "RB3+"
  ))

# Compute RB1, RB2, RB3+ percentages for each player
player_categories <- weekly_ranks |>
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

# Join player stats with category percentages and filter for players with at least 5 games
final_stats <- player_stats |>
  left_join(player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(
    rank,
    player_display_name,
    team,
    ppr_per_game,
    rb1_pct,
    rb2_pct,
    rb3_pct,
  )

# Load team logos
team_logos <- teams_colors_logos |>
  select(team_abbr, team_logo_espn) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))  # Align with stats data

# Join team logos and create the gt table
gt_table <- final_stats |>
  left_join(team_logos, by = c("team" = "team_abbr")) |>
  select(rank, player_display_name, team_logo_espn, ppr_per_game, rb1_pct, rb2_pct, rb3_pct) |>
  gt() |>
  cols_label(
    rank = "Rank",
    player_display_name = "Player",
    team_logo_espn = "Team",
    ppr_per_game = "PPR/Game",
    rb1_pct = "RB1 %",
    rb2_pct = "RB2 %",
    rb3_pct = "RB3+ %",
  ) |>
  fmt_number(
    columns = c(ppr_per_game, rb1_pct, rb2_pct, rb3_pct),
    decimals = 1
  ) |>
  text_transform(
    locations = cells_body(columns = team_logo_espn),
    fn = function(x) {
      web_image(url = x, height = 30)
    }
  ) |>
  # Apply conditional coloring to RB1, RB2, RB3+ percentages
  tab_style(
    style = cell_fill(color = "#31a354"),
    locations = cells_body(
      columns = rb1_pct,
      rows = rb1_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#636363"),
    locations = cells_body(
      columns = rb2_pct,
      rows = rb2_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#e41a1c"),
    locations = cells_body(
      columns = rb3_pct,
      rows = rb3_pct > 0
    )
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md('**2025 Fantasy Football PPR Points Per Game Rankings**'),
    subtitle = "Top 24 Running Backs (Min. 4 games played)"
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
    table.font.color = 'black',
    table_body.border.top.color = 'black',
    row_group.border.top.color = '#999999',
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.color = 'black',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    source_notes.background.color = '#F2F2F2',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>%
  tab_source_note(
    source_note = html(
      paste0(
        "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
         <div style='font-size: 12px;'>
           <b>Data:</b> nflfastR | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height   = 30
        ),
        "</div>"
      )
    )
  )

# Display the table
gt_table

gtsave(gt_table, 
       filename = "output/tables/2025_ppr_per_game_RB.png")

# Load 2025 regular season player stats (includes fantasy_points_ppr)
wr_stats <- load_player_stats(seasons = 2025) |>
  filter(position == "WR", season_type == "REG")

# Compute weekly RB12 and RB24 thresholds (12th and 24th highest PPR among RBs each week)
wr_weekly_thresholds <- wr_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  filter(rank %in% c(12, 24)) |>
  select(week, rank, fantasy_points_ppr) |>
  tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "WR") |>
  arrange(week)

# Aggregate player stats: PPR points per game, active games
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
  mutate(
    total_games = 9,  # Standard NFL regular season is 17 games in 2024
  ) |>
  filter(player_display_name != "Tyreek Hill")

# Calculate weekly ranks and categorize performance as RB1, RB2, or RB3+
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

# Compute RB1, RB2, RB3+ percentages for each player
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

# Join player stats with category percentages and filter for players with at least 5 games
wr_final_stats <- wr_player_stats |>
  left_join(wr_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(
    rank,
    player_display_name,
    team,
    ppr_per_game,
    wr1_pct,
    wr2_pct,
    wr3_pct,
  )

# Load team logos
team_logos <- teams_colors_logos |>
  select(team_abbr, team_logo_espn) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))  # Align with stats data

# Join team logos and create the gt table
wr_gt_table <- wr_final_stats |>
  left_join(team_logos, by = c("team" = "team_abbr")) |>
  select(rank, player_display_name, team_logo_espn, ppr_per_game, wr1_pct, wr2_pct, wr3_pct) |>
  gt() |>
  cols_label(
    rank = "Rank",
    player_display_name = "Player",
    team_logo_espn = "Team",
    ppr_per_game = "PPR/Game",
    wr1_pct = "WR1 %",
    wr2_pct = "WR2 %",
    wr3_pct = "WR3+ %",
  ) |>
  fmt_number(
    columns = c(ppr_per_game, wr1_pct, wr2_pct, wr3_pct),
    decimals = 1
  ) |>
  text_transform(
    locations = cells_body(columns = team_logo_espn),
    fn = function(x) {
      web_image(url = x, height = 30)
    }
  ) |>
  # Apply conditional coloring to RB1, RB2, RB3+ percentages
  tab_style(
    style = cell_fill(color = "#31a354"),
    locations = cells_body(
      columns = wr1_pct,
      rows = wr1_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#636363"),
    locations = cells_body(
      columns = wr2_pct,
      rows = wr2_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#e41a1c"),
    locations = cells_body(
      columns = wr3_pct,
      rows = wr3_pct > 0
    )
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = md('**2025 Fantasy Football PPR Points Per Game Rankings**'),
    subtitle = "Top 24 Receivers (Min. 4 games played)"
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
    table.font.color = 'black',
    table_body.border.top.color = 'black',
    row_group.border.top.color = '#999999',
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.color = 'black',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    source_notes.background.color = '#F2F2F2',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>%
  tab_source_note(
    source_note = html(
      paste0(
        "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
         <div style='font-size: 12px;'>
           <b>Data:</b> nflfastR | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height   = 30
        ),
        "</div>"
      )
    )
  )

# Display the table
wr_gt_table

gtsave(wr_gt_table, 
       filename = "output/tables/2025_ppr_per_game_WR.png")

# Load 2025 regular season player stats (includes fantasy_points_ppr)
te_stats <- load_player_stats(seasons = 2025) |>
  filter(position == "TE", season_type == "REG")

# Compute weekly RB12 and RB24 thresholds (12th and 24th highest PPR among RBs each week)
te_weekly_thresholds <- te_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  filter(rank %in% c(12, 24)) |>
  select(week, rank, fantasy_points_ppr) |>
  tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "TE") |>
  arrange(week)

# Aggregate player stats: PPR points per game, active games
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
  mutate(
    total_games = 9,  # Standard NFL regular season is 17 games in 2024
  ) |>
  filter(player_display_name != "Darren Waller" & player_display_name != "Zach Ertz")

# Calculate weekly ranks and categorize performance as RB1, RB2, or RB3+
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

# Compute RB1, RB2, RB3+ percentages for each player
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

# Join player stats with category percentages and filter for players with at least 5 games
te_final_stats <- te_player_stats |>
  left_join(te_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(
    rank,
    player_display_name,
    team,
    ppr_per_game,
    te1_pct,
    te2_pct,
    te3_pct,
  )

# Load team logos
team_logos <- teams_colors_logos |>
  select(team_abbr, team_logo_espn) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))  # Align with stats data

# Join team logos and create the gt table
te_gt_table <- te_final_stats |>
  left_join(team_logos, by = c("team" = "team_abbr")) |>
  select(rank, player_display_name, team_logo_espn, ppr_per_game, te1_pct, te2_pct, te3_pct) |>
  gt() |>
  cols_label(
    rank = "Rank",
    player_display_name = "Player",
    team_logo_espn = "Team",
    ppr_per_game = "PPR/Game",
    te1_pct = "TE1 %",
    te2_pct = "TE2 %",
    te3_pct = "TE3+ %",
  ) |>
  fmt_number(
    columns = c(ppr_per_game, te1_pct, te2_pct, te3_pct),
    decimals = 1
  ) |>
  text_transform(
    locations = cells_body(columns = team_logo_espn),
    fn = function(x) {
      web_image(url = x, height = 30)
    }
  ) |>
  # Apply conditional coloring to RB1, RB2, RB3+ percentages
  tab_style(
    style = cell_fill(color = "#31a354"),
    locations = cells_body(
      columns = te1_pct,
      rows = te1_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#636363"),
    locations = cells_body(
      columns = te2_pct,
      rows = te2_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#e41a1c"),
    locations = cells_body(
      columns = te3_pct,
      rows = te3_pct > 0
    )
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = "2025 Fantasy Football PPR Points Per Game Rankings",
    subtitle = "Top 24 Tight Ends (Min. 4 games played)"
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
    table.font.color = 'black',
    table_body.border.top.color = 'black',
    row_group.border.top.color = '#999999',
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.color = 'black',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    source_notes.background.color = '#F2F2F2',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>%
  tab_source_note(
    source_note = html(
      paste0(
        "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
         <div style='font-size: 12px;'>
           <b>Data:</b> nflfastR | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height   = 30
        ),
        "</div>"
      )
    )
  )

# Display the table
te_gt_table

gtsave(te_gt_table, 
       filename = "output/tables/2025_ppr_per_game_TE.png")

# Load 2025 regular season player stats (includes fantasy_points_ppr)
qb_stats <- load_player_stats(seasons = 2025) |>
  filter(position == "QB", season_type == "REG")

# Compute weekly RB12 and RB24 thresholds (12th and 24th highest PPR among RBs each week)
qb_weekly_thresholds <- qb_stats |>
  group_by(week) |>
  arrange(desc(fantasy_points_ppr)) |>
  mutate(rank = row_number()) |>
  filter(rank %in% c(12, 24)) |>
  select(week, rank, fantasy_points_ppr) |>
  tidyr::pivot_wider(names_from = rank, values_from = fantasy_points_ppr, names_prefix = "QB") |>
  arrange(week)

# Aggregate player stats: PPR points per game, active games
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
  ) |>
  mutate(
    total_games = 9,  # Standard NFL regular season is 17 games in 2024
  )

# Calculate weekly ranks and categorize performance as RB1, RB2, or RB3+
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

# Compute RB1, RB2, RB3+ percentages for each player
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

# Join player stats with category percentages and filter for players with at least 5 games
qb_final_stats <- qb_player_stats |>
  left_join(qb_player_categories, by = c("player_id", "player_display_name")) |>
  arrange(desc(ppr_per_game)) |>
  filter(active_games >= 4) |>
  mutate(rank = row_number()) |>
  filter(rank <= 24) |>
  select(
    rank,
    player_display_name,
    team,
    ppr_per_game,
    qb1_pct,
    qb2_pct,
    qb3_pct,
  )

# Load team logos
team_logos <- teams_colors_logos |>
  select(team_abbr, team_logo_espn) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))  # Align with stats data

# Join team logos and create the gt table
qb_gt_table <- qb_final_stats |>
  left_join(team_logos, by = c("team" = "team_abbr")) |>
  select(rank, player_display_name, team_logo_espn, ppr_per_game, qb1_pct, qb2_pct, qb3_pct) |>
  gt() |>
  cols_label(
    rank = "Rank",
    player_display_name = "Player",
    team_logo_espn = "Team",
    ppr_per_game = "PPR/Game",
    qb1_pct = "QB1 %",
    qb2_pct = "QB2 %",
    qb3_pct = "QB3+ %",
  ) |>
  fmt_number(
    columns = c(ppr_per_game, qb1_pct, qb2_pct, qb3_pct),
    decimals = 1
  ) |>
  text_transform(
    locations = cells_body(columns = team_logo_espn),
    fn = function(x) {
      web_image(url = x, height = 30)
    }
  ) |>
  # Apply conditional coloring to RB1, RB2, RB3+ percentages
  tab_style(
    style = cell_fill(color = "#31a354"),
    locations = cells_body(
      columns = qb1_pct,
      rows = qb1_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#636363"),
    locations = cells_body(
      columns = qb2_pct,
      rows = qb2_pct > 0
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#e41a1c"),
    locations = cells_body(
      columns = qb3_pct,
      rows = qb3_pct > 0
    )
  ) |>
  cols_align(
    align = "center",
    columns = everything()
  ) |>
  tab_header(
    title = "2025 Fantasy Football PPR Points Per Game Rankings",
    subtitle = "Top 24 Quarterbacks (Min. 4 games played)"
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
    table.font.color = 'black',
    table_body.border.top.color = 'black',
    row_group.border.top.color = '#999999',
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.color = 'black',
    table.border.top.color = 'transparent',
    table.background.color = '#F2F2F2',
    table.border.bottom.color = 'transparent',
    source_notes.background.color = '#F2F2F2',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>%
  tab_source_note(
    source_note = html(
      paste0(
        "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
         <div style='font-size: 12px;'>
           <b>Data:</b> nflfastR | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height   = 30
        ),
        "</div>"
      )
    )
  )

# Display the table
qb_gt_table

gtsave(qb_gt_table, 
       filename = "output/tables/2025_ppr_per_game_QB.png")