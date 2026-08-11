library(nflfastR)
library(nflreadr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)
library(progress)
library(scales)
library(webshot2)

# Load player stats for 2025 regular season
pbp <- nflreadr::load_player_stats(seasons = 2025) %>%
  filter(season_type == "REG", week >= 1, week <= 18)

# Load roster data to get player positions
roster <- nflreadr::load_rosters(2025) %>%
  dplyr::select(gsis_id, position, full_name) %>%
  distinct()

# Process passing/receiving stats
pass_df <- pbp %>%
  filter(position %in% c("WR")) %>% # Filter for WRs early
  dplyr::select(
    season, week, team, player_id, player_name,
    position, receptions, targets, passing_yards, passing_tds, receiving_yards, receiving_tds,
    receiving_fumbles_lost, receiving_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0037240" ~ "J.Williams",
      player_id == "00-0036278" ~ "J.Jefferson",
      player_id == "00-0036328" ~ "J.Chase",
      player_id == "00-0031381" ~ "D.Adams",
      player_id == "00-0038117" ~ "W.Robinson",
      player_id == "00-0031588" ~ "S.Diggs",
      player_id == "00-0036252" ~ "M.Pittman",
      player_id == "00-0036613" ~ "J.Waddle",
      player_id == "00-0035719" ~ "D.Samuel",
      player_id == "00-0034960" ~ "J.Meyers",
      player_id == "00-0037816" ~ "R.Doubs",
      player_id == "00-0035662" ~ "M.Brown",
      player_id == "00-0038393" ~ "D.Wicks",
      player_id == "00-0038559" ~ "Mi.Wilson",
      player_id == "00-0034775" ~ "C.Kirk",
      player_id == "00-0036415" ~ "V.Jefferson",
      player_id == "00-0032211" ~ "T.Lockett",
      player_id == "00-0032464" ~ "C.Raymond",
      player_id == "00-0033857" ~ "J.Smith-Schuster",
      player_id == "00-0031408" ~ "M.Evans",
      player_id == "00-0036259" ~ "J.Jennings",
      player_id == "00-0034827" ~ "D.Moore",
      TRUE ~ player_name
    ),
    team = case_when(
      player_id == "00-0031381" ~ "LAR", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0038117" ~ "TEN",
      player_id == "00-0031588" ~ "WAS",
      player_id == "00-0036252" ~ "PIT",
      player_id == "00-0036613" ~ "DEN",
      player_id == "00-0035719" ~ "SF",
      player_id == "00-0034960" ~ "JAX",
      player_id == "00-0037816" ~ "NE",
      player_id == "00-0035662" ~ "PHI",
      player_id == "00-0038393" ~ "PHI",
      player_id == "00-0038559" ~ "ARI",
      player_id == "00-0034775" ~ "SF",
      player_id == "00-0036415" ~ "WAS",
      player_id == "00-0032211" ~ "LV",
      player_id == "00-0032464" ~ "CHI",
      player_id == "00-0033857" ~ "NYG",
      player_id == "00-0031408" ~ "SF",
      player_id == "00-0036259" ~ "MIN",
      player_id == "00-0034827" ~ "BUF",
      TRUE ~ team
    ),
    fumble_penalty = ifelse(receiving_fumbles_lost >= 1, -2 * receiving_fumbles_lost, 0),
    PPR_points = receptions + (receiving_yards / 10) + (receiving_tds * 6) + (passing_tds * 4) + (passing_yards / 25) +
      (receiving_2pt_conversions * 2) + fumble_penalty,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup() %>%
  filter(player_name != "D.Hopkins" & player_name != "K.Allen")

# Process rushing stats for receivers
rush_df <- pbp %>%
  filter(position %in% c("WR"), rushing_yards > 0 | rushing_tds > 0) %>%
  dplyr::select(
    season, week, team, player_id, player_name,
    position, rushing_yards, rushing_tds, rushing_fumbles_lost, rushing_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0037240" ~ "J.Williams",
      player_id == "00-0038117" ~ "W.Robinson",
      player_id == "00-0031588" ~ "S.Diggs",
      player_id == "00-0036252" ~ "M.Pittman",
      player_id == "00-0036613" ~ "J.Waddle",
      player_id == "00-0035719" ~ "D.Samuel",
      player_id == "00-0034960" ~ "J.Meyers",
      player_id == "00-0033589" ~ "D.Moore",
      player_id == "00-0037816" ~ "R.Doubs",
      player_id == "00-0035662" ~ "M.Brown",
      player_id == "00-0038393" ~ "D.Wicks",
      player_id == "00-0038559" ~ "Mi.Wilson",
      player_id == "00-0034775" ~ "C.Kirk",
      player_id == "00-0036415" ~ "V.Jefferson",
      player_id == "00-0032211" ~ "T.Lockett",
      player_id == "00-0032464" ~ "C.Raymond",
      player_id == "00-0033857" ~ "J.Smith-Schuster",
      player_id == "00-0031408" ~ "M.Evans",
      player_id == "00-0036259" ~ "J.Jennings",
      player_id == "00-0034827" ~ "D.Moore",
      TRUE ~ player_name
    ),
    team = case_when(
      player_id == "00-0031381" ~ "LAR", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0038117" ~ "TEN",
      player_id == "00-0031588" ~ "WAS",
      player_id == "00-0036252" ~ "PIT",
      player_id == "00-0036613" ~ "DEN",
      player_id == "00-0035719" ~ "SF",
      player_id == "00-0034960" ~ "JAX",
      player_id == "00-0033589" ~ "BUF",
      player_id == "00-0037816" ~ "NE",
      player_id == "00-0035662" ~ "PHI",
      player_id == "00-0038393" ~ "PHI",
      player_id == "00-0038559" ~ "ARI",
      player_id == "00-0034775" ~ "SF",
      player_id == "00-0036415" ~ "WAS",
      player_id == "00-0032211" ~ "LV",
      player_id == "00-0032464" ~ "CHI",
      player_id == "00-0033857" ~ "NYG",
      player_id == "00-0031408" ~ "SF",
      player_id == "00-0036259" ~ "MIN",
      player_id == "00-0034827" ~ "BUF",
      TRUE ~ team
    ),
    fumble_penalty = ifelse(rushing_fumbles_lost >= 1, -2 * rushing_fumbles_lost, 0),
    PPR_points = (rushing_yards / 10) + (rushing_tds * 6) +
      (rushing_2pt_conversions * 2) + fumble_penalty,
    rush_attempt = 1,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Load expected fantasy points for the 2025 season, weeks 1-18
ff_opportunity <- load_ff_opportunity(seasons = 2025, stat_type = "weekly", model_version = "latest")

# Filter for wide receivers (WR) and select relevant columns
receivers <- ff_opportunity %>%
  filter(position == "WR") %>%
  dplyr::select(
    player_id,
    full_name,
    week,
    receptions_exp,
    rec_yards_gained_exp,
    rush_yards_gained_exp,
    rec_touchdown_exp,
    rush_touchdown_exp,
    pass_fantasy_points_exp,
    rush_fantasy_points_exp,
    rec_fantasy_points_exp,
    posteam
  )

# Aggregate expected points by receiver across weeks 1-18
receiver_totals <- receivers %>%
  filter(week %in% 1:18) %>%
  group_by(player_id, full_name) %>%
  summarise(
    games_played = n(),
    rec_exp = sum(receptions_exp, na.rm = TRUE),
    yds_exp = sum(rec_yards_gained_exp, rush_yards_gained_exp, na.rm = TRUE),
    tds_exp = sum(rec_touchdown_exp, rush_touchdown_exp, na.rm = TRUE),
    total_expected_points = sum(pass_fantasy_points_exp, rush_fantasy_points_exp, rec_fantasy_points_exp, na.rm = TRUE)
  ) %>%
  arrange(desc(total_expected_points))

# Combine passing, rushing, and expected data
avg_fp_df <- pass_df %>%
  group_by(player_name, position, player_id) %>%
  summarize(
    team = max(case_when(
      player_id == "00-0031381" ~ "LAR", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0038117" ~ "TEN",
      player_id == "00-0031588" ~ "WAS",
      player_id == "00-0036252" ~ "PIT",
      player_id == "00-0036613" ~ "DEN",
      player_id == "00-0035719" ~ "SF",
      player_id == "00-0034960" ~ "JAX",
      player_id == "00-0033589" ~ "BUF",
      player_id == "00-0037816" ~ "NE",
      player_id == "00-0035662" ~ "PHI",
      player_id == "00-0038393" ~ "PHI",
      player_id == "00-0038559" ~ "ARI",
      player_id == "00-0034775" ~ "SF",
      player_id == "00-0036415" ~ "WAS",
      player_id == "00-0032211" ~ "LV",
      player_id == "00-0032464" ~ "CHI",
      player_id == "00-0033857" ~ "NYG",
      player_id == "00-0031408" ~ "SF",
      player_id == "00-0036259" ~ "MIN",
      player_id == "00-0034827" ~ "BUF",
      TRUE ~ team
    ), na.rm = TRUE),
    games_pass = sum(game_played, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    catches = sum(receptions, na.rm = TRUE),
    pass_yards = sum(receiving_yards, na.rm = TRUE),
    pass_td = sum(receiving_tds, na.rm = TRUE),
    pass_fumbles = sum(receiving_fumbles_lost, na.rm = TRUE),
    pass_PPR_pts = sum(PPR_points, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    rush_df %>%
      group_by(player_name, position, player_id) %>%
      summarize(
        team = max(case_when(
          player_id == "00-0031381" ~ "LAR", # D.Adams
          player_id == "00-0037240" ~ "DET", # J.Williams
          player_id == "00-0038117" ~ "TEN",
          player_id == "00-0031588" ~ "WAS",
          player_id == "00-0036252" ~ "PIT",
          player_id == "00-0036613" ~ "DEN",
          player_id == "00-0035719" ~ "SF",
          player_id == "00-0034960" ~ "JAX",
          player_id == "00-0033589" ~ "BUF",
          player_id == "00-0037816" ~ "NE",
          player_id == "00-0035662" ~ "PHI",
          player_id == "00-0038393" ~ "PHI",
          player_id == "00-0038559" ~ "ARI",
          player_id == "00-0034775" ~ "SF",
          player_id == "00-0036415" ~ "WAS",
          player_id == "00-0032211" ~ "LV",
          player_id == "00-0032464" ~ "CHI",
          player_id == "00-0033857" ~ "NYG",
          player_id == "00-0031408" ~ "SF",
          player_id == "00-0036259" ~ "MIN",
          player_id == "00-0034827" ~ "BUF",
          TRUE ~ team
        ), na.rm = TRUE),
        games_rush = sum(game_played, na.rm = TRUE),
        rush_attempts = sum(rush_attempt, na.rm = TRUE),
        rush_yards = sum(rushing_yards, na.rm = TRUE),
        rush_td = sum(rushing_tds, na.rm = TRUE),
        rush_fumbles = sum(rushing_fumbles_lost, na.rm = TRUE),
        rush_PPR_pts = sum(PPR_points, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = c("team", "player_name", "position", "player_id")
  ) %>%
  left_join(
    receiver_totals %>%
      dplyr::select(player_id, rec_exp, yds_exp, tds_exp, total_expected_points),
    by = c("player_id")
  ) %>%
  mutate(
    games = pmax(games_pass, games_rush, na.rm = TRUE),
    rush_attempts = coalesce(rush_attempts, 0),
    rush_yards = coalesce(rush_yards, 0),
    rush_td = coalesce(rush_td, 0),
    rush_fumbles = coalesce(rush_fumbles, 0),
    rush_PPR_pts = coalesce(rush_PPR_pts, 0),
    PPR_pts = pass_PPR_pts + rush_PPR_pts,
    yards = pass_yards + rush_yards,
    td = pass_td + rush_td,
    fumbles = pass_fumbles + rush_fumbles,
    rec_exp = coalesce(rec_exp, 0),
    yds_exp = coalesce(yds_exp, 0),
    tds_exp = coalesce(tds_exp, 0),
    total_expected_points = coalesce(total_expected_points, 0),
    forp = PPR_pts - total_expected_points
  ) %>%
  filter(position == "WR", games >= 8) %>%
  dplyr::select(team, player_name, games, targets, catches, yards, td, fumbles, PPR_pts, rec_exp, yds_exp, tds_exp, total_expected_points, forp)

# Add team colors and create colored abbreviations
team_info <- teams_colors_logos %>%
  select(team_abbr, team_color) %>%
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

avg_fp_df <- avg_fp_df %>%
  left_join(team_info, by = c("team" = "team_abbr")) %>%
  mutate(
    team = paste0(
      "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
    )
  ) %>%
  select(-team_color)

# Create the gt table
fp_WR2025 <- avg_fp_df %>%
  arrange(-PPR_pts) %>%
  dplyr::slice(1:100) %>%
  mutate(Rank = row_number()) %>%
  gt() %>%
  tab_header(title = md('**2025 Actual vs. Expected PPR Fantasy Points Receivers**')) %>%
  cols_move_to_start(columns = vars(Rank)) %>%
  cols_label(
    games = 'GP',
    player_name = '',
    team = 'Team',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    fumbles = 'Fum',
    PPR_pts = 'FP',
    rec_exp = 'Rec',
    yds_exp = 'Yds',
    tds_exp = 'TD',
    total_expected_points = 'FP',
    forp = 'FORP'
  ) %>%
  fmt_number(columns = vars(PPR_pts, total_expected_points), decimals = 1) %>%
  fmt_number(columns = vars(yards, catches, yds_exp, rec_exp, tds_exp), decimals = 0, sep_mark = ',') %>%
  fmt_markdown(columns = vars(team)) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_body(vars(player_name))) %>%
  tab_spanner(label = md('**Actual**'), columns = vars(catches, yards, td, fumbles, PPR_pts)) %>%
  tab_spanner(label = md('**Expected**'), columns = vars(rec_exp, yds_exp, tds_exp, total_expected_points)) %>%
  data_color(
    columns = vars(PPR_pts, total_expected_points),
    colors = scales::col_numeric(palette = c('grey97', '#E03FD8'), domain = c(40, 378)),
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(forp),
    colors = scales::col_numeric(palette = c('#FF4040', '#FFFFFF', '#40C040'), domain = c(-50, 0, 72)),
    autocolor_text = FALSE
  ) %>%
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
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
           <b>Data:</b> nflreadr | <b>Credit:</b> Anthony Reinhard, 2020, Open Source Football | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height = 30
        ),
        "</div>"
      )
    )
  )

fp_WR2025

gtsave(fp_WR2025, 
       filename = "output/tables/exp_fp_WR2025.png")

###################################################################################################################################################

# Process passing/receiving stats
passTE_df <- pbp %>%
  filter(position %in% c("TE")) %>% # Filter for TEs early
  dplyr::select(
    season, week, team, player_id, player_name,
    position, receptions, targets, passing_yards, passing_tds, receiving_yards, receiving_tds,
    receiving_fumbles_lost, receiving_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0037240" ~ "J.Williams",
      player_id == "00-0036278" ~ "J.Jefferson",
      player_id == "00-0036328" ~ "J.Chase",
      player_id == "00-0031381" ~ "D.Adams",
      player_id == "00-0037809" ~ "C.Okonkwo",
      player_id == "00-0033885" ~ "D.Njoku",
      player_id == "00-0037838" ~ "I.Likely",
      player_id == "00-0038046" ~ "C.Kolar",
      player_id == "00-0038720" ~ "J.Hill",
      TRUE ~ player_name
    ),
    team = case_when(
      player_id == "00-0031381" ~ "NYJ", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0037809" ~ "WAS",
      player_id == "00-0033885" ~ "LAC",
      player_id == "00-0037838" ~ "NYG",
      player_id == "00-0038046" ~ "LAC",
      player_id == "00-0038720" ~ "NE",
      TRUE ~ team
    ),
    fumble_penalty = ifelse(receiving_fumbles_lost >= 1, -2 * receiving_fumbles_lost, 0),
    PPR_points = receptions + (receiving_yards / 10) + (receiving_tds * 6) + (passing_tds * 4) + (passing_yards / 25) +
      (receiving_2pt_conversions * 2) + fumble_penalty,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup() %>%
  filter(player_name != "Z.Ertz" & player_name != "D.Waller" & player_name != "J.Smith" & player_name != "T.Hill")

# Process rushing stats for receivers
rushTE_df <- pbp %>%
  filter(position %in% c("TE"), rushing_yards > 0 | rushing_tds > 0 | rushing_fumbles_lost == 1) %>%
  dplyr::select(
    season, week, team, player_id, player_name,
    position, rushing_yards, rushing_tds, rushing_fumbles_lost, rushing_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0037240" ~ "J.Williams",
      player_id == "00-0037809" ~ "C.Okonkwo",
      player_id == "00-0033885" ~ "D.Njoku",
      player_id == "00-0037838" ~ "I.Likely",
      player_id == "00-0038046" ~ "C.Kolar",
      player_id == "00-0038720" ~ "J.Hill",
      TRUE ~ player_name
    ),
    team = case_when(
      player_id == "00-0031381" ~ "NYJ", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0033885" ~ "LAC",
      player_id == "00-0037838" ~ "NYG",
      player_id == "00-0038046" ~ "LAC",
      player_id == "00-0038720" ~ "NE",
      TRUE ~ team
    ),
    fumble_penalty = ifelse(rushing_fumbles_lost >= 1, -2 * rushing_fumbles_lost, 0),
    PPR_points = (rushing_yards / 10) + (rushing_tds * 6) +
      (rushing_2pt_conversions * 2) + fumble_penalty,
    rush_attempt = 1,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Load expected fantasy points for the 2025 season, weeks 1-18
ff_opportunity <- load_ff_opportunity(seasons = 2025, stat_type = "weekly", model_version = "latest")

# Filter for tight ends (TE) and select relevant columns
tightends <- ff_opportunity %>%
  filter(position == "TE") %>%
  dplyr::select(
    player_id,
    full_name,
    week,
    receptions_exp,
    rec_yards_gained_exp,
    rush_yards_gained_exp,
    rec_touchdown_exp,
    rush_touchdown_exp,
    pass_fantasy_points_exp,
    rush_fantasy_points_exp,
    rec_fantasy_points_exp,
    posteam
  )

# Aggregate expected points by tight end across weeks 1-18
tightends_totals <- tightends %>%
  filter(week %in% 1:18) %>%
  group_by(player_id, full_name) %>%
  summarise(
    games_played = n(),
    rec_exp = sum(receptions_exp, na.rm = TRUE),
    yds_exp = sum(rec_yards_gained_exp, rush_yards_gained_exp, na.rm = TRUE),
    tds_exp = sum(rec_touchdown_exp, rush_touchdown_exp, na.rm = TRUE),
    total_expected_points = sum(pass_fantasy_points_exp, rush_fantasy_points_exp, rec_fantasy_points_exp, na.rm = TRUE)
  ) %>%
  arrange(desc(total_expected_points))

# Combine passing, rushing, and expected data
avg_fpTE_df <- passTE_df %>%
  group_by(player_name, position, player_id) %>%
  summarize(
    team = max(case_when(
      player_id == "00-0031381" ~ "NYJ", # D.Adams
      player_id == "00-0037240" ~ "DET", # J.Williams
      player_id == "00-0033885" ~ "LAC",
      player_id == "00-0037838" ~ "NYG",
      player_id == "00-0038046" ~ "LAC",
      player_id == "00-0038720" ~ "NE",
      TRUE ~ team
    ), na.rm = TRUE),
    games_pass = sum(game_played, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    catches = sum(receptions, na.rm = TRUE),
    pass_yards = sum(receiving_yards, na.rm = TRUE),
    pass_td = sum(receiving_tds, na.rm = TRUE),
    pass_fumbles = sum(receiving_fumbles_lost, na.rm = TRUE),
    pass_PPR_pts = sum(PPR_points, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    rushTE_df %>%
      group_by(player_name, position, player_id) %>%
      summarize(
        team = max(case_when(
          player_id == "00-0031381" ~ "NYJ", # D.Adams
          player_id == "00-0037240" ~ "DET", # J.Williams
          player_id == "00-0033885" ~ "LAC",
          player_id == "00-0037838" ~ "NYG",
          player_id == "00-0038046" ~ "LAC",
          player_id == "00-0038720" ~ "NE",
          TRUE ~ team
        ), na.rm = TRUE),
        games_rush = sum(game_played, na.rm = TRUE),
        rush_attempts = sum(rush_attempt, na.rm = TRUE),
        rush_yards = sum(rushing_yards, na.rm = TRUE),
        rush_td = sum(rushing_tds, na.rm = TRUE),
        rush_fumbles = sum(rushing_fumbles_lost, na.rm = TRUE),
        rush_PPR_pts = sum(PPR_points, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = c("team", "player_name", "position", "player_id")
  ) %>%
  left_join(
    tightends_totals %>%
      dplyr::select(player_id, rec_exp, yds_exp, tds_exp, total_expected_points),
    by = c("player_id")
  ) %>%
  mutate(
    games = pmax(games_pass, games_rush, na.rm = TRUE),
    rush_attempts = coalesce(rush_attempts, 0),
    rush_yards = coalesce(rush_yards, 0),
    rush_td = coalesce(rush_td, 0),
    rush_fumbles = coalesce(rush_fumbles, 0),
    rush_PPR_pts = coalesce(rush_PPR_pts, 0),
    PPR_pts = pass_PPR_pts + rush_PPR_pts,
    yards = pass_yards + rush_yards,
    td = pass_td + rush_td,
    fumbles = pass_fumbles + rush_fumbles,
    rec_exp = coalesce(rec_exp, 0),
    yds_exp = coalesce(yds_exp, 0),
    tds_exp = coalesce(tds_exp, 0),
    total_expected_points = coalesce(total_expected_points, 0),
    forp = PPR_pts - total_expected_points
  ) %>%
  filter(position == "TE", games >= 8) %>%
  dplyr::select(team, player_name, games, targets, catches, yards, td, fumbles, PPR_pts, rec_exp, yds_exp, tds_exp, total_expected_points, forp)

# Add team colors and create colored abbreviations
team_info <- teams_colors_logos %>%
  select(team_abbr, team_color) %>%
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

avg_fpTE_df <- avg_fpTE_df %>%
  left_join(team_info, by = c("team" = "team_abbr")) %>%
  mutate(
    team = paste0(
      "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
    )
  ) %>%
  select(-team_color)

# Create the gt table
fp_TE2025 <- avg_fpTE_df %>%
  arrange(-PPR_pts) %>%
  dplyr::slice(1:60) %>%
  mutate(Rank = row_number()) %>%
  gt() %>%
  tab_header(title = md('**2025 Actual vs. Expected PPR Fantasy Points Tight Ends**')) %>%
  cols_move_to_start(columns = vars(Rank)) %>%
  cols_label(
    games = 'GP',
    player_name = '',
    team = 'Team',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    fumbles = 'Fum',
    PPR_pts = 'FP',
    rec_exp = 'Rec',
    yds_exp = 'Yds',
    tds_exp = 'TD',
    total_expected_points = 'FP',
    forp = 'FORP'
  ) %>%
  fmt_number(columns = vars(PPR_pts, total_expected_points), decimals = 1) %>%
  fmt_number(columns = vars(yards, catches, yds_exp, rec_exp, tds_exp), decimals = 0, sep_mark = ',') %>%
  fmt_markdown(columns = vars(team)) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_body(vars(player_name))) %>%
  tab_spanner(label = md('**Actual**'), columns = vars(catches, yards, td, fumbles, PPR_pts)) %>%
  tab_spanner(label = md('**Expected**'), columns = vars(rec_exp, yds_exp, tds_exp, total_expected_points)) %>%
  data_color(
    columns = vars(PPR_pts, total_expected_points),
    colors = scales::col_numeric(palette = c('grey97', '#E03FD8'), domain = c(0, 320)),
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(forp),
    colors = scales::col_numeric(palette = c('#FF4040', '#FFFFFF', '#40C040'), domain = c(-26, 0, 47)),
    autocolor_text = FALSE
  ) %>%
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
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
           <b>Data:</b> nflreadr | <b>Credit:</b> Anthony Reinhard, 2020, Open Source Football | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height = 30
        ),
        "</div>"
      )
    )
  )

fp_TE2025

gtsave(fp_TE2025,
       filename = "output/tables/exp_fp_TE2025.png")

###################################################################################################################################################

#############################################
# 2025 NFL WRs: Actual vs Expected Touchdowns
#############################################
library(nflfastR)
library(ggimage)
library(ggrepel)
library(stringr)
library(ggtext)

# Load play-by-play data
pbp <- load_pbp(2025) |>
  filter(week >= 1 & week <= 18)

avg_exp_fpWR_df <- avg_fp_df %>%
  mutate(
    fp_game = PPR_pts / games,
    # Extract clean abbreviation whether team is plain text or HTML
    team_clean = if_else(
      str_detect(team, "<span"),
      str_extract(team, "(?<=\\>)[A-Z]{2,3}"),
      team
    )
  ) %>%
  filter(games >= 10 & fp_game >= 8) %>%
  left_join(teams_colors_logos, by = c("team_clean" = "team_abbr"))

wr_actvsexp_TD <- ggplot(avg_exp_fpWR_df, aes(x = td, y = tds_exp)) +
  geom_point(aes(color = team_color), size = 3.5) +
  scale_color_identity() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "purple") +
  geom_text_repel(aes(label = player_name), box.padding = 0.5, max.overlaps = 22) +
  labs(
    title = "2025 NFL Wide Receivers: Actual vs. Expected Touchdowns",
    x = "Actual Touchdowns",
    y = "Expected Touchdowns",
    subtitle = "**Data:** nflfastr and nflreadr | **By:** Jake Mammen | @FantasySPack | Min. 10 games & 8 Fpts per game",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  geom_label(x = 5, y = 12, label = "Postive TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  geom_label(x = 12, y = 4, label = "Negative TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  theme_minimal() +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(),
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  )

wr_actvsexp_TD

ggsave(wr_actvsexp_TD,
  filename = "output/graphs/wr_actvsexp_TD.png",
  width    = 12,
  height   = 8,
  dpi      = 300,
  units    = "in")

#############################################
# 2025 NFL TEs: Actual vs Expected Touchdowns
#############################################

avg_exp_fpTE_df <- avg_fpTE_df %>%
  mutate(
    fp_game = PPR_pts / games,
    # Extract clean abbreviation whether team is plain text or HTML
    team_clean = if_else(
      str_detect(team, "<span"),
      str_extract(team, "(?<=\\>)[A-Z]{2,3}"),
      team
    )
  ) %>%
  filter(games >= 8 & fp_game >= 7) %>%
  left_join(teams_colors_logos, by = c("team_clean" = "team_abbr"))

te_actvsexp_TD <- ggplot(avg_exp_fpTE_df, aes(x = td, y = tds_exp)) +
  geom_point(aes(color = team_color), size = 3.5) +
  scale_color_identity() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "purple") +
  geom_text_repel(aes(label = player_name), box.padding = 0.5, max.overlaps = 22) +
  labs(
    title = "2025 NFL Tight Ends: Actual vs. Expected Touchdowns",
    x = "Actual Touchdowns",
    y = "Expected Touchdowns",
    subtitle = "**Data:** nflfastr and nflreadr | **By:** Jake Mammen | @FantasySPack | Min. 8 games & 7 Fpts per game",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  geom_label(x = 2, y = 7.5, label = "Postive TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  geom_label(x = 8, y = 1, label = "Negative TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  theme_minimal() +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(),
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  )

te_actvsexp_TD

ggsave(te_actvsexp_TD,
       filename = "output/graphs/te_actvsexp_TD.png",
       width    = 12,
       height   = 8,
       dpi      = 300,
       units    = "in")

##################################################################################################################################################

#############################################
# RB Actual vs Expected Fantasy Points PPR
#############################################

library(nflfastR)
library(nflreadr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)
library(progress)
library(scales)
library(webshot2)

# Load player stats for 2025 regular season
pbp <- nflreadr::load_player_stats(seasons = 2025) %>%
  filter(season_type == "REG", week >= 1, week <= 18)

# Load roster data to get player positions
roster <- nflreadr::load_rosters(2025) %>%
  dplyr::select(gsis_id, position, full_name) %>%
  distinct()

# Process passing/receiving stats
passRB_df <- pbp %>%
  filter(position %in% c("RB")) %>% # Filter for RBs early
  dplyr::select(
    season, week, team, player_id, player_name,
    position, receptions, targets, passing_yards, passing_tds, receiving_yards, receiving_tds,
    receiving_fumbles_lost, receiving_2pt_conversions, fumble_recovery_tds
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0038542" ~ "B.Robinson",
      player_id == "00-0036973" ~ "T.Etienne",
      player_id == "00-0036919" ~ "K.Gainwell",
      player_id == "00-0036139" ~ "R.Dowdle",
      player_id == "00-0038134" ~ "K.Walker",
      player_id == "00-0035685" ~ "D.Montgomery",
      player_id == "00-0037256" ~ "R.White",
      player_id == "00-0037263" ~ "T.Allgeier",
      player_id == "00-0036924" ~ "M.Carter",
      player_id == "00-0038797" ~ "E.Wilson",
      player_id == "00-0038611" ~ "C.Rodriguez",
      player_id == "00-0037197" ~ "I.Pacheco",
      player_id == "00-0037746" ~ "B.Robinson",
      TRUE ~ player_name
    ),
    fumble_penalty = ifelse(receiving_fumbles_lost >= 1, -2 * receiving_fumbles_lost, 0),
    PPR_points = receptions + (receiving_yards / 10) + (receiving_tds * 6) + (fumble_recovery_tds * 6) + (passing_tds * 4) + (passing_yards / 25) +
      (receiving_2pt_conversions * 2) + fumble_penalty,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  ungroup() %>%
  filter(player_name != "K.Hunt" & player_name != "N.Chubb")

# Process rushing stats for receivers
rushRB_df <- pbp %>%
  filter(position %in% c("RB"), rushing_yards > 0 | rushing_tds > 0) %>%
  dplyr::select(
    season, week, team, player_id, player_name,
    position, carries, rushing_yards, rushing_tds, rushing_fumbles_lost, rushing_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0038542" ~ "B.Robinson",
      player_id == "00-0036973" ~ "T.Etienne",
      player_id == "00-0036919" ~ "K.Gainwell",
      player_id == "00-0036139" ~ "R.Dowdle",
      player_id == "00-0038134" ~ "K.Walker",
      player_id == "00-0035685" ~ "D.Montgomery",
      player_id == "00-0037256" ~ "R.White",
      player_id == "00-0037263" ~ "T.Allgeier",
      player_id == "00-0036924" ~ "M.Carter",
      player_id == "00-0038797" ~ "E.Wilson",
      player_id == "00-0038611" ~ "C.Rodriguez",
      player_id == "00-0037197" ~ "I.Pacheco",
      player_id == "00-0037746" ~ "B.Robinson",
      TRUE ~ player_name
    ),
    fumble_penalty = ifelse(rushing_fumbles_lost >= 1, -2 * rushing_fumbles_lost, 0),
    PPR_points = (rushing_yards / 10) + (rushing_tds * 6) +
      (rushing_2pt_conversions * 2) + fumble_penalty,
    rush_attempt = 1,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Load expected fantasy points for the 2025 season, weeks 1-18
ff_opportunity <- load_ff_opportunity(seasons = 2025, stat_type = "weekly", model_version = "latest")

# Filter for running backs (RB) and select relevant columns
runningbacks <- ff_opportunity %>%
  filter(position == "RB") %>%
  dplyr::select(
    player_id,
    full_name,
    week,
    receptions_exp,
    rec_yards_gained_exp,
    rush_yards_gained_exp,
    rec_touchdown_exp,
    rush_touchdown_exp,
    pass_fantasy_points_exp,
    rush_fantasy_points_exp,
    rec_fantasy_points_exp,
    posteam
  )

# Aggregate expected points by running back across weeks 1-18
runningback_totals <- runningbacks %>%
  filter(week %in% 1:18) %>%
  group_by(player_id, full_name) %>%
  summarise(
    games_played = n(),
    rec_exp = sum(receptions_exp, na.rm = TRUE),
    yds_exp = sum(rec_yards_gained_exp, rush_yards_gained_exp, na.rm = TRUE),
    tds_exp = sum(rec_touchdown_exp, rush_touchdown_exp, na.rm = TRUE),
    total_expected_points = sum(pass_fantasy_points_exp, rush_fantasy_points_exp, rec_fantasy_points_exp, na.rm = TRUE)
  ) %>%
  arrange(desc(total_expected_points))

# Combine passing, rushing, and expected data
avg_fpRB_df <- passRB_df %>%
  group_by(player_name, position, player_id) %>%
  summarize(
    team = max(case_when(
      player_id == "00-0038542" ~ "ATL",
      player_id == "00-0036973" ~ "NO",
      player_id == "00-0036919" ~ "TB",
      player_id == "00-0036139" ~ "PIT",
      player_id == "00-0038134" ~ "KC",
      player_id == "00-0035685" ~ "HOU",
      player_id == "00-0037256" ~ "WAS",
      player_id == "00-0037263" ~ "ARI",
      player_id == "00-0036924" ~ "TEN",
      player_id == "00-0038797" ~ "SEA",
      player_id == "00-0038611" ~ "JAX",
      player_id == "00-0037197" ~ "DET",
      player_id == "00-0037746" ~ "ATL",
      TRUE ~ team
    ), na.rm = TRUE),
    games_pass = sum(game_played, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    catches = sum(receptions, na.rm = TRUE),
    pass_yards = sum(receiving_yards, na.rm = TRUE),
    pass_td = sum(receiving_tds, na.rm = TRUE),
    pass_fumbles = sum(receiving_fumbles_lost, na.rm = TRUE),
    pass_PPR_pts = sum(PPR_points, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    rushRB_df %>%
      group_by(player_name, position, player_id) %>%
      summarize(
        team = max(case_when(
          player_id == "00-0038542" ~ "ATL",
          player_id == "00-0036973" ~ "NO",
          player_id == "00-0036919" ~ "TB",
          player_id == "00-0036139" ~ "PIT",
          player_id == "00-0038134" ~ "KC",
          player_id == "00-0035685" ~ "HOU",
          player_id == "00-0037256" ~ "WAS",
          player_id == "00-0037263" ~ "ARI",
          player_id == "00-0036924" ~ "TEN",
          player_id == "00-0038797" ~ "SEA",
          player_id == "00-0038611" ~ "JAX",
          player_id == "00-0037197" ~ "DET",
          player_id == "00-0037746" ~ "ATL",
          TRUE ~ team
        ), na.rm = TRUE),
        games_rush = sum(game_played, na.rm = TRUE),
        rush_attempts = sum(carries, na.rm = TRUE),
        rush_yards = sum(rushing_yards, na.rm = TRUE),
        rush_td = sum(rushing_tds, na.rm = TRUE),
        rush_fumbles = sum(rushing_fumbles_lost, na.rm = TRUE),
        rush_PPR_pts = sum(PPR_points, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = c("team", "player_name", "position", "player_id")
  ) %>%
  left_join(
    runningback_totals %>%
      dplyr::select(player_id, rec_exp, yds_exp, tds_exp, total_expected_points),
    by = c("player_id")
  ) %>%
  mutate(
    games = pmax(games_pass, games_rush, na.rm = TRUE),
    rush_attempts = coalesce(rush_attempts, 0),
    rush_yards = coalesce(rush_yards, 0),
    rush_td = coalesce(rush_td, 0),
    rush_fumbles = coalesce(rush_fumbles, 0),
    rush_PPR_pts = coalesce(rush_PPR_pts, 0),
    PPR_pts = pass_PPR_pts + rush_PPR_pts,
    yards = pass_yards + rush_yards,
    td = pass_td + rush_td,
    fumbles = pass_fumbles + rush_fumbles,
    rec_exp = coalesce(rec_exp, 0),
    yds_exp = coalesce(yds_exp, 0),
    tds_exp = coalesce(tds_exp, 0),
    total_expected_points = coalesce(total_expected_points, 0),
    forp = PPR_pts - total_expected_points
  ) %>%
  filter(position == "RB", games >= 8) %>%
  dplyr::select(team, player_name, games, rush_attempts, targets, catches, yards, td, fumbles, PPR_pts, rec_exp, yds_exp, tds_exp, total_expected_points, forp)

# Add team colors and create colored abbreviations
team_info <- teams_colors_logos %>%
  select(team_abbr, team_color) %>%
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

avg_fpRB_df <- avg_fpRB_df %>%
  left_join(team_info, by = c("team" = "team_abbr")) %>%
  mutate(
    team = paste0(
      "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
    )
  ) %>%
  select(-team_color)

# Create the gt table
fp_RB2025 <- avg_fpRB_df %>%
  arrange(-PPR_pts) %>%
  dplyr::slice(1:60) %>%
  mutate(Rank = row_number()) %>%
  gt() %>%
  tab_header(title = md('**2025 Actual vs. Expected PPR Fantasy Points Running Backs**')) %>%
  cols_move_to_start(columns = vars(Rank)) %>%
  cols_label(
    games = 'GP',
    player_name = '',
    team = 'Team',
    rush_attempts = 'Car',
    targets = 'Targ',
    catches = 'Rec',
    yards = 'Yds',
    td = 'TD',
    fumbles = 'Fum',
    PPR_pts = 'FP',
    rec_exp = 'Rec',
    yds_exp = 'Yds',
    tds_exp = 'TD',
    total_expected_points = 'FP',
    forp = 'FORP'
  ) %>%
  fmt_number(columns = vars(PPR_pts, total_expected_points), decimals = 1) %>%
  fmt_number(columns = vars(rush_attempts, yards, catches, yds_exp, rec_exp, tds_exp), decimals = 0, sep_mark = ',') %>%
  fmt_markdown(columns = vars(team)) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_body(vars(player_name))) %>%
  tab_spanner(label = md('**Actual**'), columns = vars(games, rush_attempts, catches, yards, td, fumbles, PPR_pts)) %>%
  tab_spanner(label = md('**Expected**'), columns = vars(targets, rec_exp, yds_exp, tds_exp, total_expected_points)) %>%
  data_color(
    columns = vars(PPR_pts, total_expected_points),
    colors = scales::col_numeric(palette = c('grey97', '#E03FD8'), domain = c(8, 435)),
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(forp),
    colors = scales::col_numeric(palette = c('#FF4040', '#FFFFFF', '#40C040'), domain = c(-40, 0, 62)),
    autocolor_text = FALSE
  ) %>%
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
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
           <b>Data:</b> nflreadr | <b>Credit:</b> Anthony Reinhard, 2020, Open Source Football | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height = 30
        ),
        "</div>"
      )
    )
  )

fp_RB2025

gtsave(fp_RB2025,
       filename = "output/tables/exp_fp_RB2025.png")

#####################################################################################################################################################

library(nflfastR)
library(nflreadr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)
library(progress)
library(scales)
library(webshot2)

# Load player stats for 2025 regular season
pbp <- nflreadr::load_player_stats(seasons = 2025) %>%
  filter(season_type == "REG", week >= 1, week <= 18)

# Load roster data to get player positions
roster <- nflreadr::load_rosters(2025) %>%
  dplyr::select(gsis_id, position, full_name) %>%
  distinct()

# Process passing/receiving stats
passQB_df <- pbp %>%
  filter(position %in% c("QB")) %>%
  dplyr::select(
    season, week, team, player_id, player_name,
    position, attempts, receptions, receiving_yards, receiving_tds, passing_yards, passing_tds,
    passing_interceptions, sack_fumbles_lost, rushing_fumbles_lost, passing_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0030565" ~ "G.Smith",
      player_id == "00-0036212" ~ "T.Tagovailoa",
      player_id == "00-0026158" ~ "J.Flacco",
      player_id == "00-0036945" ~ "J.Fields",
      player_id == "00-0029604" ~ "K.Cousins",
      TRUE ~ player_name
    ),
    sack_fumbles_penalty = ifelse(sack_fumbles_lost >= 1, -2 * sack_fumbles_lost, 0),
    int_penalty = ifelse(passing_interceptions >= 1, -2 * passing_interceptions, 0),
    PPR_points = (passing_yards * 0.04) + (passing_tds * 4) + (passing_2pt_conversions * 2) +
      receptions + (receiving_yards / 10) + (receiving_tds * 6) + sack_fumbles_penalty + int_penalty,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  ungroup()

# Process rushing stats for receivers
rushQB_df <- pbp %>%
  filter(position %in% c("QB"), rushing_yards >= 0 | rushing_tds >= 0) %>%
  dplyr::select(
    season, week, team, player_id, player_name,
    position, carries, rushing_yards, rushing_tds, rushing_fumbles_lost, rushing_2pt_conversions
  ) %>%
  mutate(
    player_name = case_when(
      player_id == "00-0030565" ~ "G.Smith",
      player_id == "00-0036212" ~ "T.Tagovailoa",
      player_id == "00-0026158" ~ "J.Flacco",
      player_id == "00-0036945" ~ "J.Fields",
      player_id == "00-0029604" ~ "K.Cousins",
      TRUE ~ player_name
    ),
    fumble_penalty = ifelse(rushing_fumbles_lost >= 1, -2 * rushing_fumbles_lost, 0),
    PPR_points = (rushing_yards / 10) + (rushing_tds * 6) +
      (rushing_2pt_conversions * 2) + fumble_penalty,
    rush_attempt = 1,
    game_played = 1
  ) %>%
  group_by(week, player_name) %>%
  mutate(game_played = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# Load expected fantasy points for the 2025 season, weeks 1-18
ff_opportunity <- load_ff_opportunity(seasons = 2025, stat_type = "weekly", model_version = "latest")

# Filter for quarterbacks (QB) and select relevant columns
quarterbacks <- ff_opportunity %>%
  filter(position == "QB") %>%
  dplyr::select(
    player_id,
    full_name,
    week,
    pass_yards_gained_exp,
    pass_touchdown_exp,
    rush_yards_gained_exp,
    rush_touchdown_exp,
    pass_fantasy_points_exp,
    rush_fantasy_points_exp,
    posteam
  )

# Aggregate expected points by quarterback across weeks 1-18
quarterback_totals <- quarterbacks %>%
  filter(week %in% 1:18) %>%
  group_by(player_id, full_name) %>%
  summarise(
    games_played = n(),
    pass_yds_exp = sum(pass_yards_gained_exp, na.rm = TRUE),
    rush_yds_exp = sum(rush_yards_gained_exp, na.rm = TRUE),
    tds_exp = sum(pass_touchdown_exp, rush_touchdown_exp, na.rm = TRUE),
    total_expected_points = sum(pass_fantasy_points_exp, rush_fantasy_points_exp, na.rm = TRUE)
  ) %>%
  arrange(desc(total_expected_points))

# Combine passing, rushing, and expected data
avg_fpQB_df <- passQB_df %>%
  group_by(player_name, position, player_id) %>%
  summarize(
    team = max(case_when(
      player_id == "00-0038542" ~ "ATL",
      player_id == "00-0030565" ~ "NYJ",
      player_id == "00-0036212" ~ "ATL",
      player_id == "00-0026158" ~ "CIN",
      player_id == "00-0036945" ~ "KC",
      player_id == "00-0029604" ~ "LV",
      TRUE ~ team
    ), na.rm = TRUE),
    games_pass = sum(game_played, na.rm = TRUE),
    pass_yards = sum(passing_yards, na.rm = TRUE),
    pass_td = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(passing_interceptions, na.rm = TRUE),
    pass_fumbles = sum(sack_fumbles_lost, na.rm = TRUE),
    pass_PPR_pts = sum(PPR_points, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    rushQB_df %>%
      group_by(player_name, position, player_id) %>%
      summarize(
        team = max(case_when(
          player_id == "00-0038542" ~ "ATL",
          player_id == "00-0030565" ~ "NYJ",
          player_id == "00-0036212" ~ "ATL",
          player_id == "00-0026158" ~ "CIN",
          player_id == "00-0036945" ~ "KC",
          player_id == "00-0029604" ~ "LV",
          TRUE ~ team
        ), na.rm = TRUE),
        games_rush = sum(game_played, na.rm = TRUE),
        rush_attempts = sum(carries, na.rm = TRUE),
        rush_yards = sum(rushing_yards, na.rm = TRUE),
        rush_td = sum(rushing_tds, na.rm = TRUE),
        rush_fumbles = sum(rushing_fumbles_lost, na.rm = TRUE),
        rush_PPR_pts = sum(PPR_points, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = c("team", "player_name", "position", "player_id")
  ) %>%
  left_join(
    quarterback_totals %>%
      dplyr::select(player_id, pass_yds_exp, rush_yds_exp, tds_exp, total_expected_points),
    by = c("player_id")
  ) %>%
  mutate(
    games = pmax(games_pass, games_rush, na.rm = TRUE),
    rush_attempts = coalesce(rush_attempts, 0),
    rush_yards = coalesce(rush_yards, 0),
    rush_td = coalesce(rush_td, 0),
    rush_fumbles = coalesce(rush_fumbles, 0),
    rush_PPR_pts = coalesce(rush_PPR_pts, 0),
    PPR_pts = pass_PPR_pts + rush_PPR_pts,
    yards = pass_yards + rush_yards,
    td = pass_td + rush_td,
    fumbles = pass_fumbles + rush_fumbles,
    pass_yds_exp = coalesce(pass_yds_exp, 0),
    rush_yds_exp = coalesce(rush_yds_exp, 0),
    tds_exp = coalesce(tds_exp, 0),
    total_expected_points = coalesce(total_expected_points, 0),
    forp = PPR_pts - total_expected_points
  ) %>%
  filter(position == "QB", games >= 8) %>%
  dplyr::select(team, player_name, games, yards, td, interceptions, fumbles, PPR_pts,
                pass_yds_exp, rush_yds_exp, tds_exp, total_expected_points, forp)

# Add team colors and create colored abbreviations
team_info <- teams_colors_logos %>%
  select(team_abbr, team_color) %>%
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

avg_fpQB_df <- avg_fpQB_df %>%
  left_join(team_info, by = c("team" = "team_abbr")) %>%
  mutate(
    team = paste0(
      "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
    )
  ) %>%
  select(-team_color)

# Create the gt table
fp_QB2025 <- avg_fpQB_df %>%
  arrange(-PPR_pts) %>%
  dplyr::slice(1:37) %>% # Adjust for typical number of starting QBs
  mutate(Rank = row_number()) %>%
  gt() %>%
  tab_header(title = md('**2025 Actual vs. Expected PPR Fantasy Points Quarterbacks**')) %>%
  cols_move_to_start(columns = vars(Rank)) %>%
  cols_label(
    games = 'GP',
    player_name = '',
    team = 'Team',
    yards = 'Yards',
    td = 'TD',
    interceptions = 'Int',
    fumbles = 'Fum',
    PPR_pts = 'FP',
    pass_yds_exp = 'Pass Yds',
    rush_yds_exp = 'Rush Yds',
    tds_exp = 'TD',
    total_expected_points = 'FP',
    forp = 'FORP'
  ) %>%
  fmt_number(columns = vars(PPR_pts, total_expected_points), decimals = 1) %>%
  fmt_number(columns = vars(yards, pass_yds_exp, rush_yds_exp, tds_exp), decimals = 0, sep_mark = ',') %>%
  fmt_markdown(columns = vars(team)) %>%
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_body(vars(player_name))) %>%
  tab_spanner(label = md('**Actual**'), columns = vars(games, yards, td, interceptions, fumbles, PPR_pts)) %>%
  tab_spanner(label = md('**Expected**'), columns = vars(pass_yds_exp, rush_yds_exp, tds_exp, total_expected_points)) %>%
  data_color(
    columns = vars(PPR_pts, total_expected_points),
    colors = scales::col_numeric(palette = c('grey97', '#E03FD8'), domain = c(65, 375)),
    autocolor_text = FALSE
  ) %>%
  data_color(
    columns = vars(forp),
    colors = scales::col_numeric(palette = c('#FF4040', '#FFFFFF', '#40C040'), domain = c(-42, 0, 46)),
    autocolor_text = FALSE
  ) %>%
  tab_options(
    table.font.color = 'darkblue',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = 'darkblue',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = 'darkblue',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = 'darkblue',
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
           <b>Data:</b> nflreadr | <b>Credit:</b> Anthony Reinhard, 2020, Open Source Football | <b>Created by:</b> @FantasySPack & @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height = 30
        ),
        "</div>"
      )
    )
  )

fp_QB2025

gtsave(fp_QB2025,
       filename = "output/tables/exp_fp_QB2025.png")

#######################################################################################################################
library(nflfastR)
library(ggimage)
library(ggrepel)

# Load play-by-play data
pbp <- load_pbp(2025) |>
  filter(week >= 1 & week <= 18)

avg_exp_fpRB_df <- avg_fpRB_df %>%
  mutate(
    fp_game = PPR_pts / games,
    # Extract clean abbreviation whether team is plain text or HTML
    team_clean = if_else(
      str_detect(team, "<span"),
      str_extract(team, "(?<=\\>)[A-Z]{2,3}"),
      team
    )
  ) %>%
  filter(games >= 8 & fp_game >= 8) %>%
  left_join(teams_colors_logos, by = c("team_clean" = "team_abbr"))

rb_actvsexp_TD <- ggplot(avg_exp_fpRB_df, aes(x = td, y = tds_exp)) +
  geom_point(aes(color = team_color), size = 3.5) +
  scale_color_identity() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "purple") +
  geom_text_repel(aes(label = player_name), box.padding = 0.5, max.overlaps = 22) +
  labs(
    title = "2025 NFL Running Backs: Actual vs. Expected Touchdowns",
    x = "Actual Touchdowns",
    y = "Expected Touchdowns",
    subtitle = "**Data:** nflfastr and nflreadr | **By:** Jake Mammen | @FantasySPack | Min. 8 games & 8 Fpts per game",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  geom_label(x = 5, y = 15, label = "Postive TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  geom_label(x = 15, y = 5, label = "Negative TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  theme_minimal() +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(),
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  )

rb_actvsexp_TD

ggsave(rb_actvsexp_TD,
       filename = "output/graphs/rb_actvsexp_TD.png",
       width    = 12,
       height   = 8,
       dpi      = 300,
       units    = "in")


library(nflfastR)
library(ggimage)
library(ggrepel)

# Load play-by-play data
pbp <- load_pbp(2025) |>
  filter(week >= 1 & week <= 18)

avg_exp_fpQB_df <- avg_fpQB_df %>%
  mutate(
    fp_game = PPR_pts / games,
    # Extract clean abbreviation whether team is plain text or HTML
    team_clean = if_else(
      str_detect(team, "<span"),
      str_extract(team, "(?<=\\>)[A-Z]{2,3}"),
      team
    )
  ) %>%
  filter(games >= 10 & fp_game >= 10) %>%
  left_join(teams_colors_logos, by = c("team_clean" = "team_abbr"))

qb_actvsexp_TD <- ggplot(avg_exp_fpQB_df, aes(x = td, y = tds_exp)) +
  geom_point(aes(color = team_color), size = 3.5) +
  scale_color_identity() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "purple") +
  geom_text_repel(aes(label = player_name), box.padding = 0.5, max.overlaps = 22) +
  labs(
    title = "2025 NFL Quarterbacks: Actual vs. Expected Touchdowns",
    x = "Actual Touchdowns",
    y = "Expected Touchdowns",
    subtitle = "**Data:** nflfastr and nflreadr | **By:** Jake Mammen | @FantasySPack | Min. 10 games & 10 Fpts per game",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  geom_label(x = 20, y = 40, label = "Postive TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  geom_label(x = 40, y = 12, label = "Negative TD Regression Candidates", fill = "purple", color = "white", label.size = 0.5) +
  theme_minimal() +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = ggtext::element_markdown(),
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  )

qb_actvsexp_TD

ggsave(qb_actvsexp_TD,
       filename = "output/graphs/qb_actvsexp_TD.png",
       width    = 12,
       height   = 8,
       dpi      = 300,
       units    = "in")
