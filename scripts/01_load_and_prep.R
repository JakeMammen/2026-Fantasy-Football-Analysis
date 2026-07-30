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

# Filter to regular-season games and skill positions only
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
    opponent_team,
    completions, attempts, passing_yards, passing_tds, passing_interceptions,
    carries, rushing_yards, rushing_tds,
    receptions, targets, receiving_yards, receiving_tds,
    fantasy_points_ppr
  )

# Quick checks
dim(skill_stats)
table(skill_stats$position)
glimpse(skill_stats)


# ============================================================
# 3. Aggregate to player-season level
# ============================================================

# Aggregate to player-season level
season_stats <- skill_stats %>%
  group_by(player_id, player_display_name, position, season, team) %>%
  summarise(
    games = n(),
    # Passing
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(passing_interceptions, na.rm = TRUE),
    # Rushing
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    # Receiving
    receptions = sum(receptions, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    # Fantasy
    fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Add per-game rates (useful for modeling)
  mutate(
    ppg_ppr = fantasy_points_ppr / games,
    pass_ypg = passing_yards / games,
    rush_ypg = rushing_yards / games,
    rec_ypg = receiving_yards / games,
    tgt_pg = targets / games,
    rec_pg = receptions / games
  )

# Inspection
dim(season_stats)
glimpse(season_stats)
summary(season_stats$games)


# ============================================================
# 4. Create lagged features
# ============================================================

# Arrange and create lagged features
model_data <- season_stats %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    # Lagged fantasy production
    lag_ppg_ppr       = lag(ppg_ppr),
    lag_fantasy_pts   = lag(fantasy_points_ppr),
    lag_games         = lag(games),
    
    # Lagged volume
    lag_attempts      = lag(attempts),
    lag_carries       = lag(carries),
    lag_targets       = lag(targets),
    lag_receptions    = lag(receptions),
    
    # Lagged efficiency / yards
    lag_pass_ypg      = lag(pass_ypg),
    lag_rush_ypg      = lag(rush_ypg),
    lag_rec_ypg       = lag(rec_ypg),
    lag_tgt_pg        = lag(tgt_pg),
    lag_rec_pg        = lag(rec_pg)
  ) %>%
  ungroup() %>%
  # Keep only rows that have a prior season (needed for lag-based models)
  filter(!is.na(lag_ppg_ppr))

# Inspection
dim(model_data)
glimpse(model_data)


# ============================================================
# Component-based 2026 projections
# ============================================================

# Create lagged counting stats (using 2023–2025 history)
proj_components <- season_stats %>%
  arrange(player_id, season) %>%
  group_by(player_id, player_display_name, position) %>%
  mutate(
    # Passing
    lag1_completions = lag(completions, 1),
    lag2_completions = lag(completions, 2),
    lag3_completions = lag(completions, 3),
    
    lag1_pass_yds = lag(passing_yards, 1),
    lag2_pass_yds = lag(passing_yards, 2),
    lag3_pass_yds = lag(passing_yards, 3),
    
    lag1_pass_tds = lag(passing_tds, 1),
    lag2_pass_tds = lag(passing_tds, 2),
    lag3_pass_tds = lag(passing_tds, 3),
    
    lag1_ints = lag(interceptions, 1),
    lag2_ints = lag(interceptions, 2),
    lag3_ints = lag(interceptions, 3),
    
    # Rushing
    lag1_carries = lag(carries, 1),
    lag2_carries = lag(carries, 2),
    lag3_carries = lag(carries, 3),
    
    lag1_rush_yds = lag(rushing_yards, 1),
    lag2_rush_yds = lag(rushing_yards, 2),
    lag3_rush_yds = lag(rushing_yards, 3),
    
    lag1_rush_tds = lag(rushing_tds, 1),
    lag2_rush_tds = lag(rushing_tds, 2),
    lag3_rush_tds = lag(rushing_tds, 3),
    
    # Receiving
    lag1_targets = lag(targets, 1),
    lag2_targets = lag(targets, 2),
    lag3_targets = lag(targets, 3),
    
    lag1_receptions = lag(receptions, 1),
    lag2_receptions = lag(receptions, 2),
    lag3_receptions = lag(receptions, 3),
    
    lag1_rec_yds = lag(receiving_yards, 1),
    lag2_rec_yds = lag(receiving_yards, 2),
    lag3_rec_yds = lag(receiving_yards, 3),
    
    lag1_rec_tds = lag(receiving_tds, 1),
    lag2_rec_tds = lag(receiving_tds, 2),
    lag3_rec_tds = lag(receiving_tds, 3)
  ) %>%
  ungroup() %>%
  filter(season == 2025)   # project forward from 2025

# Helper function for weighted average of three lags
wt_avg <- function(l1, l2, l3) {
  case_when(
    !is.na(l1) & !is.na(l2) & !is.na(l3) ~ 0.50*l1 + 0.30*l2 + 0.20*l3,
    !is.na(l1) & !is.na(l2) & is.na(l3)  ~ 0.60*l1 + 0.40*l2,
    !is.na(l1) & is.na(l2)               ~ l1,
    TRUE ~ NA_real_
  )
}

# Apply weighted averages to every component
proj_2026 <- proj_components %>%
  mutate(
    # Passing components
    proj_completions = wt_avg(lag1_completions, lag2_completions, lag3_completions),
    proj_pass_yds    = wt_avg(lag1_pass_yds, lag2_pass_yds, lag3_pass_yds),
    proj_pass_tds    = wt_avg(lag1_pass_tds, lag2_pass_tds, lag3_pass_tds),
    proj_ints        = wt_avg(lag1_ints, lag2_ints, lag3_ints),
    
    # Rushing components
    proj_carries     = wt_avg(lag1_carries, lag2_carries, lag3_carries),
    proj_rush_yds    = wt_avg(lag1_rush_yds, lag2_rush_yds, lag3_rush_yds),
    proj_rush_tds    = wt_avg(lag1_rush_tds, lag2_rush_tds, lag3_rush_tds),
    
    # Receiving components
    proj_targets     = wt_avg(lag1_targets, lag2_targets, lag3_targets),
    proj_receptions  = wt_avg(lag1_receptions, lag2_receptions, lag3_receptions),
    proj_rec_yds     = wt_avg(lag1_rec_yds, lag2_rec_yds, lag3_rec_yds),
    proj_rec_tds     = wt_avg(lag1_rec_tds, lag2_rec_tds, lag3_rec_tds)
  ) %>%
  # Convert projected components into full-PPR fantasy points
  mutate(
    proj_points = 
      # Passing
      (proj_pass_yds * 0.04) + (proj_pass_tds * 4) + (proj_ints * -2) +
      # Rushing
      (proj_rush_yds * 0.1) + (proj_rush_tds * 6) +
      # Receiving
      (proj_receptions * 1) + (proj_rec_yds * 0.1) + (proj_rec_tds * 6),
    
    proj_ppg = proj_points / 17
  ) %>%
  select(
    player_id, player_display_name, position, team, games,
    # Projected components (optional to keep for inspection)
    proj_completions, proj_pass_yds, proj_pass_tds, proj_ints,
    proj_carries, proj_rush_yds, proj_rush_tds,
    proj_targets, proj_receptions, proj_rec_yds, proj_rec_tds,
    # Final projections
    proj_points, proj_ppg
  ) %>%
  arrange(position, desc(proj_points))

# Inspection
head(proj_2026, 15)
summary(proj_2026$proj_points)

# ============================================================
# Position-specific linear regression models
# ============================================================

library(dplyr)
library(purrr)
library(broom)   # optional, for tidy model output

# 1. Prepare modeling panel with one-year lags
model_panel <- season_stats %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    # Target will be current season PPG
    # Predictors = previous season values
    lag_ppg          = lag(ppg_ppr),
    lag_games        = lag(games),
    
    # Passing
    lag_attempts     = lag(attempts),
    lag_pass_yds     = lag(passing_yards),
    lag_pass_tds     = lag(passing_tds),
    lag_ints         = lag(interceptions),
    
    # Rushing
    lag_carries      = lag(carries),
    lag_rush_yds     = lag(rushing_yards),
    lag_rush_tds     = lag(rushing_tds),
    
    # Receiving
    lag_targets      = lag(targets),
    lag_receptions   = lag(receptions),
    lag_rec_yds      = lag(receiving_yards),
    lag_rec_tds      = lag(receiving_tds)
  ) %>%
  ungroup() %>%
  filter(!is.na(lag_ppg))   # keep only rows with prior-season data

# 2. Define position-specific model formulas
#    (using the most relevant lagged volume stats + prior PPG)

qb_formula <- ppg_ppr ~ lag_ppg + lag_attempts + lag_pass_yds + lag_pass_tds + 
  lag_ints + lag_carries + lag_rush_yds + lag_rush_tds

rb_formula <- ppg_ppr ~ lag_ppg + lag_carries + lag_rush_yds + lag_rush_tds + 
  lag_targets + lag_receptions + lag_rec_yds + lag_rec_tds

wr_te_formula <- ppg_ppr ~ lag_ppg + lag_targets + lag_receptions + lag_rec_yds + 
  lag_rec_tds + lag_carries + lag_rush_yds + lag_rush_tds

# 3. Fit models by position
qb_model <- model_panel %>%
  filter(position == "QB", games >= 6) %>%          # minimum games filter
  lm(qb_formula, data = .)

rb_model <- model_panel %>%
  filter(position == "RB", games >= 6) %>%
  lm(rb_formula, data = .)

wr_model <- model_panel %>%
  filter(position == "WR", games >= 6) %>%
  lm(wr_te_formula, data = .)

te_model <- model_panel %>%
  filter(position == "TE", games >= 6) %>%
  lm(wr_te_formula, data = .)

# Optional: inspect model summaries
summary(qb_model)
summary(rb_model)
summary(wr_model)
summary(te_model)

# 4. Generate 2026 projections (apply models to 2025 data)
pred_2025 <- model_panel %>%
  filter(season == 2025)

# Predict by position
pred_2025 <- pred_2025 %>%
  mutate(
    pred_ppg = case_when(
      position == "QB" ~ predict(qb_model, newdata = .),
      position == "RB" ~ predict(rb_model, newdata = .),
      position == "WR" ~ predict(wr_model, newdata = .),
      position == "TE" ~ predict(te_model, newdata = .),
      TRUE ~ NA_real_
    ),
    pred_points = pred_ppg * 17
  ) %>%
  select(
    player_id, player_display_name, position, team, games,
    lag_ppg, pred_ppg, pred_points
  ) %>%
  arrange(position, desc(pred_ppg))

# Inspection
head(pred_2025, 20)
summary(pred_2025$pred_ppg)