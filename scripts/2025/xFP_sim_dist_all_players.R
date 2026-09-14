# =============================================================================
# Stage 1: Foundation
# Multi-player PPR Expected Points Simulation Framework
# =============================================================================

# ----- Libraries -------------------------------------------------------------
library(nflreadr)
library(tidyverse)

# ----- Parameters (edit these) -----------------------------------------------
season_year     <- 2025
# player_codes    <- "M.Wilson"          # single player, vector of players, or NULL for all
# player_codes  <- c("M.Wilson", "M.Harrison", "T.McBride")  # example multi-player
player_codes  <- NULL                                        # all players
week_range      <- 1:18
n_sims          <- 5000                # will be used in later stages
min_targets     <- 1                   # placeholder; enforced in Stage 4

set.seed(42)

# ----- Load play-by-play data ------------------------------------------------
message("Loading ", season_year, " play-by-play data...")
pbp <- load_pbp(season_year) %>%
  filter(
    week %in% week_range,
    season_type == "REG"
  )

# ----- Create clean targets table --------------------------------------------
targets <- pbp %>%
  filter(
    pass_attempt == 1,
    two_point_attempt == 0,
    !is.na(receiver_id),
    !is.na(receiver)
  ) %>%
  { if (!is.null(player_codes)) filter(., receiver %in% player_codes) else . } %>%
  select(
    season,
    week,
    game_id,
    play_id,
    posteam,
    receiver,
    receiver_id,
    yardline_100,
    air_yards,
    yards_gained,
    complete_pass,
    cp,                  # completion probability
    xyac_success         # expected YAC success probability
  ) %>%
  mutate(
    # Preserve original logic for end-zone / goal-to-go cases
    gain     = if_else(yardline_100 == air_yards, yardline_100, yards_gained),
    yac_prob = if_else(yardline_100 == air_yards, 1, xyac_success)
  )

# Quick diagnostic
message("Targets table created: ", nrow(targets), " rows covering ",
        n_distinct(targets$receiver), " receiver(s).")
glimpse(targets)

# =============================================================================
# Stage 2 (Final): Outcome Construction with Normalized Probabilities
# =============================================================================

# Remove plays that lack model probabilities
targets_clean <- targets %>%
  filter(!is.na(cp), !is.na(yac_prob))

message("Targets after removing missing cp/yac_prob: ", nrow(targets_clean),
        " rows covering ", n_distinct(targets_clean$receiver), " receivers.")

# ----- Success path ----------------------------------------------------------
success_df <- targets_clean %>%
  mutate(
    PPR_points        = 1 + gain / 10 + if_else(gain == yardline_100, 6, 0),
    catch_run_prob    = cp * yac_prob,
    actual_outcome    = if_else(yards_gained == gain & complete_pass == 1, 1L, 0L),
    actual_PPR_points = if_else(actual_outcome == 1L, PPR_points, 0),
    outcome_type      = "success"
  )

# ----- Incomplete path -------------------------------------------------------
incomplete_df <- targets_clean %>%
  mutate(
    gain              = 0,
    PPR_points        = 0,
    catch_run_prob    = 1 - cp,
    actual_PPR_points = NA_real_,
    outcome_type      = "incomplete"
  )

# ----- Combined + Normalized outcomes table ----------------------------------
outcomes <- bind_rows(success_df, incomplete_df) %>%
  select(
    season, week, game_id, play_id, posteam,
    receiver, receiver_id,
    catch_run_prob, PPR_points,
    actual_PPR_points, outcome_type
  ) %>%
  group_by(game_id, play_id) %>%
  mutate(catch_run_prob = catch_run_prob / sum(catch_run_prob)) %>%  # normalize
  ungroup() %>%
  arrange(game_id, play_id, outcome_type)

# Diagnostics
message("Outcomes table created: ", nrow(outcomes), " rows (",
        nrow(outcomes) / 2, " targets × 2 outcomes).")
message("Unique receivers: ", n_distinct(outcomes$receiver))

outcomes %>%
  group_by(game_id, play_id) %>%
  summarise(prob_sum = sum(catch_run_prob), .groups = "drop") %>%
  summarise(
    min_prob  = min(prob_sum),
    max_prob  = max(prob_sum),
    mean_prob = mean(prob_sum)
  ) %>%
  print()

# =============================================================================
# Stage 3 (Revised): Fast Monte Carlo Simulation
# =============================================================================

message("Preparing fast simulation...")

# 1. Create one row per play with both possible outcomes
play_level <- outcomes %>%
  select(season, week, game_id, play_id, posteam, receiver, receiver_id,
         outcome_type, PPR_points, catch_run_prob) %>%
  pivot_wider(
    names_from  = outcome_type,
    values_from = c(PPR_points, catch_run_prob),
    names_glue  = "{outcome_type}_{.value}"
  ) %>%
  mutate(
    # After pivoting we have success_PPR_points, incomplete_PPR_points,
    # success_catch_run_prob, incomplete_catch_run_prob
    prob_success = success_catch_run_prob   # already normalized
  ) %>%
  select(
    season, week, game_id, play_id, posteam, receiver, receiver_id,
    ppr_success   = success_PPR_points,
    ppr_incomplete = incomplete_PPR_points,
    prob_success
  )

message("Play-level table created: ", nrow(play_level), " plays")

# 2. Fast simulation using vectorized random draws
n_plays <- nrow(play_level)
n_sims  <- n_sims   # from parameters (consider lowering to 2000–3000 for testing)

message("Running ", n_sims, " simulations on ", n_plays, " plays...")

start_time <- Sys.time()

# Generate all random numbers at once (n_plays × n_sims)
rand_mat <- matrix(runif(n_plays * n_sims), nrow = n_plays, ncol = n_sims)

# Choose outcome: TRUE = success, FALSE = incomplete
success_mat <- rand_mat < play_level$prob_success

# Calculate PPR points for every play × simulation
ppr_mat <- success_mat * play_level$ppr_success +
  (!success_mat) * play_level$ppr_incomplete

# 3. Aggregate to game + receiver level for every simulation
# We convert to a long format efficiently
sim_df <- bind_cols(
  play_level %>% select(season, week, game_id, posteam, receiver, receiver_id),
  as.data.frame(ppr_mat)
) %>%
  pivot_longer(
    cols      = starts_with("V"),
    names_to  = "sim_id",
    values_to = "ppr"
  ) %>%
  mutate(sim_id = as.integer(sub("V", "", sim_id))) %>%
  group_by(season, week, game_id, posteam, receiver, receiver_id, sim_id) %>%
  summarise(sim_tot = sum(ppr), .groups = "drop")

end_time <- Sys.time()
elapsed  <- round(difftime(end_time, start_time, units = "secs"), 1)

message("Simulation complete in ", elapsed, " seconds.")
message("sim_df dimensions: ", nrow(sim_df), " rows × ", ncol(sim_df), " columns")
message("Unique receivers: ", n_distinct(sim_df$receiver))

# =============================================================================
# Stage 4: Summaries — Actual vs Expected
# =============================================================================

message("Calculating actual points and simulation summaries...")

# ----- Actual PPR points per player-game -------------------------------------
actual_df <- targets_clean %>%
  mutate(
    PPR_points = 1 + gain / 10 + if_else(gain == yardline_100, 6, 0),
    actual_PPR = if_else(complete_pass == 1, PPR_points, 0)
  ) %>%
  group_by(season, week, game_id, posteam, receiver, receiver_id) %>%
  summarise(
    actual     = sum(actual_PPR, na.rm = TRUE),
    targets    = n(),
    .groups    = "drop"
  )

# ----- Simulation summaries (mean + full distribution for percentiles) -------
sim_summary <- sim_df %>%
  group_by(season, week, game_id, posteam, receiver, receiver_id) %>%
  summarise(
    expected = mean(sim_tot),
    sd_sim   = sd(sim_tot),
    .groups  = "drop"
  )

# ----- Percentile of actual within simulated distribution --------------------
# Combine actual with all simulations to compute percent_rank
percentile_df <- bind_rows(
  sim_df %>% select(season, week, game_id, posteam, receiver, receiver_id, sim_tot) %>%
    mutate(is_actual = 0L),
  actual_df %>% transmute(season, week, game_id, posteam, receiver, receiver_id,
                          sim_tot = actual, is_actual = 1L)
) %>%
  group_by(season, week, game_id, posteam, receiver, receiver_id) %>%
  mutate(percentile = percent_rank(sim_tot)) %>%
  filter(is_actual == 1L) %>%
  select(season, week, game_id, posteam, receiver, receiver_id, percentile) %>%
  ungroup()

# ----- Final summary table ---------------------------------------------------
summary_df <- actual_df %>%
  left_join(sim_summary, by = c("season", "week", "game_id", "posteam",
                                "receiver", "receiver_id")) %>%
  left_join(percentile_df, by = c("season", "week", "game_id", "posteam",
                                  "receiver", "receiver_id")) %>%
  mutate(
    residual   = actual - expected,
    percentile = round(percentile * 100, 1)
  ) %>%
  select(
    season, week, game_id, posteam, receiver, receiver_id,
    targets, actual, expected, residual, percentile, sd_sim
  ) %>%
  arrange(week, desc(targets))

# Diagnostics
message("Summary table created: ", nrow(summary_df), " player-game rows")
message("Unique receivers: ", n_distinct(summary_df$receiver))

# Preview
summary_df %>%
  head(10) %>%
  print()

# =============================================================================
# Stage 5: Player-Level Simulation Density Plot
# =============================================================================

plot_player_sims <- function(player_code, player_name = NULL) {
  
  # Use receiver code as name if full name not supplied
  if (is.null(player_name)) player_name <- player_code
  
  # Filter simulation draws and summary for the selected player
  player_sims <- sim_df %>%
    filter(receiver == player_code)
  
  player_summary <- summary_df %>%
    filter(receiver == player_code)
  
  if (nrow(player_sims) == 0) {
    stop("No simulation data found for receiver: ", player_code)
  }
  
  # Build the plot
  p <- ggplot(player_sims, aes(x = sim_tot)) +
    geom_density(fill = "#8B1E3F", color = "#4A0E1F", alpha = 0.55, linewidth = 0.65) +
    
    # Actual result
    geom_vline(data = player_summary, aes(xintercept = actual),
               color = "#1A1A2E", linewidth = 0.85) +
    
    # Expected (mean of simulations)
    geom_vline(data = player_summary, aes(xintercept = expected),
               color = "#1A1A2E", linewidth = 0.65, linetype = "dashed", alpha = 0.75) +
    
    # Labels
    geom_text(
      data = player_summary,
      aes(
        x = actual,
        y = Inf,
        label = paste0(
          number(actual, accuracy = 0.1), " pts\n",
          "Res: ", number(residual, accuracy = 0.1), "\n",
          percentile, "th"
        )
      ),
      vjust = 1.3, size = 2.6, color = "#1A1A2E", lineheight = 0.9
    ) +
    
    facet_wrap(~ week, ncol = 4, scales = "free_y",
               labeller = labeller(week = function(x) paste("Week", x))) +
    
    scale_x_continuous(breaks = pretty_breaks(n = 5),
                       expand = expansion(mult = c(0.02, 0.04))) +
    scale_y_continuous(labels = label_percent(accuracy = 1),
                       expand = expansion(mult = c(0, 0.18))) +
    
    labs(
      title = paste0(player_name, " — Simulated PPR Fantasy Points"),
      subtitle = paste0("Monte Carlo simulations based on target quality (cp × xyac)  |  ",
                        season_year, " Regular Season"),
      x = "PPR Fantasy Points",
      y = NULL,
      caption = "Solid line = actual PPR  |  Dashed line = expected (simulation mean)\nData: nflreadr play-by-play"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, color = "#1A1A2E", margin = margin(b = 4)),
      plot.subtitle = element_text(size = 8.5, color = "#4A4A4A", margin = margin(b = 10)),
      plot.caption  = element_text(size = 7, color = "#666666", hjust = 0, lineheight = 1.1),
      strip.text    = element_text(face = "bold", size = 9, color = "#1A1A2E"),
      strip.background = element_rect(fill = "grey92", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
      axis.title.y  = element_blank(),
      axis.text     = element_text(size = 7.5, color = "#333333"),
      plot.margin   = margin(12, 14, 10, 12),
      panel.spacing = unit(0.55, "lines")
    )
  
  return(p)
}

# =============================================================================
# Opportunity Realization Score (ORS)
# 0.55 × Percentile + 0.45 × SNR Percentile Rank
# =============================================================================

# ----- Parameters ------------------------------------------------------------
min_targets_game   <- 5    # minimum targets required for a game to be ranked
min_targets_season <- 25   # minimum targets required for a season ranking

# =============================================================================
# 1. Game-level ORS (adds column to summary_df)
# =============================================================================

summary_df <- summary_df %>%
  filter(targets >= min_targets_game) %>%
  mutate(
    # Signal-to-noise ratio (protect against division by zero)
    snr = residual / pmax(sd_sim, 0.1)
  ) %>%
  mutate(
    # Convert SNR to a 0–100 scale via percentile rank
    snr_pct = percent_rank(snr) * 100,
    
    # Opportunity Realization Score
    ors = 0.55 * percentile + 0.45 * snr_pct
  ) %>%
  arrange(desc(ors))

message("Game-level summary with ORS created: ", nrow(summary_df), " rows")
message("Top 10 games by ORS:")
summary_df %>%
  select(week, receiver, targets, actual, expected, residual, percentile, sd_sim, ors) %>%
  head(10) %>%
  print()


# =============================================================================
# 2. Full-season ORS (proper aggregation of simulations)
# =============================================================================

message("Building season-level summaries...")

# --- Season-level actuals & target volume ------------------------------------
season_actual <- targets_clean %>%
  mutate(
    PPR_points = 1 + gain / 10 + if_else(gain == yardline_100, 6, 0),
    actual_PPR = if_else(complete_pass == 1, PPR_points, 0)
  ) %>%
  group_by(season, receiver, receiver_id, posteam) %>%
  summarise(
    games      = n_distinct(game_id),
    targets    = n(),
    actual     = sum(actual_PPR, na.rm = TRUE),
    .groups    = "drop"
  )

# --- Season-level simulated totals (sum across games within each sim_id) -----
season_sims <- sim_df %>%
  group_by(season, receiver, receiver_id, posteam, sim_id) %>%
  summarise(season_sim_tot = sum(sim_tot, na.rm = TRUE), .groups = "drop")

# --- Expected value and SD of season simulations -----------------------------
season_sim_summary <- season_sims %>%
  group_by(season, receiver, receiver_id, posteam) %>%
  summarise(
    expected = mean(season_sim_tot),
    sd_sim   = sd(season_sim_tot),
    .groups  = "drop"
  )

# --- Season-level percentile -------------------------------------------------
season_percentile <- bind_rows(
  season_sims %>%
    select(season, receiver, receiver_id, posteam, sim_tot = season_sim_tot) %>%
    mutate(is_actual = 0L),
  season_actual %>%
    transmute(season, receiver, receiver_id, posteam,
              sim_tot = actual, is_actual = 1L)
) %>%
  group_by(season, receiver, receiver_id, posteam) %>%
  mutate(percentile = percent_rank(sim_tot) * 100) %>%
  filter(is_actual == 1L) %>%
  select(season, receiver, receiver_id, posteam, percentile) %>%
  ungroup()

# --- Final season summary with ORS -------------------------------------------
season_summary_df <- season_actual %>%
  left_join(season_sim_summary, by = c("season", "receiver", "receiver_id", "posteam")) %>%
  left_join(season_percentile, by = c("season", "receiver", "receiver_id", "posteam")) %>%
  filter(targets >= min_targets_season) %>%
  mutate(
    residual = actual - expected,
    snr      = residual / pmax(sd_sim, 0.1),
    snr_pct  = percent_rank(snr) * 100,
    ors      = 0.55 * percentile + 0.45 * snr_pct
  ) %>%
  select(
    season, receiver, receiver_id, posteam,
    games, targets, actual, expected, residual,
    percentile, sd_sim, snr, ors
  ) %>%
  arrange(desc(ors))

message("Season-level summary created: ", nrow(season_summary_df), " players")
message("Top 15 players by Season ORS:")
season_summary_df %>%
  select(receiver, games, targets, actual, expected, residual, percentile, ors) %>%
  head(15) %>%
  print()