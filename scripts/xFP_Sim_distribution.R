library(nflfastR)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(ggthemes)
library(tidyverse)
library(progress)

pbp <- load_pbp(2025) |>
  filter(week >= 1 & week <= 18)

fant_pt_dist_df <- pbp %>% 
  filter(pass_attempt==1 & season_type=='REG' & two_point_attempt==0 & !is.na(receiver_id) & receiver == 'M.Wilson' & week <= 18) %>% 
  select(season = season, week, game_id, play_id, posteam = posteam, receiver, yardline_100 = yardline_100, air_yards = air_yards, actual_yards_gained = yards_gained, complete_pass, cp, yac_prob = xyac_success, gain = yards_gained) %>% 
  mutate(
    gain = ifelse(yardline_100==air_yards, yardline_100, gain),
    yac_prob = ifelse(yardline_100==air_yards, 1, yac_prob),
    PPR_points = 1 + gain/10 + ifelse(gain == yardline_100, 6, 0),
    catch_run_prob = cp * yac_prob,
    exp_PPR_points = PPR_points * catch_run_prob,
    actual_outcome = ifelse(actual_yards_gained==gain & complete_pass==1, 1, 0),
    actual_PPR_points = ifelse(actual_outcome==1, PPR_points, 0),
    target = 0,
    game_played = 0
  )

incomplete_df <- fant_pt_dist_df %>% 
  mutate(
    gain = 0,
    PPR_points = 0,
    yac_prob = 0,
    exp_PPR_points = 0,
    complete_pass = 0,
    catch_run_prob = 1 - cp,
    actual_outcome = NA,
    actual_PPR_points = NA,
    target = 1
  ) %>% 
  distinct %>% 
  group_by(game_id, receiver) %>% 
  mutate(game_played = ifelse(row_number()==1,1,0)) %>% 
  ungroup

# make a data frame to loop around
sampling_df <- rbind(incomplete_df, fant_pt_dist_df) %>% 
  select(season, week, game_id, play_id, posteam, receiver, catch_run_prob, PPR_points) %>% 
  group_by(game_id, play_id)

# do sim
sim_df <- do.call(rbind, lapply(1:10000, function(x) {
  sampling_df %>% 
    mutate(sim_res = sample(PPR_points, 1, prob = catch_run_prob)) %>% 
    select(season, week, game_id, play_id, posteam, receiver, sim_res) %>% 
    distinct %>% 
    group_by(game_id, week, posteam, receiver) %>% 
    summarize(sim_tot = sum(sim_res, na.rm = T), .groups = 'drop') %>% 
    return
}))

sim_df <- sim_df %>% mutate(sim = 1)

# calculate how many points were actually scored
actual_df <- fant_pt_dist_df %>%
  group_by(game_id, week, posteam, receiver) %>% 
  summarize(sim_tot = sum(actual_PPR_points, na.rm = T), .groups = 'drop') %>% 
  mutate(sim = 0)

# figure out what percentile the actual values fall in
percentile_df <- rbind(sim_df, actual_df) %>% 
  group_by(game_id, week, posteam, receiver) %>% 
  mutate(perc = percent_rank(sim_tot)) %>% 
  filter(sim == 0)

library(scales)


ggplot(data = sim_df, aes(x = sim_tot, group = game_id, color = game_id, fill = game_id)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  geom_vline(data = percentile_df, 
             aes(xintercept = sim_tot), 
             color = "darkblue", linewidth = 0.7, linetype = "dashed", 
             show.legend = FALSE) +
  geom_label(data = percentile_df,
             aes(x = sim_tot, y = Inf,
                 label = paste0("Actual\n", game_id, "\n",
                                number(round(perc * 100, 2), accuracy = 0.1), "%")),
             size = 2.4, fill = "grey98", color = "darkblue",
             vjust = 1.4, hjust = 0.5, label.padding = unit(0.15, "lines"),
             show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.12))) +
  scale_color_manual(values = c("#ff7f00", "#9932cc", "red", "blue", "deeppink", "darkorange",
                                "aquamarine", "azure", "bisque", "brown", "blueviolet", "cadetblue",
                                "cyan3", "darksalmon", "deepskyblue", "darkred", "darkgreen")) +
  scale_fill_manual(values = c("#ff7f00", "#9932cc", "red", "blue", "deeppink", "darkorange",
                               "aquamarine", "azure", "bisque", "brown", "blueviolet", "cadetblue",
                               "cyan3", "darksalmon", "deepskyblue", "darkred", "darkgreen")) +
  labs(title = "Michael Wilson Expected PPR Fantasy Point Distribution",
       subtitle = "Based on 10,000 Simulations",
       y = "Density",
       x = "Expected PPR Fantasy Points",
       color = NULL,
       fill = NULL) +
  facet_wrap(~ week, ncol = 4, scales = "free_y",
             labeller = labeller(week = function(x) paste0("Week\n", x))) +
  theme(
    line = element_line(lineend = "round", color = "darkblue"),
    text = element_text(color = "darkblue"),
    plot.background = element_rect(fill = "grey95", color = "transparent"),
    panel.border = element_rect(color = "darkblue", fill = NA),
    panel.background = element_rect(fill = "white", color = "transparent"),
    axis.ticks = element_line(color = "darkblue", linewidth = 0.5),
    axis.ticks.length = unit(2.75, "pt"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7, color = "darkblue"),
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.background = element_rect(fill = "grey90", color = "darkblue"),
    strip.text = element_text(size = 8, color = "darkblue", face = "bold"),
    panel.spacing = unit(0.6, "lines")
  )
