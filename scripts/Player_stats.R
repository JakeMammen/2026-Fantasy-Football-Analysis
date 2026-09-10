
library(nflreadr)
library(nflplotR)
library(dplyr)
library(ggplot2)

options(nflreadr.verbose = FALSE)

highlight_weeks <- c(6, 7, 17, 18)

stats <- load_player_stats(seasons = 2025)

michael_mayer <- stats |>
  filter(
    player_display_name == "Michael Mayer",
    season_type == "REG"
  ) |>
  mutate(
    week = as.integer(week),
    target_share = coalesce(target_share, 0),
    ppr_label = sprintf("%.1f", fantasy_points_ppr),
    tgt_label = scales::percent(target_share, accuracy = 0.1),
    highlight = week %in% highlight_weeks
  )

mayer_reg <- michael_mayer |> filter(!highlight)
mayer_hot <- michael_mayer |> filter(highlight)

avg_ppr <- mean(michael_mayer$fantasy_points_ppr, na.rm = TRUE)
season_total <- sum(michael_mayer$fantasy_points_ppr, na.rm = TRUE)
avg_tgt <- mean(michael_mayer$target_share, na.rm = TRUE)
total_targets <- sum(michael_mayer$targets, na.rm = TRUE)

caption_logo <- "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", size = 10),
    plot.caption = element_path(hjust = 1, size = 1.0),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8)),
    plot.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.background = element_rect(fill = "#F0F0F0", color = NA),
    legend.position = "none"
  )

p_ppr <- ggplot(michael_mayer, aes(x = week, y = fantasy_points_ppr)) +
  geom_hline(
    yintercept = avg_ppr,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.4
  ) +
  geom_col(
    data = mayer_reg,
    aes(fill = team, color = team),
    width = 0.72
  ) +
  geom_col(
    data = mayer_hot,
    fill = "red",
    color = "red",
    width = 0.72
  ) +
  geom_text(
    aes(label = ppr_label),
    vjust = -0.4,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_nfl_logos(
    aes(team_abbr = opponent_team, y = pmin(fantasy_points_ppr, 0) - 1.15),
    width = 0.045
  ) +
  scale_fill_nfl(alpha = 0.85) +
  scale_color_nfl(type = "secondary") +
  scale_x_continuous(breaks = sort(unique(michael_mayer$week))) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0.16, 0.14))
  ) +
  labs(
    title = "Michael Mayer — PPR Fantasy Points by Week",
    subtitle = paste0(
      "2025 regular season  |  ",
      nrow(michael_mayer), " games  |  ",
      sprintf("%.1f", season_total), " total PPR pts  |  ",
      sprintf("%.1f", avg_ppr), " pts/game (dashed)  |  ",
      "Weeks Bowers was out in red  |  ",
      "Data: nflverse / nflreadr"
    ),
    x = "Week",
    y = "PPR Fantasy Points",
    caption = caption_logo
  ) +
  plot_theme

p_tgt <- ggplot(michael_mayer, aes(x = week, y = target_share)) +
  geom_hline(
    yintercept = avg_tgt,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.4
  ) +
  geom_col(
    data = mayer_reg,
    aes(fill = team, color = team),
    width = 0.72
  ) +
  geom_col(
    data = mayer_hot,
    fill = "red",
    color = "red",
    width = 0.72
  ) +
  geom_text(
    aes(label = tgt_label),
    vjust = -0.4,
    size = 3.1,
    fontface = "bold"
  ) +
  geom_nfl_logos(
    aes(team_abbr = opponent_team, y = pmin(target_share, 0) - 0.02),
    width = 0.045
  ) +
  scale_fill_nfl(alpha = 0.85) +
  scale_color_nfl(type = "secondary") +
  scale_x_continuous(breaks = sort(unique(michael_mayer$week))) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0.16, 0.14))
  ) +
  labs(
    title = "Michael Mayer — Target Share by Week",
    subtitle = paste0(
      "2025 regular season  |  ",
      nrow(michael_mayer), " games  |  ",
      total_targets, " targets  |  ",
      scales::percent(avg_tgt, accuracy = 0.1), " avg target share (dashed)  |  ",
      "Weeks Bowers was out in red |  ",
      "Data: nflverse / nflreadr"
    ),
    x = "Week",
    y = "Target Share",
    caption = caption_logo
  ) +
  plot_theme

print(p_ppr)
print(p_tgt)

ggsave(p_ppr,
       filename = "output/graphs/michael_mayer_ppr.png",
       width    = 12,
       height   = 8,
       dpi      = 300,
       units    = "in")

ggsave(p_tgt,
       filename = "output/graphs/michael_mayer_tgt.png",
       width    = 12,
       height   = 8,
       dpi      = 300,
       units    = "in")