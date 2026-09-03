library(nflplotR)
library(ggplot2)
library(gt)
library(nflreadr)

pbp <- nflreadr::load_pbp(2025) |> 
  dplyr::filter(season_type == "REG") |>
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp |>
  dplyr::group_by(team = posteam) |>
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp |>
  dplyr::group_by(team = defteam) |>
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

combined <- offense |>
  dplyr::inner_join(defense, by = "team")

qbs <- pbp |>
  dplyr::filter(pass == 1 | rush == 1) |>
  dplyr::filter(down %in% 1:4) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) |>
  dplyr::filter(plays > 200) |>
  dplyr::slice_max(qb_epa, n = 10)

epa <- ggplot2::ggplot(combined, aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = off_epa , y0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
    title = "2025 NFL Offensive and Defensive EPA per Play",
    subtitle = "Regular Season | Data: @nflfastR"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  ) +
  ggplot2::scale_y_reverse()
  
epa

ggsave(filename = "output/graphs/2025_OffandDef_EPA.png",
       plot     = epa,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

best_off <- c("LA", "GB", "NE", "BUF")

epa_best_off <- combined |>
  dplyr::mutate(
    colour = ifelse(team %in% best_off, NA, "b/w"),
    alpha = ifelse(team %in% best_off, 0.9, 0.2)
  ) |>
  ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = off_epa , y0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team, alpha = alpha, colour = colour), width = 0.065) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
    title = "2025 NFL Offensive and Defensive EPA per Play",
    subtitle = "Data: @nflfastR"
  ) +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_color_identity() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  ) +
  ggplot2::scale_y_reverse()

epa_best_off

ggsave(filename = "output/graphs/2025_EPA_best_off.png",
       plot     = epa_best_off,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

best_def <- c("SEA", "HOU", "MIN", "CLE")

epa_best_def <- combined |>
  dplyr::mutate(
    colour = ifelse(team %in% best_def, NA, "b/w"),
    alpha = ifelse(team %in% best_def, 0.9, 0.2)
  ) |>
  ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(x0 = off_epa , y0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team, alpha = alpha, colour = colour), width = 0.065) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
    title = "2025 NFL Offensive and Defensive EPA per Play",
    subtitle = "Data: @nflfastR"
  ) +
  ggplot2::scale_alpha_identity() +
  ggplot2::scale_color_identity() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0)
  ) +
  ggplot2::scale_y_reverse()

epa_best_def

ggsave(filename = "output/graphs/2025_EPA_best_def.png",
       plot     = epa_best_def,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

qb_epa <- ggplot2::ggplot(qbs, aes(x = reorder(name, -qb_epa), y = qb_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2025 NFL Quarterback EPA per Play Leaders",
    subtitle = "Regular Season | Data: @nflfastR",
    y = "EPA/play",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
  )

qb_epa

ggsave(filename = "output/graphs/2025_qb_epa.png",
       plot     = qb_epa,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

library(nflreadr)
library(tidyverse)
library(nflplotR)
library(ggplot2)
library(gt)
library(dplyr)
library(ggrepel)

data <- load_player_stats(seasons = 2025)

qb_stats <- data |>
  filter(position == "QB")

head(qb_stats)
class(qb_stats)

qb_fppg <- qb_stats |>
  # Select key columns for readability
  select(season, week, player_id, player_name, position, fantasy_points_ppr) %>%
  group_by(week, player_name) %>%
  # Arrange by week and highest fantasy points descending
  arrange(week, desc(fantasy_points_ppr))

print(qb_fppg)

# 2. Filter for WRs
wr_stats <- data |> filter(position == "WR")

# 3. Summarize and include recent_team for coloring
wr_fdprr <- wr_stats |> 
  select(week, player_id, player_name, team, receiving_first_downs, fantasy_points_ppr, headshot_url) |> 
  group_by(player_name, headshot_url, player_id, team) |> 
  summarize(
    sum_fpts_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
    sum_rec_fd = sum(receiving_first_downs, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  filter(sum_rec_fd >= 30)

# 4. Exploratory calculations
cor(wr_fdprr$sum_rec_fd, wr_fdprr$sum_fpts_ppr)
plot(wr_fdprr$sum_rec_fd, wr_fdprr$sum_fpts_ppr)

# 5. Enhanced plot with team colors and wordmarks
wr_fdandppr <- ggplot(wr_fdprr, aes(x = sum_rec_fd, y = sum_fpts_ppr)) +
  # Replaced geom_point with nflplotR points scaled automatically to team colors
  geom_point(aes(color = team, fill = team), size = 3, shape = 21, stroke = 1) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(type = "primary") +
  geom_text_repel(aes(label = player_name), segment.colour = "gray50", box.padding = 0.5) +
  labs(
    title = tools::toTitleCase("First Down Receptions vs. PPR Fantasy Points"),
    subtitle = "2025 Season | Min. 30 FD Recs | Total | @FantasySPack | Data: nflreadr",
    x = "Total First Down Receptions",
    y = "Total Fantasy Points (PPR)",
    caption = "/Users/jakemammen/Library/CloudStorage/OneDrive-Personal/Desktop/Masters Program/Fantasy_Football/Graph_logo2.png"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    legend.position = "none" # Hides the messy color legend since colors match teams directly
  )

ggsave(filename = "output/graphs/2025_WR_fd_and_ppr.png",
       plot     = wr_fdandppr,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

# 1. Summarize target share (fixed typo na.ram -> na.rm)
wr_tar_share <- wr_stats |> 
  dplyr::group_by(player_id) |> 
  dplyr::summarise(
    name = dplyr::first(player_name),
    team = dplyr::last(team),
    target_share = mean(target_share, na.rm = TRUE)
  ) |> 
  dplyr::slice_max(target_share, n = 10)

# 2. Plotting with names on the x-axis
wr_target_share <- ggplot2::ggplot(wr_tar_share, aes(x = reorder(name, -target_share), y = target_share)) + 
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) + 
  # Removed geom_text since names are now on the axis, but you can keep it to show percentages
  ggplot2::geom_text(aes(label = scales::percent(target_share, accuracy = 0.1)), vjust = -0.5, size = 3.5) + 
  nflplotR::scale_color_nfl(type = "secondary") + 
  nflplotR::scale_fill_nfl(alpha = 0.4) + 
  ggplot2::labs(
    title = "2025 NFL WR Avg Target Share Leaders",
    subtitle = "Data: @nflfastR",
    y = "Target Share",
    caption = "/Users/jakemammen/Library/CloudStorage/OneDrive-Personal/Desktop/Masters Program/Fantasy_Football/Graph_logo2.png"
  ) + 
  ggplot2::theme_minimal() + 
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    # It's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # Rotates text slightly if names overlap, or keep standard text by leaving this blank
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1) 
  )

ggsave(filename = "output/graphs/2025_wr_target_share.png",
       plot     = wr_target_share,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

wr_tar_share_ppr <- wr_stats |> 
  # 1. Group strictly by unique player identifiers
  dplyr::group_by(player_id, player_name, team, headshot_url) |> 
  
  # 2. Calculate the season-wide metrics directly from the weekly rows
  dplyr::summarise(
    avg_target_share   = mean(target_share, na.rm = TRUE),
    total_fpts_ppr     = sum(fantasy_points_ppr, na.rm = TRUE),
    .groups            = "drop"
  ) |>
  filter(avg_target_share >= .15)

cor(wr_tar_share_ppr$avg_target_share, wr_tar_share_ppr$total_fpts_ppr)
plot(wr_tar_share_ppr$avg_target_share, wr_tar_share_ppr$total_fpts_ppr)

library(scales) # Required for labels = percent
library(ggrepel) # Required for geom_text_repel

wr_tar_share_and_ppr <- ggplot(wr_tar_share_ppr, aes(x = avg_target_share, y = total_fpts_ppr)) + 
  # Replaced geom_point with nflplotR points scaled automatically to team colors 
  geom_point(aes(color = team, fill = team), size = 3, shape = 21, stroke = 1) + 
  scale_color_nfl(type = "secondary") + 
  scale_fill_nfl(type = "primary") + 
  
  # 1. FIX: Added ggrepel namespace
  ggrepel::geom_text_repel(aes(label = player_name), segment.colour = "gray50", box.padding = 0.5) + 
  
  # 2. FIX: Format x-axis as a percentage
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  labs( 
    title = tools::toTitleCase("Avg. Target Share vs. PPR Fantasy Points"), 
    subtitle = "2025 Season | @FantasySPack | Data: nflreadr", 
    x = "Average Target Share", 
    y = "Total Fantasy Points (PPR)", 
    caption = "/Users/jakemammen/Library/CloudStorage/OneDrive-Personal/Desktop/Masters Program/Fantasy_Football/Graph_logo2.png" 
  ) + 
  theme_minimal() + 
  theme( 
    plot.title.position = "plot", 
    plot.title = ggplot2::element_text(face = "bold"), 
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"), 
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0), 
    legend.position = "none" # Hides the messy color legend since colors match teams directly 
  )


wr_tar_share_and_ppr

ggsave(filename = "output/graphs/2025_wr_tar_share_and_ppr.png",
       plot     = wr_tar_share_and_ppr,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")
