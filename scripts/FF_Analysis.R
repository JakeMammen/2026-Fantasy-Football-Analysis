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
  

ggsave(filename = "output/graphs/2025_OffandDef_EPA.png",
       plot     = epa,
       width    = 10,
       height   = 6,
       dpi      = 300,
       units    = "in")

best_off <- c("LA", "GB", "NE", "BUF")

combined |>
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

best_def <- c("SEA", "HOU", "MIN", "CLE")

combined |>
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

nfc_north <- c("GB", "DET", "CHI", "MIN")

combined |>
  dplyr::mutate(
    colour = ifelse(team %in% nfc_north, NA, "b/w"),
    alpha = ifelse(team %in% nfc_north, 0.9, 0.2)
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

ggplot2::ggplot(qbs, aes(x = reorder(id, -qb_epa), y = qb_epa)) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  geom_text(aes(label = name), vjust = -0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2025 NFL Quarterback EPA per Play",
    subtitle = "Data: @nflfastR",
    y = "EPA/play",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of gsis ids with player headshots
    axis.text.x.bottom = nflplotR::element_nfl_headshot(size = 1)
  )