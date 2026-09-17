# =============================================================================
# RB high-value touches — 2026
#
# Option A: volume vs HVT rate scatter
# Option D: team committee panels (HVT touches vs all other touches)
#
# HVT = carries inside the opponent 10-yard line + receptions
# =============================================================================
library(tidyverse)
library(nflfastR)
library(nflreadr)
library(ggpath)
library(ggrepel)

# ---- settings ----------------------------------------------------------------
season_year         <- 2026L
min_touches         <- 8L
backfield_positions <- c("RB", "HB", "FB")
excluded_players    <- c("Al-Jay Henderson")
logo_path <- "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/FSP_Logo_ggplot.png"

# ---- helpers -----------------------------------------------------------------
plot_theme <- function() {
  theme(
    plot.title          = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background     = element_rect(fill = "#F0F0F0", color = NA),
    panel.background    = element_rect(fill = "#F0F0F0", color = NA),
    plot.margin         = margin(10, 12, 18, 10),
    plot.caption        = ggpath::element_path(hjust = 1, vjust = 0.5, size = 3),
    legend.position     = "bottom"
  )
}

reorder_within <- function(x, by, within, sep = "___") {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by)
}

scale_y_reordered <- function(...) {
  ggplot2::scale_y_discrete(
    labels = function(x) sub("___.*$", "", x),
    ...
  )
}

# ---- player directory (one row per gsis id) ----------------------------------
player_dir <- bind_rows(
  load_rosters(season_year) %>%
    transmute(gsis_id, player_name = full_name, position, src = "roster"),
  load_players() %>%
    transmute(gsis_id, player_name = display_name, position, src = "players")
) %>%
  filter(!is.na(gsis_id), !is.na(position)) %>%
  arrange(gsis_id, src != "roster") %>%
  distinct(gsis_id, .keep_all = TRUE)

stopifnot(!anyDuplicated(player_dir$gsis_id))

backfield_ids <- player_dir %>%
  filter(position %in% backfield_positions) %>%
  select(gsis_id, player_name, position)

# ---- plays -------------------------------------------------------------------
pbp <- load_pbp(season_year) %>%
  filter(
    season_type == "REG",
    !is.na(down),
    down <= 4,
    play_type != "no_play"
  ) %>%
  mutate(
    rusher_key   = coalesce(rusher_player_id, rusher_id),
    receiver_key = coalesce(receiver_player_id, receiver_id),
    inside_10    = as.integer(!is.na(yardline_100) & yardline_100 <= 10)
  )

rb_rushes <- pbp %>%
  filter(rush_attempt == 1, !is.na(rusher_key)) %>%
  inner_join(backfield_ids, by = c("rusher_key" = "gsis_id")) %>%
  filter(!player_name %in% excluded_players) %>%
  select(player_id = rusher_key, player_name, posteam, inside_10)

rb_receptions <- pbp %>%
  filter(complete_pass == 1, !is.na(receiver_key)) %>%
  inner_join(backfield_ids, by = c("receiver_key" = "gsis_id")) %>%
  filter(!player_name %in% excluded_players) %>%
  select(player_id = receiver_key, player_name, posteam)

# ---- player-level HVT table --------------------------------------------------
rush_summary <- rb_rushes %>%
  group_by(player_id) %>%
  summarise(
    player_name      = first(player_name),
    rush_attempts    = n(),
    inside_10_rushes = sum(inside_10 == 1, na.rm = TRUE),
    .groups          = "drop"
  )

rec_summary <- rb_receptions %>%
  group_by(player_id) %>%
  summarise(
    rec_name   = first(player_name),
    receptions = n(),
    .groups    = "drop"
  )

team_summary <- bind_rows(
  rb_rushes     %>% count(player_id, posteam, name = "n"),
  rb_receptions %>% count(player_id, posteam, name = "n")
) %>%
  group_by(player_id, posteam) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(player_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, posteam)

team_meta <- teams_colors_logos %>%
  select(team_abbr, team_name, team_color, team_color2) %>%
  distinct(team_abbr, .keep_all = TRUE)

rb_hvt <- rush_summary %>%
  full_join(rec_summary, by = "player_id") %>%
  left_join(team_summary, by = "player_id") %>%
  left_join(team_meta, by = c("posteam" = "team_abbr")) %>%
  transmute(
    player_id,
    player_name      = coalesce(player_name, rec_name),
    posteam,
    team_name        = coalesce(team_name, posteam),
    team_color       = coalesce(team_color, "#4D4D4D"),
    rush_attempts    = replace_na(rush_attempts, 0),
    inside_10_rushes = replace_na(inside_10_rushes, 0),
    receptions       = replace_na(receptions, 0),
    total_touches    = rush_attempts + receptions,
    hvts             = inside_10_rushes + receptions,
    other_touches    = (rush_attempts + receptions) - (inside_10_rushes + receptions),
    hvt_pct          = (inside_10_rushes + receptions) / (rush_attempts + receptions)
  ) %>%
  filter(
    !is.na(player_name),
    !is.na(posteam),
    total_touches >= min_touches
  ) %>%
  group_by(player_id) %>%
  slice_max(total_touches, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(player_name) %>%
  slice_max(total_touches, n = 1, with_ties = FALSE) %>%
  ungroup()

stopifnot(all(rb_hvt$hvt_pct <= 1 + 1e-8, na.rm = TRUE))
stopifnot(!anyDuplicated(rb_hvt$player_id))
stopifnot(!anyDuplicated(rb_hvt$player_name))

message("Players plotted: ", nrow(rb_hvt))
message(
  "HVT range: ",
  scales::percent(min(rb_hvt$hvt_pct), 0.1),
  " to ",
  scales::percent(max(rb_hvt$hvt_pct), 0.1)
)

# =============================================================================
# Option A — volume vs HVT rate
# =============================================================================
median_touches <- median(rb_hvt$total_touches)
median_hvt     <- median(rb_hvt$hvt_pct)

option_a <- ggplot(
  rb_hvt,
  aes(x = total_touches, y = hvt_pct)
) +
  geom_vline(
    xintercept = median_touches,
    linetype   = "dashed",
    color      = "grey40",
    linewidth  = 0.4
  ) +
  geom_hline(
    yintercept = median_hvt,
    linetype   = "dashed",
    color      = "grey40",
    linewidth  = 0.4
  ) +
  annotate(
    "text",
    x = -Inf, y = Inf,
    label = "Low volume, high HVT",
    hjust = -0.08, vjust = 1.15,
    size = 3.3, fontface = "bold", color = "grey30", lineheight = 0.95
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "High volume, high HVT",
    hjust = 1.08, vjust = 1.15,
    size = 3.3, fontface = "bold", color = "grey30", lineheight = 0.95
  ) +
  annotate(
    "text",
    x = -Inf, y = -Inf,
    label = "Low volume, low HVT",
    hjust = -0.08, vjust = -0.25,
    size = 3.3, fontface = "bold", color = "grey30", lineheight = 0.95
  ) +
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = "High volume, low HVT",
    hjust = 1.08, vjust = -0.25,
    size = 3.3, fontface = "bold", color = "grey30", lineheight = 0.95
  ) +
  geom_point(
    aes(fill = team_color),
    shape  = 21,
    color  = "white",
    size   = 4.2,
    stroke = 0.7
  ) +
  geom_text_repel(
    aes(label = player_name),
    size               = 2.8,
    family             = "sans",
    seed               = 16,
    max.overlaps       = Inf,
    box.padding        = 0.55,
    point.padding      = 0.65,
    force              = 2.4,
    force_pull         = 0.4,
    min.segment.length = unit(-1, "pt"),
    max.time           = 2,
    max.iter           = 10000,
    segment.size       = 0.5,
  ) +
  scale_fill_identity() +
  scale_x_continuous(
    expand = expansion(mult = c(0.16, 0.18))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.18, 0.20))
  ) +
  labs(
    x        = "Total touches (carries + receptions)",
    y        = "High-value touch rate",
    title    = paste0(
      "RB Workload vs High-value Touch Rate"
    ),
    subtitle = paste(
      "HVT = carries inside the 10 + receptions",
      "| Through Week 1 (min. ", min_touches, " touches)",
      "| Regular season", season_year
    ),
    caption  = logo_path
  ) +
  plot_theme() +
  theme(
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text()
  )

# =============================================================================
# Option D — team committee panels
# =============================================================================
committee_long <- rb_hvt %>%
  transmute(
    player_id,
    player_name,
    posteam,
    hvts,
    `High-value` = hvts,
    `All other`  = other_touches
  ) %>%
  pivot_longer(
    cols      = c(`High-value`, `All other`),
    names_to  = "touch_class",
    values_to = "touches"
  ) %>%
  mutate(
    touch_class = factor(touch_class, levels = c("All other", "High-value")),
    player_axis = reorder_within(player_name, hvts, posteam)
  )

option_d <- ggplot(
  committee_long,
  aes(x = touches, y = player_axis, fill = touch_class)
) +
  geom_col(width = 0.72) +
  facet_wrap(~posteam, scales = "free_y", ncol = 4) +
  scale_fill_manual(
    values = c("All other" = "#B0B0B0", "High-value" = "#1F4E79"),
    name   = NULL
  ) +
  scale_y_reordered() +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    x        = "Touches",
    y        = NULL,
    title    = paste0(
      "Team RB Committees: Who is Getting the High-value Touches"
    ),
    subtitle = paste(
      "High-value = carries inside the 10 + receptions",
      "| Min. ",
      min_touches, " touches",
      "| Through Week 1 - Regular season", season_year
    ),
    caption  = logo_path
  ) +
  plot_theme() +
  theme(
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x    = unit(0.9, "lines"),
    panel.spacing.y    = unit(0.8, "lines"),
    strip.background   = element_rect(fill = "black", color = NA),
    strip.text         = element_text(
      face  = "bold",
      color = "white",
      size  = 11
    ),
    legend.position    = "bottom"
  )

print(option_a)
print(option_d)

output_dir <- file.path("output", "graphs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

ggsave(
  file.path(output_dir, "rb_hvt_volume_scatter.png"),
  option_a,
  width = 12, height = 8, dpi = 320, bg = "#F0F0F0"
)
ggsave(
  file.path(output_dir, "rb_committee_panels.png"),
  option_d,
  width = 16, height = 18, dpi = 320, bg = "#F0F0F0"
)