library(tidyverse)
library(dplyr)
library(ggimage)
library(nflfastR)

pbp <- load_pbp(2025) |>
  filter(week >= 1 & week <= 18)

rosters <- load_rosters(2025) %>%
  select(season, gsis_id, full_name, position, team) %>%
  distinct()

rosters <- rosters %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))

pbp <- pbp %>%
  filter(season_type == "REG", down <= 4, play_type != "no_play") %>%
  left_join(rosters, by = c("passer_id" = "gsis_id")) %>%
  rename(
    passer_full_name = full_name,
    passer_position = position
  ) %>%
  left_join(rosters, by = c("receiver_id" = "gsis_id")) %>%
  rename(
    receiver_full_name = full_name,
    receiver_position = position
  ) %>%
  left_join(rosters, by = c("rusher_id" = "gsis_id")) %>%
  rename(
    rusher_full_name = full_name,
    rusher_position = position
  ) %>%
  mutate(
    ten_zone_rush = if_else(yardline_100 <= 10 & rush_attempt == 1, 1, 0),
    ten_zone_pass = if_else(yardline_100 <= 10 & pass_attempt == 1 & sack == 0, 1, 0),
    ten_zone_rec = if_else(yardline_100 <= 10 & complete_pass == 1, 1, 0),
    field_touch = case_when(
      yardline_100 <= 100 & yardline_100 >= 81 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_100_81",
      yardline_100 <= 80 & yardline_100 >= 61 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_80_61",
      yardline_100 <= 60 & yardline_100 >= 41 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_60_41",
      yardline_100 <= 40 & yardline_100 >= 21 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_40_21",
      yardline_100 <= 20 & yardline_100 >= 0 & (rush_attempt == 1 | complete_pass == 1) ~ "touch_20_1",
      TRUE ~ "other"
    )
  )

rb_touches <- pbp %>%
  filter(rusher_position == "RB") %>%
  group_by(
    rusher_full_name,
    rusher_player_id,
    field_touch
  ) %>%
  summarize(touches = n())

rb_touches <- rb_touches %>%
  group_by(rusher_full_name, rusher_player_id) %>%
  mutate(
    total_touches = sum(touches),
    pct_touches = touches / total_touches
  ) %>%
  filter(total_touches >= 100)

rb_touches_2 <- rb_touches %>%
  filter(field_touch == "touch_20_1") %>%
  select(rusher_full_name, rusher_player_id, pct_touches)

rb_touches <- left_join(rb_touches,
                        rb_touches_2,
                        by = c(
                          "rusher_full_name" = "rusher_full_name",
                          "rusher_player_id" = "rusher_player_id"
                        )
)

library(RColorBrewer)
rb_touches$field_touch <- as.factor(rb_touches$field_touch)
rb_touches$field_touch <- factor(rb_touches$field_touch, levels = c("touch_20_1", "touch_40_21", "touch_60_41", "touch_80_61", "touch_100_81"))

colors <- brewer.pal(name = "RdYlGn", n = nlevels(rb_touches$field_touch))
names(colors) <- rev(levels(rb_touches$field_touch))

ggplot() +
  geom_col(
    data = rb_touches,
    aes(x = pct_touches.x, y = reorder(rusher_full_name, pct_touches.y), fill = field_touch)
  ) +
  scale_fill_manual(
    values = colors,
    limits = c("touch_100_81", "touch_80_61", "touch_60_41", "touch_40_21", "touch_20_1"), labels = c("100 to 81 yds", "80 to 61 yds", "60 to 41 yds", "40 to 21 yds", "20 to 1 yds")
  ) +
  labs(
    x = "Percent of plays",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
    title = "RB touch % based on how far away from the goal line the touch was (min. 100 touches):\nChris Rodriguez & Zach Charbonnet lead the league in % of touches in the red zone in 2025",
    subtitle = "Regular Season | Data: @nflfastR"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0.01)
  ) +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

rb_hvt <- pbp %>%
  filter(rusher_position == "RB") %>%
  group_by(
    rusher_full_name,
    rusher_player_id,
    posteam
  ) %>%
  summarize(
    rush_attempts = sum(rush_attempt),
    ten_zone_rushes = sum(ten_zone_rush),
    receptions = sum(complete_pass),
    total_touches = rush_attempts + receptions,
    hvts = receptions + ten_zone_rushes,
    non_hvts = total_touches - hvts,
    hvt_pct = hvts / total_touches,
    non_hvt_pct = non_hvts / total_touches
  )

rb_hvt <- rb_hvt %>%
  pivot_longer(cols = c(hvt_pct, non_hvt_pct), names_to = "hvt_type", values_to = "touch_pct")

hvt_lookup <- rb_hvt %>%
  filter(hvt_type == "hvt_pct") %>%
  select(rusher_full_name, rusher_player_id, hvt_type, touch_pct)

rb_hvt <- left_join(rb_hvt,
                    hvt_lookup,
                    by = c(
                      "rusher_full_name" = "rusher_full_name",
                      "rusher_player_id" = "rusher_player_id"
                    )
)

rb_hvt <- left_join(rb_hvt,
                    teams_colors_logos,
                    by = c("posteam" = "team_abbr")
) %>%
  filter(total_touches >= 100, hvt_type.x == "hvt_pct")

ggplot() +
  geom_col(
    data = rb_hvt,
    aes(x = touch_pct.x, y = reorder(rusher_full_name, touch_pct.x)), fill = rb_hvt$team_color
  ) +
  geom_text() +
  labs(
    x = "Percent of Total Touches",
    fill = "Distance from Goal Line",
    caption = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
    title = "Visualization of TRAP backs, displaying RB high value touches (carries inside the 10\nand catches) as a % of total touches (min 100 touches)",
    subtitle = "Regular Season | Data: @nflfastR"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.22),
    expand = c(0, 0)
  ) +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0"),
    plot.caption = ggpath::element_path(hjust = 1, size = 1.0),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
