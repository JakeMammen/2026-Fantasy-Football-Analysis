library(tidyverse)
library(gt)
library(nflreadr)
library(scales)

# ------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------
pbp_r <- load_pbp(2025)
pbp_r_p <- pbp_r |>
  filter(
    play_type == "pass",
    !is.na(air_yards),
    !is.na(passer_id),
    season_type == "REG",
    week %in% 1:18
  ) |>
  mutate(
    player_name = case_when(
      id == "00-0030565" ~ "G.Smith",
      id == "00-0036212" ~ "T.Tagovailoa",
      id == "00-0026158" ~ "J.Flacco",
      id == "00-0036945" ~ "J.Fields",
      id == "00-0029604" ~ "K.Cousins",
      TRUE ~ fantasy_player_name
    )
  )

adot_tbl <- pbp_r_p |>
  group_by(passer_id, passer) |>
  summarise(
    n = n(),
    adot = mean(air_yards, na.rm = TRUE),
    team = names(which.max(table(posteam))),
    .groups = "drop",
    team = max(case_when(
      id == "00-0030565" ~ "NYJ",
      id == "00-0036212" ~ "ATL",
      id == "00-0026158" ~ "CIN",
      id == "00-0036945" ~ "KC",
      id == "00-0029604" ~ "LV",
      TRUE ~ team
    ), na.rm = TRUE)
  ) |>
  filter(
    n > 300,
    !is.na(passer),
    passer != "R.Wilson"
  ) |>
  arrange(desc(adot)) |>
  mutate(rank = row_number()) |>
  select(rank, team, passer, n, adot)

# Load team colors and create colored abbreviations
team_info <- teams_colors_logos |>
  select(team_abbr, team_color) |>
  mutate(team_abbr = ifelse(team_abbr == "WSH", "WAS", team_abbr))

adot_tbl <- adot_tbl |>
  left_join(team_info, by = c("team" = "team_abbr")) |>
  mutate(
    team = paste0(
      "<span style='color:", team_color, "; font-weight: bold;'>", team, "</span>"
    )
  ) |>
  select(rank, team, passer, n, adot)

# ------------------------------------------------------------------
# gt table with matching aesthetics
# ------------------------------------------------------------------
adot_tbl <- adot_tbl |>
  gt() |>
  tab_header(
    title = md("**2025 Average Depth of Target (ADOT) – Qualified Quarterbacks**"),
    subtitle = md("Regular season (Weeks 1–18) only | Minimum 300 pass attempts")
  ) |>
  cols_move_to_start(columns = rank) |>
  cols_label(
    rank = "Rank",
    team = "Team",
    passer = "Quarterback",
    n = "# Passes",
    adot = "ADOT"
  ) |>
  fmt_number(columns = adot, decimals = 1) |>
  fmt_number(columns = n, decimals = 0, sep_mark = ",") |>
  fmt_markdown(columns = team) |>
  tab_style(
    style = cell_text(size = "x-large"),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = cell_text(align = "center", size = "medium"),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = passer)
  ) |>
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) |>
  data_color(
    columns = adot,
    colors = scales::col_numeric(
      palette = c("grey97", "#E03FD8"),
      domain = range(adot_tbl$adot, na.rm = TRUE)
    ),
    autocolor_text = FALSE
  ) |>
  tab_options(
    table.font.color = "darkblue",
    data_row.padding = "2px",
    row_group.padding = "3px",
    column_labels.border.bottom.color = "darkblue",
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = "darkblue",
    row_group.border.top.width = 1.5,
    row_group.border.top.color = "#999999",
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = "#999999",
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = "darkblue",
    table.border.top.color = "transparent",
    table.background.color = "#F2F2F2",
    table.border.bottom.color = "transparent",
    source_notes.background.color = "#F2F2F2",
    row.striping.background_color = "#FFFFFF",
    row.striping.include_table_body = TRUE
  ) |>
  tab_source_note(
    source_note = html(
      paste0(
        "<div style='display: flex; align-items: center; justify-content: space-between; width: 100%;'>
         <div style='font-size: 12px;'>
           <b>Data:</b> nflreadr | <b>Created by:</b> @jakemammen
         </div>",
        local_image(
          filename = "/Users/jakemammen/Developer/2026_Fantasy_Football_Analysis/logos/Graph_logo2.png",
          height = 30
        ),
        "</div>"
      )
    )
  )

adot_tbl

gtsave(adot_tbl, 
       filename = "output/tables/adot_tbl.png")