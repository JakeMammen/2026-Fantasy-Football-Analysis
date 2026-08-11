library(shiny)
library(nflreadr)
library(dplyr)
library(reactable)
library(htmltools)
library(htmlwidgets)

# -------------------------------------------------
# Load & prepare data (executed once at app start)
# -------------------------------------------------
stats_2025 <- load_player_stats(
  seasons = 2025,
  summary_level = "reg"
) %>%
  filter(
    position %in% c("WR", "TE"),
    targets >= 50
  ) %>%
  mutate(
    aDOT = round(receiving_air_yards / targets, 2),
    yac = receiving_yards_after_catch
  ) %>%
  left_join(
    load_teams() %>% select(team_abbr, team_logo_espn),
    by = c("recent_team" = "team_abbr")
  ) %>%
  arrange(desc(aDOT)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    team_logo_espn,
    player_display_name,
    position,
    games,
    targets,
    aDOT
  )

# -------------------------------------------------
# User Interface
# -------------------------------------------------
ui <- fluidPage(
  titlePanel("2025 WR/TE aDOT Leaderboard"),
  reactableOutput("adot_table")
)

# -------------------------------------------------
# Server
# -------------------------------------------------
server <- function(input, output, session) {
  
  output$adot_table <- renderReactable({
    reactable(
      stats_2025,
      columns = list(
        rank = colDef(
          name = "Rank",
          align = "center",
          width = 60
        ),
        team_logo_espn = colDef(
          name = "Team",
          align = "center",
          width = 55,
          cell = function(value) {
            img(
              src = value,
              style = "height: 28px; width: auto;"
            )
          }
        ),
        player_display_name = colDef(
          name = "Receiver",
          align = "left",
          minWidth = 75
        ),
        position = colDef(
          name = "Pos",
          align = "center",
          width = 55
        ),
        games = colDef(
          name = "Games Played",
          align = "center",
          width = 80
        ),
        targets = colDef(
          name = "Targets",
          align = "center",
          width = 75
        ),
        aDOT = colDef(
          name = "aDOT",
          align = "center",
          width = 70,
          style = function(value) {
            normalized <- (value - min(stats_2025$aDOT)) /
              (max(stats_2025$aDOT) - min(stats_2025$aDOT))
            color <- rgb(0.88, 0.25 + 0.5 * (1 - normalized), 0.85)
            list(background = color, fontWeight = "bold", color = "white")
          }
        )
      ),
      defaultPageSize = 20,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 20, 30, 50),
      filterable = FALSE,      # column-level search/filter boxes removed
      searchable = FALSE,      # global search box disabled
      highlight = TRUE,
      bordered = TRUE,
      striped = TRUE,
      compact = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "6px 4px",
        style = list(
          fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
        ),
        headerStyle = list(
          background = "#f2f2f2",
          color = "darkblue",
          fontWeight = "600",
          borderBottom = "2px solid darkblue"
        )
      )
    )
  })
}

# -------------------------------------------------
# Launch the application
# -------------------------------------------------
shinyApp(ui = ui, server = server)