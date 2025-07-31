
library(dplyr)
tiebreaks <- readRDS("data/tiebreaks.RDS")

tb_players <- tiebreaks |>
    group_by(player) |>
    summarise(won = sum(!is.na(advancement)), lost = sum(is.na(advancement))) |>
    mutate(played = won + lost) |>
    arrange(-played) |>
    print()

tb_advancements <- tiebreaks |>
    filter(!is.na(advancement)) |>
    group_by(scheduled_time, advancement) |>
    count() |>
    arrange(-n) |>
    print()

