
library(dplyr)
library(ggplot2)
source("theme.R")

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

tb_short <- tiebreaks |>
    group_by(match_id, scheduled_time) |>
    summarise(
        n_players = n(),
        duration = duration[1],
        
    ) |>
    arrange(-duration) |>
    print()

duration_plot <- ggplot(tb_short, aes(x = scheduled_time, y = duration)) +
    geom_point(position = position_jitter(width = 10, height = 0)) +
    scale_x_time(name = "Time", labels = format_hms(), breaks = 12 * 60 * 1:5) +
    scale_y_time(name = "Duration", labels = format_hms(h = FALSE)) +
    ggtitle("Tiebreak Durations") +
    theme_most()

plot(duration_plot)
