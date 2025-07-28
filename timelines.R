
library(dplyr)
matches <- readRDS("data/matches.RDS")

advancement_timeline <- function(match_id, timeline, standings, timepoints = seq(0, 3600, 60)[-1]) {
    expand.grid(player = unique(timeline$player), time = timepoints) |>
        tibble() |>
        rowwise() |>
        mutate(n_advancements = advancements_by(timeline, standing, time)) |>
        mutate(match_id = match_id, .before = 1L) |>
        ungroup() |>
        left_join(select(standings, player, standing))
}

advancements_by <- function(timeline, standing, time) {
    timeline |>
        filter(is.na(ranked_event), 
               standing == !!standing, 
               round(time, 1) <= !!time) |>
        nrow()
}


advancements <- matches |>
    select(match_id, timeline, standings) |>
    rowwise() |>
    mutate(adv_timeline = list(advancement_timeline(match_id, timeline, standings)))