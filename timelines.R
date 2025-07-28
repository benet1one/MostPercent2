
library(dplyr)
matches <- readRDS("data/matches.RDS")

advancement_timeline <- function(match_id, timeline, standings, timepoints = seq(0, 3600, 10)) {
    st <- standings |>
        select(player) |>
        mutate(standing = 6:1)
    
    tl <- timeline |>
        filter(is.na(ranked_event)) |>
        left_join(st)
    
    expand.grid(standing = 1:6, time = timepoints) |>
        tibble() |>
        rowwise() |>
        mutate(n_advancements = advancements_by(tl, standing, time)) |>
        mutate(match_id = match_id, .before = 1L) |>
        ungroup()
}

advancements_by <- function(timeline, standing, time) {
    timeline |>
        filter(standing == !!standing, round(time, 1) <= !!time) |>
        nrow()
}


