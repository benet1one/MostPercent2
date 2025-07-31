
library(dplyr)

timelines <- readRDS("data/timelines.RDS")

deaths <- timelines |>
    group_by(match_id, player, standing) |>
    summarise(deaths = sum(ranked_event == "death", na.rm = TRUE))

