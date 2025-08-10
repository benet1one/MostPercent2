
library(dplyr)

points <- readRDS("data/points.RDS") |>
    group_by(matchup) |>
    mutate(
        winner_points = max(points),
        winner_advancements = total_advancements[which.max(points)],
        most_advancements = max(total_advancements),
        range_points = range(points) |> diff(),
    )

points |> arrange(-winner_points)
points |> arrange(+range_points)

points |> arrange(-total_advancements)
points |> arrange(-most_advancements)

points |> filter(winner_advancements != most_advancements)
