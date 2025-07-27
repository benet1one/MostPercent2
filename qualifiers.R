
library(dplyr)
library(flextable)

qual_simple <- read.csv("Qualifiers.csv") |>
    as_tibble() |>
    select(Name, Twitch, starts_with("Round"), Score, Tiebreaker) |>
    mutate(across(Round.1:Tiebreaker, as.integer)) |>
    arrange(-Score, -Tiebreaker) |>
    mutate(Qualified = 1:n() <= 32) |>
    suppressWarnings()

qual <- qual_simple |> mutate(
    Round = select(qual, starts_with("Round")) |> as.matrix() |> unname(),
    .before = Round.1
)

qual <- select(qual, !c(Round.1, Round.2, Round.3))
print(qual)

tab <- gt::gt()