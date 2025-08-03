
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")
standings <- readRDS("data/standings.RDS")

players <- unique(timelines$player) |> setdiff("OliverSR") |> sort()
matches_played <- table(standings$player)

early_advancements <- timelines |> 
    group_by(match_id, player) |>
    filter(time < 12 * 60, !is.na(advancement), !duplicated(advancement)) |>
    ungroup() |>
    count(advancement) |>
    filter(n > 2000 / length(unique(timelines$match_id))) |>
    _$advancement |>
    unname()

player_advancements <- timelines |>
    mutate(advancement = factor(advancement, levels = early_advancements)) |>
    group_by(match_id, player) |>
    filter(!is.na(advancement), !duplicated(advancement)) |>
    group_by(player, advancement, .drop = FALSE) |>
    summarise(
        mean_time = mean(time) |> hms::as_hms(),
        median_index = median(n_advancements),
        n = n(),
        prop = ifelse(n > 0, n/matches_played[player[1]], 0),
        .groups = "drop"
    ) |>
    group_by(advancement) |>
    arrange(advancement) 

player_distance <- function(p1, p2) {
    deltas <- player_advancements |>
        filter(player == p1 | player == p2) |>
        summarise(
            delta_index = ifelse(anyNA(median_index), 0, diff(median_index)/40) |> abs(),
            delta_prop = diff(prop) |> abs(),
            delta = delta_index + delta_prop,
            .groups = "drop"
        )
    
    sum(deltas$delta)
}

player_grid <- combn(sort(players), 2) |>
    t() |>
    as_tibble(.name_repair = "minimal") |>
    setNames(c("p1", "p2")) |>
    mutate(p1 = factor(p1, levels = players), p2 = factor(p2, levels = players)) |>
    rowwise() |>
    mutate(delta = player_distance(p1, p2))

distance <- player_grid |>
    reshape2::acast(p2 ~ p1, value.var = "delta", drop = FALSE) |>
    as.dist()

clustering <- hclust(distance, method = "ward.D2")
plot(clustering)

player_advancements <- player_advancements |>
    mutate(cluster = factor(cutree(clustering, k = 3)[player])) |>
    left_join(standings, by = "player")

rsq <- function(formula) {
    lm(formula) |> summary() |> _$r.squared |> suppressWarnings()
}

discriminant <- player_advancements |>
    group_by(advancement) |>
    summarise(
        index_rsq = rsq(median_index ~ cluster),
        prop_rsq = rsq(prop ~ cluster),
        sum_rsq = index_rsq + prop_rsq
    ) |>
    arrange(-index_rsq)
