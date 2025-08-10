
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")

breakpoints <- timelines |>
    distinct(match_id, player, standing) |>
    group_by(match_id, player, standing) |>
    reframe(time = hms::hms(seq(0, 3600, 2*60)[-1L])) |>
    rowwise() |>
    group_map(function(x, ...) {
        n_adv <- timelines |>
            filter(match_id == x$match_id, player == x$player, time <= x$time) |>
            _$n_advancements |>
            last()
        bind_cols(x, n_advancements = n_adv)
    }) |>
    bind_rows() |>
    group_by(match_id, time) |>
    arrange(-n_advancements, standing) |>
    mutate(temp_standing = 1:n(), .before = standing) |>
    arrange(match_id, time, temp_standing)

comebacks <- breakpoints |>
    ungroup() |>
    mutate(delta = abs(standing - temp_standing)) |>
    mutate(delta = cut(delta, breaks = c(0:3, 6), right = FALSE, labels = c(
        "No Difference", "±1", "±2", "±3 or More"
    ))) |>
    count(time, delta, .drop = FALSE) |>
    group_by(time) |>
    mutate(prop = n / sum(n))

comeback_plot <- ggplot(comebacks, aes(x = time, y = prop, fill = delta)) +
    geom_area(stat = "smooth", span = 0.36) +
    scale_x_time(
        name = "Time", labels = format_hms(s = FALSE),
        limits = range(comebacks$time), breaks = 12 * 60 * 1:6, 
    ) +
    scale_y_continuous(name = "Probability", breaks = c(0.0, 0.5, 1.0)) +
    scale_fill_manual(name = "Difference in Standing", values = rev(scale_most)[c(1, 2, 4, 5)]) +
    theme_most()

plot(comeback_plot)
save_png(comeback_plot, "plots/comebacks.png")


eliminations <- readRDS("data/eliminations.RDS") |>
    rename(elim_standing = temp_standing) |>
    select(match_id, player, elim_standing, elimination) |>
    mutate(was_eliminated = elim_standing == max(elim_standing))

elim_comebacks <- tibble(elimination = 1:5) |>
    group_by(elimination) |>
    reframe(time = seq(12*60 * (elimination - 1), 12*60 * elimination + 1, by = 2*60)) |>
    mutate(time = hms::hms(time)) |>
    right_join(breakpoints, by = "time", relationship = "many-to-many") |>
    left_join(eliminations, by = c("match_id", "player", "elimination")) |>
    group_by(elimination, time, temp_standing) |>
    summarise(prob_eliminated = mean(was_eliminated), .groups = "drop")
    
elim_comeback_plot <- ggplot(elim_comebacks, aes(x = time, y = prob_eliminated, fill = factor(temp_standing))) +
    facet_wrap(~elimination, scales = "free_x", nrow = 1) +
    geom_area() +
    scale_x_continuous(name = "Time (m)", labels = ~./60, breaks = 4*60 * 0:100) +
    scale_y_continuous(name = "Probability of\nBeing Eliminated", breaks = c(0.0, 0.5, 1.0)) +
    scale_fill_manual(name = "Standing At Time", values = rev(scale_most), labels = format_standings) +
    ggtitle("Probability of Being the Next Eliminated", ) + 
    theme_most() +
    theme(panel.spacing = unit(18, "pt"))

plot(elim_comeback_plot)
save_png(elim_comeback_plot, "plots/elim_comebacks.png")
