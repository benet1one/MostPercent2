
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

running_weighted_mean <- function(x, w = 0.6) {
    n <- length(x)
    y <- numeric(n)
    v <- 1 - w
    
    y[1] <- w * x[1] + v * x[2]
    y[n] <- w * x[n] + v * x[n-1]
    
    for (k in 2:(n - 1)) {
        y[k] <- w * x[k] + v/2 * x[k-1] + v/2 * x[k+1]
    }

    y
}

smooth_spline <- function(df, group) {
    df$time <- as.numeric(df$time)
    sfun <- splinefun(df$time, df$smoothed)
    tibble(
        time = seq(min(df$time), max(df$time), by = 10) |> hms::hms(),
        splined = sfun(time) |> pmax(0)
    )
}

winner_comebacks <- breakpoints |>
    ungroup() |>
    mutate(temp_standing = factor(temp_standing)) |>
    filter(standing == 1L) |>
    count(time, temp_standing, .drop = FALSE) |>
    group_by(time) |>
    mutate(prop = n / sum(n)) |>
    group_by(temp_standing) |>
    arrange(time) |>
    mutate(smoothed = running_weighted_mean(prop, w = 0.5)) |>
    mutate(is_split = as.numeric(time) %% (12*60) == 0) |>
    within(smoothed[is_split] <- prop[is_split]) |>
    group_modify(smooth_spline)

winner_comeback_plot <- ggplot(winner_comebacks, aes(x = time, y = splined, fill = temp_standing)) +
    geom_area(position = "fill") +
    scale_x_time(
        name = "Time", labels = format_hms(s = FALSE),
        breaks = 12 * 60 * 1:6, minor_breaks = NULL
    ) +
    scale_y_continuous(name = "Probability\nof Winning", breaks = 0:2 / 2, minor_breaks = NULL) +
    scale_fill_manual(name = "Standing\nAt Time", values = rev(scale_most), labels = format_standings) +
    ggtitle("Can you come back?", "Probability of winning the match by current standing") +
    theme_most()

plot(winner_comeback_plot)
save_png(winner_comeback_plot, "plots/winner_comebacks.png")


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
    summarise(prob_eliminated = mean(was_eliminated), .groups = "drop") |>
    group_by(temp_standing, elimination) |>
    mutate(smoothed = running_weighted_mean(prob_eliminated, w = 0.5)) |>
    mutate(is_split = as.numeric(time) %% (12*60) == 0) |>
    within(smoothed[is_split] <- prob_eliminated[is_split]) |>
    mutate(smoothed = pmax(smoothed, 0, na.rm = TRUE)) |>
    group_modify(smooth_spline)
    
elim_comeback_plot <- ggplot(elim_comebacks, aes(x = time, y = splined, fill = factor(temp_standing))) +
    facet_wrap(~elimination, scales = "free_x", nrow = 1) +
    geom_area(position = "fill") +
    scale_x_continuous(name = "Time (m)", labels = ~./60, breaks = c(2*60, 4*60 * 0:100), minor_breaks = NULL) +
    scale_y_continuous(name = "Probability of\nBeing Eliminated", breaks = c(0.0, 0.5, 1.0)) +
    scale_fill_manual(name = "Standing\nAt Time", values = rev(scale_most), labels = format_standings) +
    ggtitle("Never Give Up", "Probability of being next eliminated") + 
    theme_most() +
    theme(panel.spacing = unit(18, "pt"), strip.text = element_blank())

plot(elim_comeback_plot)
save_png(elim_comeback_plot, "plots/elim_comebacks.png", height = 5)
