
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")

eliminations <- timelines |>
    distinct(match_id, player, standing) |>
    group_by(match_id, player, standing) |>
    reframe(elimination = 1:5, time = hms::hms(12 * 60 * 1:5)) |>
    filter(standing < 8 - elimination) |>
    rowwise() |>
    group_map(function(x, ...) {
        new_cols <- timelines |>
            filter(match_id == x$match_id, player == x$player, time <= x$time) |>
            summarise(
                n_advancements = last(n_advancements), 
                died = any(ranked_event == "death", na.rm = TRUE)
            )
        bind_cols(x, new_cols)
    }) |>
    bind_rows() |>
    arrange(match_id, elimination, standing)

differences <- eliminations |>
    group_by(match_id, elimination, time) |>
    reframe(
        eliminated = 7 - elimination[1L],
        survivor = 1:(eliminated - 1),
        difference = n_advancements[survivor] - n_advancements[eliminated],
        eliminated_died = died[eliminated]
    )

difference_summary <- differences |>
    # filter(!eliminated_died) |>
    group_by(elimination, time, eliminated, survivor) |>
    summarise(
        mean = mean(difference),
        median = round(median(difference)),
        tiebreak_prop = mean(difference == 0L),
        p25 = quantile(difference, .25),
        p75 = quantile(difference, .75)
    ) |>
    print()

line_color <- scale_most[5] |> 
    scales::col_darker(10) |> 
    scales::col_saturate(-20)

difference_plot <- difference_summary |>
    ggplot(aes(x = time, y = survivor)) +
    annotate("segment", color = line_color, linewidth = 0.9, x = 12 * 60 * 1:5, y = 6:2, yend = 1) +
    annotate("point", color = line_color, size = 4, x = 12 * 60 * 1:5, y = 6:2) +
    geom_point(aes(size = mean, color = mean)) +
    geom_text(aes(label = format(mean, nsmall = 1, digits = 1)), color = "white", family = "bold") +
    geom_text(aes(label = round(tiebreak_prop*100) |> paste0("%")), 
              hjust = 0, nudge_x = 60 * 4, alpha = 0.6, size = 3, family = "bold") +
    scale_x_time(name = "", labels = ~format_hms(.x, s = FALSE),
                 breaks = 12 * 60 * (1:5), minor_breaks = NULL, limits = 60 * c(10, 65)) +
    scale_y_reverse(name = "Standings", breaks = 1:6, limits = c(6.2, 0.6), labels = format_standings) +
    scale_color_gradient(low = scale_most[2], high = scale_most[5]) +
    scale_size(range = c(12, 19)) +
    ggtitle("Average Margin of Elimination", 
            "Advancement difference between eliminated player and survivors \nPercentage of tiebreaks") +
    theme_most() +
    theme(
        aspect.ratio = 0.88,
        panel.grid = element_blank(),
        legend.position = "none"
    )

plot(difference_plot)

