
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")

timelines_without_end_again_death <- timelines |>
    group_by(match_id, player) |>
    mutate(end_again = pmax(advancement == "The End... Again...", FALSE, na.rm = TRUE)) |>
    mutate(done_end_again = cummax(end_again)) |>
    rowwise() |>
    filter(!isTRUE(ranked_event == "death"  &  done_end_again)) |>
    ungroup()

eliminations <- timelines |>
    distinct(match_id, player, standing) |>
    group_by(match_id, player, standing) |>
    reframe(elimination = 1:5, time = hms::hms(12 * 60 * 1:5)) |>
    filter(standing < 8 - elimination) |>
    rowwise() |>
    group_map(function(x, ...) {
        new_cols <- timelines_without_end_again_death |>
            filter(match_id == x$match_id, player == x$player, time <= x$time) |>
            summarise(
                n_advancements = last(n_advancements), 
                died = any(ranked_event == "death", na.rm = TRUE)
            )
        bind_cols(x, new_cols)
    }) |>
    bind_rows() |>
    group_by(match_id, elimination) |>
    arrange(-n_advancements, standing) |>
    mutate(temp_standing = 1:n(), .before = standing) |>
    arrange(match_id, elimination, temp_standing)

differences <- eliminations |>
    arrange(match_id, time, temp_standing) |>
    group_by(match_id, elimination, time) |>
    reframe(
        eliminated = 7 - elimination[1L],
        survivor = 1:(eliminated - 1),
        difference = n_advancements[survivor] - n_advancements[eliminated],
        eliminated_died = died[eliminated]
    )

difference_summary <- differences |>
    group_by(elimination, time, eliminated, survivor) |>
    summarise(
        mean = mean(difference),
        median = round(median(difference)),
        tiebreak_prop = mean(difference == 0L),
        p25 = quantile(difference, .25),
        p75 = quantile(difference, .75),
        .groups = "drop"
    )

line_color <- scale_most[5] |> 
    scales::col_darker(10) |> 
    scales::col_saturate(-20)

difference_plot <- difference_summary |>
    ggplot(aes(x = time, y = survivor)) +
    annotate("segment", color = line_color, linewidth = 0.9, x = 12 * 60 * 1:5, y = 6:2, yend = 1) +
    annotate("point", color = line_color, size = 4, x = 12 * 60 * 1:5, y = 6:2) +
    geom_point(aes(size = mean, color = mean)) +
    geom_text(aes(label = format(mean, nsmall = 1, digits = 1)), color = "white", family = "bold") +
    geom_text(aes(label = format_percentage(tiebreak_prop, 0)), 
              hjust = 0, nudge_x = 60 * 4, alpha = 0.6, size = 3, family = "bold") +
    scale_x_time(name = "", labels = format_hms(s = FALSE),
                 breaks = 12 * 60 * (1:5), minor_breaks = NULL, limits = 60 * c(9, 65)) +
    scale_y_reverse(name = "Standings\nAt Elimination", labels = format_standings,
                    breaks = 1:6, limits = c(6.2, 0.6)) +
    scale_color_gradient(low = scale_most[2], high = scale_most[5]) +
    scale_size(range = c(12, 19)) +
    ggtitle("Average Margin of Elimination", 
            "Advancement difference between eliminated player and survivors \nand percentage of tiebreaks") +
    theme_most() +
    theme(
        aspect.ratio = 0.88,
        panel.grid = element_blank(),
        legend.position = "none"
    )

plot(difference_plot)
save_png(difference_plot, "plots/difference.png")

elimination_summary <- eliminations |>
    group_by(standing, elimination) |>
    summarise(
        time = first(time),
        mean = mean(n_advancements),
        mean_fd = mean(n_advancements[!died]),
        median = median(n_advancements),
        median_fd = median(n_advancements[!died]),
        death_prop = mean(died),
        .groups = "drop"
    )
    
elimination_plot <- elimination_summary |>
    ggplot(aes(x = time, y = standing)) +
    annotate("segment", color = line_color, linewidth = 0.5, x = 12 * 60 * 1:5, y = 6:2, yend = 1) +
    geom_point(aes(size = mean, color = mean)) +
    geom_text(aes(label = round(median)), color = "white", family = "bold") +
    geom_text(aes(label = format_percentage(death_prop, 0)), 
              hjust = 0, nudge_x = 60 * 4, alpha = 0.6, size = 3, family = "bold") +
    scale_x_time(name = "", labels = format_hms(s = FALSE),
                 breaks = 12 * 60 * (1:5), minor_breaks = NULL, limits = 60 * c(9, 65)) +
    scale_y_reverse(name = "Final Standings", labels = format_standings,
                    breaks = 1:6, limits = c(6.2, 0.6)) +
    scale_color_gradient(low = scale_most[2], high = scale_most[5]) +
    scale_size(range = c(12, 19)) +
    ggtitle("Median Advancements", "and percentage of players who died") +
    theme_most() +
    theme(
        aspect.ratio = 0.88,
        panel.grid = element_blank(),
        legend.position = "none"
    )

plot(elimination_plot)
save_png(elimination_plot, "plots/eliminations.png")


eliminations_csv <- eliminations |>
    mutate(time_ms = as.numeric(time) * 1000)

write.csv(eliminations_csv, file = "data/eliminations.csv")
saveRDS(eliminations, file = "data/eliminations.RDS")
