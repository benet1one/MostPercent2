
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")

example_curves <- expand.grid(
    t = seq(0, 3600, 10),
    beta = 4,
    phi = c(0.6, 1.0, 1.4)
) |>
    mutate(y = 80 - 80 / (beta * (t/3600)^phi + 1))

example_curve_plot <- ggplot(example_curves, aes(x = t, y = y, color = factor(phi))) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(name = "Advancements", minor_breaks = NULL) +
    scale_x_time(
        name = "Time", breaks = 12 * 60 * 1:5, minor_breaks = NULL,
        labels = format_hms(s = FALSE)) +
    scale_color_manual(
        name = quote(phi), values = scale_most[c(6, 4, 3)], 
        labels = ~format(as.numeric(.x), nsmall = 1)
    ) +
    ggtitle("Fit examples", rlang::expr("With " * beta == !!example_curves$beta[1])) +
    theme_most()

plot(example_curve_plot)
save_png(example_curve_plot, "plots/example_curves.png")


timeline_model <- timelines |> 
    filter(is.na(ranked_event), n_advancements > 0) |>
    mutate(
        y = log(80 / (80 - n_advancements) - 1),
        x = log(as.numeric(time) / 3600),
        wp = as.numeric(time) %/% (12 * 60) + 2,
        wt = as.numeric(time) + 60,
        player = factor(player),
        standing = factor(standing)
    )

simple_fit <- glm(y ~ x, weights = wp * wt^2, data = timeline_model)
simple_phi <- coef(simple_fit)["x"] |> unname()
simple_beta <- exp(coef(simple_fit)[1]) |> unname()

t <- timeline_model$time |> as.numeric()

timeline_model$y_fitted <-
    log(simple_beta) + simple_phi * log(t / 3600)
timeline_model$n_advancements_fitted <-
    80 - 80 / (simple_beta * (t / 3600)^simple_phi + 1)

subtitle <- rlang::expr(
    beta == !!round(simple_beta, 4) * ",  " * phi == !!round(simple_phi, 4)
)

transformed_plot <- ggplot(timeline_model, aes(x = x, y = y)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(y = y_fitted), color = scale_most[2], linewidth = 2) +
    xlab(quote(log(t / 3600))) +
    ylab(quote(log(80 / (80 - y) - 1))) +
    ggtitle("Global Fit", subtitle) +
    theme_most()

fit_plot <- ggplot(timeline_model, aes(x = time, y = n_advancements)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(y = n_advancements_fitted), color = scale_most[2], linewidth = 2) +
    scale_x_time(
        name = "t", labels = format_hms(s = FALSE),
        breaks = 12 * 60 * 1:5, minor_breaks = NULL
    ) + 
    ylab("y") +
    theme_most()

general_fit_plot <- cowplot::plot_grid(
    transformed_plot, 
    fit_plot, align = "h", 
    rel_widths = c(4, 3)
)

plot(general_fit_plot)
save_png(general_fit_plot, "plots/general_fit.png")


fit <- glm(y ~ x * standing + 0, weights = wt^2, data = timeline_model)
cf <- coef(fit)
nm <- names(cf)

fit_parameters <- tibble(
    standing = factor(1:6),
    beta = exp(cf[startsWith(nm, "standing")]),
    phi = c(cf["x"], cf["x"] + cf[startsWith(nm, "x:")])
)

curves <- expand.grid(
    time = seq(0, 3600, 10),
    standing = factor(1:6)
) |>
    filter(time <= 12 * 60 * (7 - as.integer(standing))) |>
    left_join(fit_parameters, by = "standing") |>
    mutate(n_advancements = 80 - 80 / (beta * (time/3600)^phi + 1))

gtl_plot <- timelines |>
    filter(time <= 12 * 60 * (7 - standing), is.na(ranked_event)) |>
    mutate(standing = factor(standing, levels = 6:1)) |>
    ggplot(aes(x = time, y = n_advancements, color = standing)) +
    geom_point(alpha = 0.2, size = 0.6) +
    geom_line(data = curves, linewidth = 1.05) +
    scale_x_time(name = "Time", labels = format_hms(s = FALSE),
                 breaks = 12 * 60 * (1:5), minor_breaks = NULL) + 
    scale_y_continuous(name = "Advancements", minor_breaks = NULL) +
    scale_color_manual(name = "Standing", values = rev(scale_most),
                       breaks = 1:6, labels = format_standings) +
    ggtitle("The Timeline", "Smoothed progression curve") +
    theme_most()

plot(gtl_plot)
save_png(gtl_plot, "plots/global_timeline.png")

