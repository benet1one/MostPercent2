
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")

timeline_model <- timelines |> 
    filter(is.na(ranked_event), n_advancements > 0) |>
    mutate(
        y = log(80 / (80 - n_advancements) - 1),
        x = log(as.numeric(time) / 1800),
        w = as.numeric(time) %/% (12 * 60) + 2,
        player = factor(player)
    )

simple_fit <- glm(y ~ x, weights = w, data = timeline_model)
simple_phi <- coef(simple_fit)["x"] |> unname()
simple_beta <- exp(coef(simple_fit)[1]) |> unname()

t <- timeline_model$time |> as.numeric()

timeline_model$y_fitted <-
    log(simple_beta) + simple_phi * log(t / 1800)
timeline_model$n_advancements_fitted <-
    -80 / (simple_beta * (t / 1800)^simple_phi + 1) + 80

subtitle <- rlang::expr(
    beta == !!round(simple_beta, 4) * ",  " * phi == !!round(simple_phi, 4)
)

transformed_plot <- ggplot(timeline_model, aes(x = x, y = y)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(y = y_fitted), color = scale_most[2], linewidth = 2) +
    xlab(quote(log(t / 1800))) +
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

cowplot::plot_grid(transformed_plot, fit_plot, align = "h", rel_widths = c(4, 3)) |>
    plot()

fit <- glm(y ~ x + player + 0, weights = w, data = timeline_model)
cf <- coef(fit)
phi <- cf["x"] |> unname()
beta <- exp(cf[-1])
names(beta) <- stringr::str_remove(names(beta), "^player")
