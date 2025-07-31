
library(dplyr)

timelines <- readRDS("data/timelines.RDS") |>
    mutate(match_id = factor(match_id), player = factor(player)) |>
    group_by(match_id, player) |>
    filter(is.na(ranked_event)) |>
    filter(row_number() %% 10L == 0L)

model <- thomas::cmdstan_model("timeline_model.stan")
fit <- thomas::run_cmdstan(
    model = model,
    data = list(
        N = nrow(timelines),
        n_seeds = length(levels(timelines$match_id)),
        n_players = length(levels(timelines$player)),
        seed = as.integer(timelines$match_id),
        player = as.integer(timelines$player),
        t = as.numeric(timelines$time),
        a = timelines$n_advancements
    ),
    chains = 3,
    iter = 4000
)

saveRDS(fit, "model_fit.RDS")
thomas::traceplot(fit) |> plot()

draws <- thomas::get_draws(fit)
colnames(draws$beta_player) <- levels(timelines$player)

draws$beta_player[, sample.int(ncol(draws$beta_player), 3L)] |>
    reshape2::melt() |>
    ggplot(aes(x = value, fill = Var2, color = Var2)) +
    geom_density(linewidth = 1, alpha = 0.4, bw = 0.15) +
    theme_minimal()
