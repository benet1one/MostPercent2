
library(dplyr)
library(ggplot2)
source("theme.R")

matches <- readRDS("data/matches.RDS")
timelines <- readRDS("data/timelines.RDS")
standings <- readRDS("data/standings.RDS")

gtl_plot <- timelines |>
    filter(time <= 12 * 60 * (7 - standing), is.na(ranked_event)) |>
    mutate(standing = factor(standing, levels = 6:1)) |>
    ggplot(aes(x = time, y = n_advancements, color = standing)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_smooth(alpha = 0) +
    scale_x_time(name = "Time", breaks = 12 * 60 * (1:5), labels = ~format_hms(.x, s = FALSE)) + 
    scale_y_continuous(name = "Advancements", minor_breaks = NULL) +
    scale_color_manual(name = "Standing", values = rev(scale_most), breaks = 1:6, labels = format_standings) +
    theme_most()

plot(gtl_plot)
ggsave("plots/mean_timeline.png", width = 8, height = 6)

timeline_plot <- function(match_id) {
    title <- paste(
        "Stage", substring(match_id, 1, 1), 
        " Group", substring(match_id, 2, 2),
        " Seed", substring(match_id, 3, 3)
    )
    st <- standings |>
        filter(match_id == !!match_id)
    timelines |>
        filter(match_id == !!match_id) |>
        mutate(player = factor(player, levels = st$player)) |>
        ggplot(aes(x = time, y = n_advancements, color = player)) +
        geom_step(show.legend = FALSE) +
        geom_text(data = st, aes(label = player), show.legend = FALSE,
                  hjust = 0, nudge_x = 20, family = "bold") +
        scale_x_time(name = "Time", breaks = 12 * 60 * (1:5), limit = c(0, 3950),
                     labels = ~format_hms(.x, s = FALSE)) + 
        scale_y_continuous(name = "Advancements", minor_breaks = NULL) +
        scale_color_manual(values = scale_most) +
        ggtitle(title) +
        theme_most()
}

all_timelines <- purrr::map(matches$match_id, timeline_plot) |>
    cowplot::plot_grid(plotlist = _, ncol = 3)


splits <- tribble(
    ~advancement, ~split, ~color,
    "Overworld",            "Overworld",  "#415a77",
    "We Need to Go Deeper", "Nether",     "#703131",
    "Those Were the Days",  "Bastion",    "#27232b",
    "A Terrible Fortress",  "Fortress",   "#4a252b",
    "Eye Spy",              "Stronghold", "#485734",
    "The End?",             "End",        "#9c966d",
    "Remote Getaway",       "Outer End",  "gray20",
    # "The City at the End of the Game",  "End City", "#8c648b"
) |> mutate(split_ind = 0:6, split = factor(split, levels = split))

splitted <- timelines |>
    left_join(select(splits, advancement, split_ind), by = "advancement") |>
    within(split_ind[is.na(split_ind)] <- 0L) |>
    group_by(match_id, player) |>
    mutate(split_ind = cummax(split_ind)) |>
    left_join(select(splits, split_ind, split), by = "split_ind") |>
    select(!split_ind)
    # filter(match_id == "1A1") |>
    # print(n = 500)

split_plot <- timelines |>
    mutate(advancement = factor(advancement, levels = splits$advancement)) |>
    filter(!is.na(advancement)) |>
    group_by(standing, advancement) |>
    summarise(n_advancements = round(median(n_advancements)) - 1,
              tl = quantile(time, 0.35),
              tu = quantile(time, 0.65), 
              tm = (tl + tu) / 2,
              n = n(), .groups = "drop_last") |>
    filter(n >= 3) |>
    ggplot(aes(x = tm, y = factor(standing), fill = advancement, color = advancement)) +
    geom_segment(aes(x = tl, xend = tu), linewidth = 6, alpha = 0.5) +
    geom_text(aes(label = n_advancements), nudge_y = 0.3, family = "bold", show.legend = FALSE) +
    scale_x_time(name = "Time (m)", breaks = 60 * 2 * (0:10), 
                 labels = ~format_hms(.x, h = FALSE, s = FALSE), minor_breaks = NULL) + 
    scale_y_discrete(name = "Standing", limits = rev) +
    scale_color_manual(name = "Split", values = splits$color[-1]) +
    ggtitle("Early Advancements", "Advancement count before the any% splits") +
    theme_most()

plot(split_plot)
ggsave("plots/splits.png", width = 8.0, height = 4.2)
