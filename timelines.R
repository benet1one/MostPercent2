
library(dplyr)
library(ggplot2)
source("theme.R")

matches <- readRDS("data/matches.RDS")
timelines <- readRDS("data/timelines.RDS")
standings <- readRDS("data/standings.RDS")

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
        group_by(player, standing) |>
        group_modify(function(x, g) {
            if (g$standing != 1  ||  last(x$time) > 3600) 
                return(x)
            add_row(x, time = hms::hms(3600), n_advancements = last(x$n_advancements))
        }) |>
        ggplot(aes(x = time, y = n_advancements, color = player)) +
        geom_step(show.legend = FALSE) +
        ggrepel::geom_text_repel(
            data = st, aes(label = player), show.legend = FALSE,
            hjust = 0, nudge_x = 20, nudge_y = -0.1, family = "bold", 
            direction = "y", force = 1e-4, seed = 1
        ) +
        scale_x_time(name = "Time", labels = format_hms(s = FALSE),
                     breaks = 12 * 60 * 1:5, minor_breaks = NULL,
                     limit = c(0, max(timelines$time) + 400)) + 
        scale_y_continuous(name = "Advancements", minor_breaks = NULL) +
        scale_color_manual(values = scale_most) +
        ggtitle(title) +
        theme_most()
}

timeline_plot("1F1")

# all_timelines <- purrr::map(matches$match_id, timeline_plot) |>
#     cowplot::plot_grid(plotlist = _, ncol = 3)


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
    filter(n >= 4) |>
    ggplot(aes(x = tm, y = factor(standing), fill = advancement, color = advancement)) +
    geom_segment(aes(x = tl, xend = tu), linewidth = 6, alpha = 0.5) +
    geom_text(aes(label = n_advancements), nudge_y = 0.3, family = "bold", show.legend = FALSE) +
    scale_x_time(name = "Time (m)", breaks = 60 * 2 * (0:10), 
                 labels = format_hms(h = FALSE, s = FALSE), minor_breaks = NULL) + 
    scale_y_discrete(name = "Standing", limits = rev) +
    scale_color_manual(name = "Split", values = splits$color[-1]) +
    ggtitle("Early Advancements", "Advancement count before the any% splits") +
    theme_most()

plot(split_plot)
save_png(split_plot, "plots/splits.png")


# split_plot_2 <- timelines |>
#     mutate(advancement = factor(advancement, levels = splits$advancement)) |>
#     filter(!is.na(advancement)) |>
#     group_by(standing, advancement) |>
#     summarise(n_advancements = round(median(n_advancements)) - 1,
#               tl = quantile(time, 0.35),
#               tu = quantile(time, 0.65), 
#               tm = (tl + tu) / 2,
#               n = n(), .groups = "drop_last") |>
#     filter(n >= 3) |>
#     reframe(ts = c(0, tl), tm = c(tm, 0), tu = c(tu, hms::hms(22 * 60)), 
#             split = c("Overworld", as.character(advancement)) |> factor(levels = splits$advancement),
#             n_advancements = c(n_advancements, NA)) |>
#     ggplot(aes(x = tm, y = factor(standing), color = split)) +
#     geom_segment(aes(x = ts, xend = tu), linewidth = 6, alpha = 0.3) +
#     geom_text(aes(label = n_advancements), nudge_y = 0.3, family = "bold", show.legend = FALSE) +
#     scale_x_time(name = "Time (m)", breaks = 60 * 2 * (0:20), 
#                  labels = format_hms(h = FALSE, s = FALSE), minor_breaks = NULL) + 
#     scale_y_discrete(name = "Standing", limits = rev, labels = format_standings) +
#     scale_color_manual(name = "Split", values = splits$color) +
#     ggtitle("Early Advancements", "Advancement count during the any% splits") +
#     theme_most()


nether_entry <- timelines |>
    group_by(match_id, player) |>
    filter(advancement == "We Need to Go Deeper", !duplicated(advancement)) |>
    group_by(match_id) |>
    arrange(time, .by_group = TRUE) |>
    mutate(enter_nether_order = 1:n())

with(nether_entry, table(
    first_eliminated = (standing == 6L), 
    entered_first_three = (enter_nether_order <= 3L)
))


split_scatter_df <- timelines |>
    mutate(advancement = factor(advancement, levels = splits$advancement[c(2, 3, 4, 6)])) |>
    filter(!is.na(advancement))

split_scatter_fits <- split_scatter_df |> 
    group_by(advancement) |>
    reframe(beta = coef(lm(n_advancements ~ time))) |>
    group_by(advancement) |>
    reframe(intercept = beta[1], slope = beta[2])

line_color <- scale_most[1] |>
    scales::col_saturate(-20)

split_scatter <- split_scatter_df |>
    ggplot(aes(x = time, y = n_advancements, color = factor(standing))) +
    facet_wrap(~advancement, scales = "free", ncol = 2) +
    geom_point(size = 4, alpha = 0.45) +
    geom_abline(data = split_scatter_fits, aes(intercept = intercept, slope = slope), 
                linewidth = 1, alpha = 0.25, color = line_color) + 
    scale_x_time(name = "Time (m)", labels = format_hms(h = FALSE, s = FALSE)) +
    scale_y_continuous(name = "", breaks = 0:40 * 2, minor_breaks = NULL) +
    scale_color_manual(name = "Standing", values = rev(scale_most), labels = format_standings) +
    ggtitle("Faster Runners Win (duh)", "Winners reach each split faster, with similar advancement count") +
    theme_most()
    
plot(split_scatter)
save_png(split_scatter, "plots/split_scatter.png")
