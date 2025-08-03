
library(dplyr)
library(ggplot2)
source("theme.R")

timelines <- readRDS("data/timelines.RDS")
standings <- readRDS("data/standings.RDS")

advancements <- timelines |>
    group_by(match_id, player) |>
    filter(!duplicated(advancement)) |>
    mutate(is_advancement = !is.na(advancement)) |>
    within({advancement[!is_advancement] <- ranked_event[!is_advancement]}) |>
    group_by(advancement) |>
    reframe(
        n = n(),
        prop = (n / 6) / length(unique(standings$match_id)),
        mean = mean(time),
        median = median(time) |> hms::as_hms(),
        sd = sd(as.numeric(time)),
        soonest = min(time),
        who_soonest = player[which.min(time)],
        match_soonest = match_id[which.min(time)],
        z_soonest = unclass(abs(mean - soonest) / sd),
        latest = max(time),
        who_latest = player[which.max(time)],
        z_latest = unclass(abs(mean - latest) / sd),
        match_latest = match_id[which.max(time)],
        is_advancement = is_advancement[1]
    ) |>
    mutate(across(c(mean, sd, soonest, latest), hms::as_hms)) |>
    arrange(median)

latest <- advancements |>
    select(!soonest:match_soonest) |>
    arrange(-z_latest) |>
    head(10) |>
    arrange(latest) |>
    left_join(standings, by = join_by(who_latest == player, match_latest == match_id))

latest_plot <- timelines |>
    filter(advancement %in% latest$advancement) |>
    mutate(advancement = factor(advancement, levels = latest$advancement)) |>
    ggplot(aes(x = time, y = advancement, color = factor(standing))) +
    geom_point(aes(alpha = time), size = 1.8, position = position_jitter(height = 0.15, seed = 1)) +
    geom_label(data = latest, aes(x = latest, label = who_latest, colour = factor(standing)),
              hjust = 0, nudge_x = 50, label.size = 0, family = "bold", show.legend = FALSE) +
    scale_x_time(name = "Time", labels = format_hms(h = FALSE), 
                 breaks = 12 * 60 * 0:5, minor_breaks = NULL, limits = c(0, max(latest$latest) + 400)) +
    ylab("") +
    scale_color_manual(name = "Standing", values = rev(scale_most), labels = format_standings) +
    scale_alpha_continuous(range = c(0.1, 1.0), guide = "none") +
    ggtitle("Oops, I forgot A Seedy Place!", "Queenkac plants a seed right before the 48m elimination") +
    theme_most()

plot(latest_plot)




adv <- advancements |>
    filter(n >= 12, is_advancement) |>
    _$advancement |>
    sort()

advancements_by_match <- timelines |>
    group_by(match_id, player) |>
    filter(!is.na(advancement), !duplicated(advancement)) |>
    mutate(advancement = factor(advancement, levels = adv)) |>
    select(!ranked_event) |>
    group_by(match_id, advancement, .drop = FALSE) |>
    summarise(
        n = n(),
        index = mean(n_advancements),
        sd = sd(n_advancements),
        .groups = "drop"
    ) |>
    group_by(match_id)

advancement_dissimilarity <- function(a1, a2) {
    deltas <- advancements_by_match |>
        filter(advancement == a1 | advancement == a2) |>
        summarise(
            delta_n = abs(n[1] - n[2]),
            delta_index = abs(index[1] - index[2]) / sqrt(sd[1] * sd[2]),
            delta = 2*delta_n + delta_index
        ) |>
        filter(!is.na(delta), is.finite(delta))
    
    if (nrow(deltas) == 0L)
        browser()
    
    mean(deltas$delta)
}

advancement_grid <- combn(adv, 2) |>
    t() |>
    as_tibble(.name_repair = "minimal") |>
    rename(a1 = 1, a2 = 2) |>
    mutate(a1 = factor(a1, levels = adv), a2 = factor(a2, levels = adv)) |>
    rowwise() |>
    mutate(dissim = advancement_dissimilarity(a1, a2))

dist <- advancement_grid |>
    reshape2::acast(a2 ~ a1, drop = FALSE, value.var = "dissim") |>
    as.dist()

clustering <- hclust(dist, method = "ward.D2")

advancements <- advancements |>
    filter(is_advancement) |>
    mutate(cluster = cutree(clustering, k = 4)[advancement], .after = median) |>
    within(cluster[is.na(cluster)] <- 5)

cluster_order <- advancements |>
    group_by(cluster) |>
    summarise(median = median(median)) |>
    arrange(median) |>
    _$cluster

advancements$cluster <- factor(advancements$cluster, levels = cluster_order)

cluster_colors <- tribble(
    ~background, ~foreground,
    "#1C5373", "white",
    "#904E55", "white",
    "#FFC857", "black",
    "#469B4B", "white",
    "#7AC74F", "white"
)


advancements |>
    ggplot(aes(x = median, y = prop, color = cluster)) +
    geom_point() +
    scale_color_manual(values = cluster_colors$background) +
    theme_most()

advancements |> 
    mutate(proportion = format_percentage(prop, 1), median = .format_hms(round(median), h = FALSE)) |>
    rename_with(stringr::str_to_title) |>
    select(Advancement, N, Proportion, Median) |>
    flextable::flextable() |>
    flextable::font(fontname = "Montserrat Black", part = "header") |>
    flextable::font(fontname = "Montserrat SemiBold", j = "Advancement") |>
    flextable::font(fontname = "Roboto", j = -1) |>
    flextable::color(color = cluster_colors$foreground[advancements$cluster]) |>
    flextable::bg(bg = cluster_colors$background[advancements$cluster]) |>
    flextable::align(j = -1, align = "right") |>
    flextable::width(width = c(2, 1, 1, 1))

