
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
save_png(latest_plot, "plots/latest_advancement.png")




common_advancements <- advancements |>
    filter(prop > 0.6) |>
    _$advancement

playstyles <- timelines |> 
    mutate(player = factor(player), standing = factor(standing)) |>
    filter(advancement %in% common_advancements) |>
    filter(!is.na(advancement)) |>
    group_by(match_id, advancement) |>
    mutate(std_index = n_advancements - median(n_advancements)) |>
    group_by(advancement, player) |>
    filter(n() > 3) |>
    filter(std_index < 2 * diff(quantile(std_index, c(.25, .75)))) |>
    group_by(advancement) |>
    mutate(player = droplevels(player))

player_playstyles <- playstyles |>
    group_modify(function(x, ...) {
        fit <- summary(lm(std_index ~ player + 0, data = x))
        t <- fit$coefficients[, "t value"]
        p <- fit$coefficients[, "Pr(>|t|)"] |> p.adjust(method = "bonferroni")
        r <- fit$r.squared
        
        tibble(
            fit = list(fit),
            t_values = list(t),
            r_squared = r,
            earliest = names(which.min(t)) |> substring(7),
            latest = names(which.max(t)) |> substring(7),
            p_earliest = p[which.min(t)],
            p_latest = p[which.max(t)]
        )
    }) |>
    arrange(-r_squared)

standing_playstyles <- playstyles |>
    group_modify(function(x, ...) {
        fit <- summary(lm(std_index ~ standing + 0, data = x))
        t <- fit$coefficients[, "t value"]
        p <- fit$coefficients[, "Pr(>|t|)"] |> p.adjust(method = "bonferroni")
        r <- fit$r.squared
        
        tibble(
            fit = list(fit),
            t_values = list(t),
            r_squared = r,
        )
    }) |>
    arrange(-r_squared)


player_playstyle_plot <- function(ps, ...) {
    highlighted <- c(ps$earliest, ps$latest)
    
    df <- timelines |>
        filter(advancement == ps$advancement) |>
        group_by(match_id, advancement) |>
        mutate(std_index = n_advancements - median(n_advancements)) |>
        ungroup() |>
        filter(std_index < 5 * diff(quantile(std_index, c(.25, .75)))) |>
        mutate(player = factor(player, levels = highlighted)) |>
        mutate(y = rnorm(n(), sd = 0.15))
    
    df_highlighted <- df |> filter(!is.na(player))
    df_gray <- df |> filter(is.na(player))
    df_names <- df |> group_by(player) |> summarise(x = mean(std_index))
    
    ggplot(mapping = aes(x = std_index, y = y, color = player)) +
        geom_point(data = df_gray, size = 2, color = "gray", alpha = 0.5) +
        geom_point(data = df_highlighted, size = 3) +
        geom_text(data = df_names, aes(x = x, y = 0.7, label = player), family = "bold") +
        scale_x_continuous(name = "", breaks = NULL) +
        scale_y_continuous(name = "", breaks = NULL, limits = c(-1, +1)) +
        scale_color_manual(values = c(scale_most[2], scale_most[5])) +
        ggtitle(ps$advancement) +
        theme_most() +
        theme(
            plot.title = element_text(family = "regular", hjust = 0.5), 
            legend.position = "none",
            plot.margin = margin(t = 10, l = 20, r = 20)
        )
}

set.seed(121393)

playstyle_plot_grid <- player_playstyles |>
    head(4) |>
    rowwise() |>
    group_map(player_playstyle_plot) |>
    cowplot::plot_grid(plotlist = _)

plot(playstyle_plot_grid)
save_png(playstyle_plot_grid, "plots/playstyle.png")

# You Need a Mint
# Enchanter
# Best Friends Forever
# This Boat Has Legs

# Hired Help
# Bullseye
# Arbalistic
# Sniper Duel

multimodal_adv <- c(
    # "You Need a Mint",
    # "Enchanter",
    # "Best Friends Forever",
    "Stone Age",
    "Diamonds!",
    "Acquire Hardware",
    "Oh Shiny"
)

multimodal_plot <- timelines |> 
    filter(advancement %in% multimodal_adv) |>
    mutate(standing = factor(standing)) |>
    ggplot(aes(x = time, fill = standing)) +
    facet_wrap(~advancement, scales = "free_y") +
    geom_histogram(breaks = seq(0, 3600, 120)) +
    scale_x_time(name = "Time", labels = format_hms(s = FALSE), breaks = 12 * 60 * 0:5) +
    scale_y_continuous(name = NULL, labels = NULL) +
    scale_fill_manual(name = "Standing", values = rev(scale_most), labels = format_standings) +
    ggtitle("Advancements that Define Playstyle", "Non-RNG-Heavy Multi-Modal Advancements") +
    theme_most(panel.grid.minor = element_blank())

plot(multimodal_plot)
save_png(multimodal_plot, "plots/multimodal_plot.png")

fake_advancements <- c("Minecraft", "Adventure", "Husbandry", "Nether", "The End")
adv <- advancements |>
    filter(n >= 12, is_advancement) |>
    _$advancement |>
    setdiff(fake_advancements) |>
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
    filter(is_advancement, !is.element(advancement, fake_advancements)) |>
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

advancement_list <- advancements |> 
    mutate(proportion = format_percentage(prop, 1), median = .format_hms(round(median), h = FALSE)) |>
    rename_with(stringr::str_to_title) |>
    select(Advancement, N, Proportion, Median) |>
    flextable::flextable() |>
    flextable::font(fontname = "Montserrat Black", part = "header") |>
    flextable::font(fontname = "Montserrat SemiBold", j = "Advancement") |>
    flextable::font(fontname = "Roboto", j = -1) |>
    flextable::color(color = cluster_colors$foreground[advancements$cluster]) |>
    flextable::bg(bg = cluster_colors$background[advancements$cluster]) |>
    flextable::align(j = -1, align = "right", part = "all") |>
    flextable::width(width = c(2, 1, 1, 1))

print(advancement_list)
flextable::save_as_image(advancement_list, "plots/advancement_list.png")
