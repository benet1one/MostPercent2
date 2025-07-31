
library(dplyr)
library(stringr)
source("api.R")

# Advancements ----------------------------------------------
fix_advancement_id <- function(advancement_id) {
    advancement_id |>
        str_remove("^advancements.") |>
        str_remove(".title$") |>
        str_replace("husbandry.netherite_hoe", "husbandry.obtain_netherite_hoe")
    
}

language <- jsonlite::read_json("raw-data/Language.json")

advancements <- tibble(advancement = unlist(language), advancement_id = names(language)) |>
    filter(advancement_id |> startsWith("advancements.")) |>
    filter(advancement_id |> endsWith(".title")) |>
    mutate(advancement_id = fix_advancement_id(advancement_id))


# Nicknames ----------------------------------------------------
best_username <- function(user) {
    if (length(user$connections$twitch) == 0L) {
        user$nickname
    } else {
        user$connections$twitch$name
    }
}

# Complex Data --------------------------------------------------
parse_timeline <- function(match, match_id) {
    pl <- bind_rows(match$players) |>
        left_join(players, by = "uuid")
        
    bind_rows(match$timeline) |>
        mutate(match_id = match_id, .before = 1L) |>
        mutate(time = hms::hms(time/1000)) |>
        arrange(time) |>
        
        filter(!str_detect(type, ".root")) |>
        left_join(players, by = "uuid") |>
        left_join(advancements, by = join_by(type == advancement_id)) |>
        mutate(ranked_event = if_else(
            condition = type |> startsWith("projectelo."),
            false = NA,
            true = str_remove(type, "^projectelo.timeline.")
        )) |>
        
        select(match_id, player, time, advancement, ranked_event) |>
        group_by(player) |>
        mutate(n_advancements = cumsum(!is.na(advancement)  &  !duplicated(advancement))) |>
        ungroup()
}


get_tiebreaks <- function(timeline) {
    eliminations <- timeline |>
        filter(ranked_event == "eliminate") |>
        select(match_id, player, time, n_advancements)
    
    if (nrow(eliminations) != 5L)
        return(NULL)
    
    tb <- eliminations |>
        mutate(scheduled_time = hms::hms(60 * 12 * 1:5)) |>
        filter(round(time, 1) > scheduled_time) |>
        rowwise()
    
    if (nrow(tb) > 0L)
        group_map(tb, summarise_tiebreak, timeline = timeline)
    else
        return(NULL)
}

summarise_tiebreak <- function(x, timeline, ...) {
    timeline |>
        filter(time >= x$scheduled_time, time <= x$time) |>
        filter(ranked_event == "eliminate" | n_advancements == x$n_advancements + 1) |>
        mutate(scheduled_time = x$scheduled_time, .before = time) |>
        mutate(match_id = match_id, .before = 1L) |>
        arrange(time, !is.na(ranked_event))
}

get_standings <- function(timeline) {
    standings <- timeline |>
        group_by(player) |>
        summarise(match_id = match_id[1L], time = last(time), n_advancements = last(n_advancements)) |>
        arrange(n_advancements) |>
        mutate(standing = 6:1, points = 0:5, .after = player) |>
        relocate(match_id, .before = 1L)
    
    standings$time[6L] <- standings$time[5L]
    standings
}

include_standing <- function(df, standings) {
    st <- standings |> select(player, standing)
    left_join(df, st, by = "player") |> relocate(standing, .after = player)
}

# Main Datasets ----------------------------------------------------
matches <- read.csv("raw-data/MatchIDs_07-31.csv") |>
    tibble() |>
    rlang::set_names("match_id", "api_id") |>
    filter(!is.na(api_id)) |>
    rowwise() |>
    mutate(stage = as.integer(substring(match_id, 1, 1)),
           group = substring(match_id, 2, 2),
           .after = match_id) |>
    mutate(match_data = list(get_match(api_id)))

players <- matches$match_data |>
    purrr::map(\(md) md$players) |>
    bind_rows() |>
    distinct(uuid, .keep_all = TRUE) |>
    rowwise() |>
    mutate(player = best_username(get_user(uuid)), .before = nickname) |>
    rename(ign = nickname) |>
    within({
        player[ign == "NOHACKSJUSTTIGER"] <- "TigerMCSR"
        player[ign == "L9_FOXGIRLPAWJOB"] <- "chrisXD"
    })

matches <- matches |>
    mutate(datetime = as.POSIXct(match_data$date, tz = "UTC"),
           timeline = list(parse_timeline(match_data, match_id)),
           standings = list(get_standings(timeline)),
           timeline = list(include_standing(timeline, standings)),
           tiebreaks = list(get_tiebreaks(timeline)),
           winner = last(standings$player))

timelines <- bind_rows(matches$timeline)
standings <- bind_rows(matches$standings) |>
    mutate(matchup = substring(match_id, 1, 2), .before = 1L)
points <- standings |>
    group_by(matchup, player) |>
    summarise(points = sum(points), total_advancements = sum(n_advancements), .groups = "drop")
tiebreaks <- bind_rows(matches$tiebreaks) |>
    group_by(match_id, scheduled_time) |>
    mutate(duration = hms::as_hms(time - scheduled_time), .after = time)

# Saving Data ---------------------------------------------------
saveRDS(matches, file = "data/matches.RDS")
saveRDS(timelines, file = "data/timelines.RDS")
saveRDS(standings, file = "data/standings.RDS")
saveRDS(points, file = "data/points.RDS")
saveRDS(tiebreaks, file = "data/tiebreaks.RDS")

matches_csv <- matches |> 
    select(match_id, api_id, datetime, winner) 

timelines_csv <- timelines |>
    mutate(time_ms = as.integer(time * 1000))

tiebreaks_csv <- tiebreaks |>
    mutate(time_ms = as.integer(time * 1000), 
           duration_ms = as.integer(duration * 1000), 
           .after = duration)

write.csv(matches_csv, file = "data/matches.csv", row.names = FALSE)
write.csv(timelines_csv, file = "data/timelines.csv", row.names = FALSE)
write.csv(standings, file = "data/standings.csv", row.names = FALSE)
write.csv(points, file = "data/points.csv", row.names = FALSE)
write.csv(tiebreaks_csv, file = "data/tiebreaks.csv", row.names = FALSE)