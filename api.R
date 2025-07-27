
library(dplyr)
library(httr2)
library(stringr)

base_url <- "https://api.mcsrranked.com/"

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


get_match <- function(api_id) {
    request(base_url) |>
        req_url_path("matches", api_id) |>
        req_perform() |>
        resp_body_json() |>
        _$data
}

timeline <- function(match) {
    players <- bind_rows(match$players)
    bind_rows(match$timeline) |>
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
        
        rename(player = nickname) |>
        select(player, time, advancement, ranked_event) |>
        
        group_by(player) |>
        mutate(n_advancements = cumsum(!is.na(advancement))) |>
        ungroup()
}


matches <- read.csv("raw-data/MatchIDs_07-27.csv") |>
    tibble() |>
    rlang::set_names("match_id", "api_id") |>
    filter(!is.na(api_id)) |>
    rowwise() |>
    mutate(match_data = list(get_match(api_id)),
           datetime = as.POSIXct(match_data$date),
           timeline = list(timeline(match_data)),
           match_data = NULL)



