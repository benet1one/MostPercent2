
library(httr2)
base_url <- "https://api.mcsrranked.com/"

get_match <- function(id) {
    request(base_url) |>
        req_url_path("matches", id) |>
        req_perform() |>
        resp_body_json() |>
        _$data
}

get_user <- function(uuid) {
    request(base_url) |>
        req_url_path("users", uuid) |>
        req_perform() |>
        resp_body_json() |>
        _$data
}
