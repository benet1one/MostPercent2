
library(httr2)
base_url <- "https://api.mcsrranked.com/"

get_match <- function(api_id) {
    request(base_url) |>
        req_url_path("matches", api_id) |>
        req_perform() |>
        resp_body_json() |>
        _$data
}

