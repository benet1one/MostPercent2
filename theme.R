
scale_most <- RColorBrewer::brewer.pal(11, "PuOr")[c(2, 3, 4, 8, 9, 11)]
windowsFonts(most = "Roboto Mono")

theme_most <- function(...) {
    theme_minimal(base_family = "most") +
        theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
}

format_hms <- function(x, h = TRUE, s = TRUE) {
    x <- hms::as_hms(x) |> format()
    if (!h)
        x <- stringr::str_remove(x, "^\\d{2}\\:")
    if (!s)
        x <- stringr::str_remove(x, "\\:\\d{2}$")
    return(x)
}
