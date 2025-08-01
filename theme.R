
plot_width <- 8.0
scale_most <- RColorBrewer::brewer.pal(11, "PuOr")[c(2, 3, 4, 8, 9, 11)]
windowsFonts(
    regular = "Montserrat Medium", 
    bold = "Montserrat SemiBold", 
    black = "Montserrat Black"
)

theme_most <- function(...) {
    theme_minimal(base_family = "regular") +
        theme(
            plot.title = element_text(family = "black"),
            axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r = 16)),
            ...
        )
}

format_hms <- function(x, h = TRUE, s = TRUE) {
    x <- hms::as_hms(x) |> format()
    if (!h)
        x <- stringr::str_remove(x, "^\\d{2}\\:")
    if (!s)
        x <- stringr::str_remove(x, "\\:\\d{2}$")
    return(x)
}

format_standings <- function(x) {
    c("1st", "2nd", "3rd", "4th", "5th", "6th")[as.integer(x)]
}