
files <- c(
    "api.R",
    "cleaning.R",
    "eliminations.R",
    "timelines.R",
    "timeline_model.R",
    "advancements.R",
    "comebacks.R",
    "tiebreaks.R"
)

for (f in files) {
    source(f)
    rm(list = ls())
}