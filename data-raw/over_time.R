#
# library(foodrescue)
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# data(raw_food)
# df <- clean_food(raw_food)
# tidy_data <- df[['tidy_data']]
#
# over_time <-function(data, location, food_type) {
#   no <- tidy_data |>
#     filter(dining_hall %in% location) |>
#     filter(type == food_type) |>
#     mutate(semester = case_when(
#       date < mdy('1-1-2025') ~ "Fall 2024",
#       date < mdy('9-1-2025') ~ "Spring 2025",
#       date < mdy('1-1-2026') ~ "Fall 2025"
#     )) |>
#     group_by(semester) |>
#     summarize(total_weight = sum(weight))
#
#
#   g <- ggplot(no, aes(x = semester, y = total_weight)) +
#     geom_col() +
#     ylim(0, max(no$total_weight)) +
#     labs(y = "Weight (lbs)", x = "")
#
#   return(g)
# }
#
# over_time(tidy_data, "Tyler", "chicken")
