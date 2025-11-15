#' Function to clean raw FRN survey data
#'
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @param location which dining halls are to be included
#' @param option total weight, grouped by food type, grouped by dining hall, and grouped by both food type and dining hall
#' @return table
#' @importFrom lubridate mdy
#' @importFrom dplyr filter summarize arrange group_by
#' @importFrom knitr kable
#' @importFrom utils head
#' @importFrom kableExtra row_spec
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("raw_food")
#' df <- clean_food(raw_food)
#' tidy_data <- df[['tidy_data']]
#'
#' # Call function
#' recovery_tables(tidy_data, '10-01-2024', '12-01-2024', "Tyler", "total")
#' recovery_tables(tidy_data, '10-01-2024', '12-01-2024', "Tyler", "type")
#' }
recovery_tables <- function(tidy_data, start, end, location, option = c("total", "type", "hall", "top")) {
  if (length(location) == 1 & option %in% c("hall", "top")) {
    option <- "total"
  }

  tidy_data <- tidy_data |>
    filter(.data$dining_hall %in% location) |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end))

  if (option == "total") {
    table <- tidy_data |>
      summarize(total_weight = round(sum(.data$weight, na.rm = TRUE), 1)) |>
      arrange(desc(.data$total_weight)) |>
      kable(col.names = c("Total Weight (lbs)"))
  } else if (option == "type") {
    table <- tidy_data |>
      group_by(.data$type) |>
      summarize(weight = round(sum(.data$weight, na.rm = TRUE), 1)) |>
      arrange(desc(.data$weight)) |>
      kable(col.names = c("Type", "Weight (lbs)"))
  } else if (option == "hall") {
    table <- tidy_data |>
      group_by(.data$dining_hall) |>
      summarize(weight = round(sum(.data$weight, na.rm = TRUE), 1)) |>
      arrange(desc(.data$weight)) |>
      kable(col.names = c("Dining Hall", "Weight (lbs)"))
  } else if (option == "top") {
    table <- tidy_data |>
      group_by(.data$dining_hall, .data$type) |>
      summarize(weight = round(sum(.data$weight, na.rm = TRUE), 1)) |>
      arrange(desc(.data$weight)) |>
      head(25) |>
      kable(col.names = c("Dining Hall", "Type", "Weight (lbs)"))
  }

  table <- table |> row_spec(0, background = "#48D1CC", color = "black")
  return(table)
}
