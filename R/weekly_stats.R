#' Function to clean raw FRN survey data
#'
#' @param clean_data cleaned data from clean_food
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @return A list with three values: weekly summed weight (lb), # containers, # pick-ups
#' @importFrom lubridate mdy
#' @importFrom dplyr filter summarize
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("raw_food")
#' cooked_food <- clean_food(raw_food)
#'
#' # Call function
#' weekly_stats(cooked_food[["clean_data"]], cooked_food[["tidy_data"]], '10-1-2024', '10-7-2024')
#' }
weekly_stats <- function(clean_data, tidy_data, start, end) {
  # Formatting data
  tidy_data$date <- mdy(tidy_data$date)
  clean_data$date <- mdy(clean_data$date)
  clean_data$containers <- as.numeric(clean_data$containers)

  # Filtering to specified week
  tidy_filtered <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end))

  clean_filtered <- clean_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end))

  # Getting number of containers
  containers <- clean_filtered |>
    filter(dining_hall != "Campus Center Cafe") |>
    filter(dining_hall != "Compass Cafe") |>
    summarize(num_containers = sum(containers, na.rm = TRUE))

  # Getting number of pick-ups
  pickups <- clean_filtered |>
    filter(containers > 0) |>
    summarize(count = n())

  # Getting weekly weight
  if (mdy(start) > mdy("01-01-2025")) {
    dining_halls <- tidy_filtered |>
      filter(!dining_hall %in% c("Campus Center Cafe", "Compass Cafe")) |>
      summarize(total_weight = sum(weight, na.rm = TRUE) - containers*2)
  } else {
    dining_halls <- tidy_filtered |>
      filter(!dining_hall %in% c("Campus Center Cafe", "Compass Cafe")) |>
      summarize(total_weight = sum(weight, na.rm = TRUE))
  }
  cafes <- tidy_filtered |>
    filter(dining_hall == "Campus Center Cafe" | dining_hall == "Compass Cafe") |>
    summarize(total_weight = sum(weight, na.rm = TRUE))
  weight <- dining_halls + cafes

  return(list(weight = weight, containers = containers, pickups = pickups))
}
