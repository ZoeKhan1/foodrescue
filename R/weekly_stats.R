#' Function to clean raw FRN survey data
#'
#' @param clean_data cleaned data from clean_food
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @param location specified dining hall(s)
#' @return A list with three values: weekly summed weight (lb), # containers, # pick-ups
#' @importFrom lubridate mdy
#' @importFrom dplyr filter summarize
#' @importFrom knitr kable
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
weekly_stats <- function(clean_data, tidy_data, start, end, location) {

  # Filtering to specified week and location
  tidy_filtered <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(dining_hall %in% location)

  clean_filtered <- clean_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(dining_hall %in% location)

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
  weight <- tidy_filtered |>
    summarize(total_weight = sum(weight, na.rm = TRUE))

  # Consolidation
  stats <- data.frame(
    Weight = weight,
    Containers = containers,
    Pickups = pickups
  ) |>
    kable(col.names = c("Weight", "Containers", "Pick-ups"))

  return(stats)
}
