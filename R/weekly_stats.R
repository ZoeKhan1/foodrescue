#' Function to clean raw FRN survey data
#'
#' @param clean_data cleaned data from clean_food
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @param location specified dining hall(s)
#' @return A list with three values: weekly summed weight (lb), # containers, # pick-ups
#' @importFrom lubridate mdy
#' @importFrom dplyr filter summarize n
#' @importFrom knitr kable
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("raw_food")
#' df <- clean_food(raw_food)
#' tidy_data <- df[['tidy_data']]
#' clean_data <- df[['clean_data']]
#'
#' # Call function
#' weekly_stats(clean_data, tidy_data, '10-1-2024', '11-1-2024', "Tyler")
#' }
weekly_stats <- function(clean_data, tidy_data, start, end, location) {

  # Filtering to specified week and location
  tidy_filtered <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(.data$dining_hall %in% location)

  clean_filtered <- clean_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(.data$dining_hall %in% location)

  # Getting number of containers
  containers <- clean_filtered |>
    filter(.data$dining_hall != "Campus Center Cafe") |>
    filter(.data$dining_hall != "Compass Cafe") |>
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
  ) #|>
    #kable(col.names = c("Weight (lbs)", "Containers", "Pick-ups"))

  return(stats)
}
