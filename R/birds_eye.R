#' Function to make interactive plot
#'
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @param location specified dining hall(s)
#' @return A ggplot object
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot theme_bw geom_point theme labs aes
#' @importFrom plotly ggplotly
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data
#' df <- clean_food(raw_food)
#' tidy_data <- df[['tidy_data']]
#'
#' # Call function
#' birds_eye(tidy_data, '9-1-2024', '9-1-2025', "Tyler")
#' }
birds_eye <- function(tidy_data, start, end, location) {
  tidy_data <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(dining_hall %in% location) |>
    filter(weight > 0)

  plot <- ggplot(tidy_data, aes(date, weight,
                                text = paste0(dining_hall, "<br>", weight, " lbs"))) +
    geom_point() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "", y = "Weight (lbs)")

  product <- ggplotly(plot, tooltip = "text")
  return(product)
}
