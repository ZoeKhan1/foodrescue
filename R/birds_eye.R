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
#' data("raw_food")
#' cooked_food <- clean_food(raw_food)
#'
#' # Call function
#' birds_eye(cooked_food[['tidy_data']], min(tidy_data$date), max(tidy_data$date), c("Tyler", "Comstock", "Cutter Ziskind", "Compass Cafe"))
#' }
birds_eye <- function(tidy_data, start, end, location) {
  tidy_data <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(dining_hall %in% location)

  plot <- ggplot(tidy_data, aes(date, weight,
                                text = paste0(dining_hall, "<br>", weight, " lbs"))) +
    geom_point() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "", y = "Weight (lbs)")

  product <- ggplotly(plot, tooltip = "text")
  return(product)
}
