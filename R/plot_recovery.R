#' Function to plot recovery
#'
#' @param tidy_data tidy data from clean_food
#' @param start start date
#' @param end end date
#' @param location specified dining hall(s)
#' @param fill option to fill with food type if multiple locations are selected
#' @return A ggplot object
#' @importFrom lubridate mdy
#' @importFrom dplyr filter mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes labs geom_col scale_fill_viridis_d theme_bw theme
#' @export
#'
#' @examples
#' \dontrun{
#' # Get data
#' data("raw_food")
#' cooked_food <- clean_food(raw_food)
#'
#' # Call function
#' plot_recovery(cooked_food, '10-1-2024', '11-01-2024', "Tyler")
#' plot_recovery(cooked_food, '10-1-2024', '11-01-2024', c("Tyler", "Comstock"), fill = TRUE)
#' }
plot_recovery <- function(tidy_data, start, end, location, fill = FALSE) {
  tidy_data <- tidy_data |>
    filter(date >= mdy(start)) |>
    filter(date <= mdy(end)) |>
    filter(dining_hall %in% location) |>
    filter(!is.na(weight)) |>
    mutate(dining_hall = fct_reorder(dining_hall, desc(weight), .fun = sum, .na_rm = TRUE)) |>
    mutate(type = fct_reorder(type, weight, .fun = sum, .desc = FALSE, .na_rm = TRUE))

  if (length(location) != 1) {
    if (fill == TRUE) {
      graph <- tidy_data |>
        ggplot(aes(dining_hall, y = weight, fill = type)) +
        geom_col() +
        scale_fill_viridis_d(option = "H", name = "Food type")

    } else if (fill == FALSE) {
      graph <- tidy_data |>
        ggplot(aes(dining_hall, y = weight)) +
        geom_col()
    }
    graph <- graph +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Recovery by dining hall",
           y = "Weight (lbs)", x = "")
    return(graph)
  } else if (length(location) == 1) {
    graph <- tidy_data |>
      ggplot(aes(x = fct_rev(type), y = weight)) +
      geom_col() +
      labs(x = "Food type", y = "Weight (lbs)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_bw() +
      theme(panel.grid = element_blank())
    return(graph)
  }
}
