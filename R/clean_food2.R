#' Function to clean raw FRN survey data
#'
#' @param data corrected raw data from FRN survey
#' @return A list with two data frames:
#' \describe{
#'  \item{clean_data}{}
#'  \item{tidy_data}{}
#' }
#' @importFrom tidyr separate_rows pivot_longer
#' @importFrom dplyr filter select rename mutate rename_with case_when if_else
#' @importFrom stringr str_replace str_replace_all
#' @importFrom purrr map flatten_chr
#' @importFrom lubridate mdy
#' @importFrom rlang sym
#' @importFrom janitor clean_names
#' @export
#'
#' @examples
#' \dontrun{
#' cooked_food <- clean_food2(data)
#' }

clean_food2 <- function(data) {
  # Initial cleaning of variable names with janitor
  data <- clean_names(data)

  # Preliminary cleaning
  clean_data <- data |>
    filter(status == "IP Address") |>
    dplyr::select(-c(start_date, end_date, progress, recipient_first_name, recipient_last_name, recipient_email, external_reference, location_latitude, location_longitude, distribution_channel, user_language, finished, duration_in_seconds, ip_address, response_id, recorded_date)) |>
    separate_rows(q2, sep = ",") |>
    rename(dining_hall = q2) |>
    rename(date = q1)

  # Renaming columns for interpretation
  prefixes <- list(c("x1_", "CCC_"), c("x2_", "CD_"), c("x3_", "CC_"), c("x4_", "C_"), c("x6_", "H_"), c("x7_", "KS_"), c("x8_", "L_"), c("x9_", "NG_"), c("x10_", "T_"), c("x11_", "O_"), c("x12_", "Ca_"), c("x13_", "CZ_"))
  suffixes <- list(c("q3", "containers"), c("q4_16", "beef"), c("q4_22", "beans"), c("q4_17", "bread"), c("q4_19", "chicken"), c("q4_18", "dessert"), c("q4_20", "fish"), c("q4_21", "fruit"), c("q4_24", "meat_substitute"), c("q4_26", "pasta"), c("q4_3", "pork"), c("q4_27", "potatoes"), c("q4_25", "rice"), c("q4_23", "shellfish"), c("q4_4", "tofu"), c("q4_15", "turkey_lamb"), c("q4_10", "vegetables"), c("q4_14", "mixed"), c("q5", "mixed_reasoning"))
  rename_cols_prefix <- function(df, prefix_pairs) {
    for (pair in prefix_pairs) {
      df <- df |>
        rename_with(~ str_replace(., paste0("^", pair[1]), pair[2]), starts_with(pair[1]))
    }
    return(df)
  }
  rename_cols_suffix <- function(df, suffix_pairs) {
    for (pair in suffix_pairs) {
      df <- df |>
        rename_with(~ str_replace(., paste0(pair[1], "$"), pair[2]), ends_with(pair[1]))
    }
    return(df)
  }
  clean_data <- rename_cols_prefix(clean_data, prefixes)
  clean_data <- rename_cols_suffix(clean_data, suffixes)

  # Creating the columns to combine by iterating through the suffixes list and extracting names, flattening to chr class
  cols_to_combine <- map(suffixes, ~ .x[2]) |>
    flatten_chr()

  # Creating dining hall character by iterating through prefixes and extracting names, flattening to chr class, removing underscores
  hall_codes <- map(prefixes, ~ .x[2]) |>
    flatten_chr() |>
    str_replace_all("_", "")

  # Combining columns function
  combine_data <- function(clean_data, new_column_name, column_suffix, c1, c2, c3, c4, c6, c7, c8, c9, c10, c11, c12, c13) {
    clean_data <- clean_data |>
      mutate(!!new_column_name := case_when(
        dining_hall == "Campus Center Cafe" ~ !!sym(paste0(c1, "_", column_suffix)),
        dining_hall == "Chase Duckett" ~ !!sym(paste0(c2, "_", column_suffix)),
        dining_hall == "Compass Cafe" ~ !!sym(paste0(c3, "_", column_suffix)),
        dining_hall == "Comstock" ~ !!sym(paste0(c4, "_", column_suffix)),
        dining_hall == "Haynes" ~ !!sym(paste0(c6, "_", column_suffix)),
        dining_hall == "King Scales" ~ !!sym(paste0(c7, "_", column_suffix)),
        dining_hall == "Lamont" ~ !!sym(paste0(c8, "_", column_suffix)),
        dining_hall == "Northrop Gillett" ~ !!sym(paste0(c9, "_", column_suffix)),
        dining_hall == "Tyler" ~ !!sym(paste0(c10, "_", column_suffix)),
        dining_hall == "Other" ~ !!sym(paste0(c11, "_", column_suffix)),
        dining_hall == "Catering" ~ !!sym(paste0(c12, "_", column_suffix)),
        dining_hall == "Cutter Ziskind" ~ !!sym(paste0(c13, "_", column_suffix))
      ))
    return(clean_data)
  }

  # Call function to combine food types data
  for (item in cols_to_combine) {
    clean_data <- combine_data(clean_data, item, item, hall_codes[1], hall_codes[2], hall_codes[3], hall_codes[4], hall_codes[5], hall_codes[6], hall_codes[7], hall_codes[8], hall_codes[9], hall_codes[10], hall_codes[11], hall_codes[12])
  }

  # Selecting relevant columns --> this clean_data will give you accurate # of containers
  clean_data <- clean_data |>
    dplyr::select(date, dining_hall, all_of(cols_to_combine))

  # Getting rid of "mixed_reasoning" and "containers" in preparation for the pivot
  food_types <- cols_to_combine
  food_types <- setdiff(food_types, "mixed_reasoning")
  food_types <- setdiff(food_types, "containers")

  # Using pivot_longer to convert food type columns into just one column
  tidy_data <- clean_data |>
    pivot_longer(cols = all_of(food_types),
                 names_to = "type",
                 values_to = "weight",
                 values_drop_na = TRUE) |>
    dplyr::select(c(date, dining_hall, type, weight)) |>
    mutate(weight = as.numeric(weight)) |>
    filter(!is.na(weight))

  # Formatting data
  tidy_data$date <- mdy(tidy_data$date)
  clean_data$date <- mdy(clean_data$date)
  clean_data$containers <- as.numeric(clean_data$containers)

  # Subtract 2 lbs starting January 1st, 2025
  tidy_data <- tidy_data |>
    mutate(weight = if_else(
      date >= mdy('01-01-2025') & !dining_hall %in% c("Campus Center Cafe", "Compass Cafe"),
      weight - 2,
      weight
    )) |>
    mutate(weight = if_else(
      weight < 0, 0, weight
    ))

  return(list(clean_data = clean_data, tidy_data = tidy_data))
}
