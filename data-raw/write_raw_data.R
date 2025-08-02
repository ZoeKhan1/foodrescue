library(usethis)

raw_food <- read_csv("data-raw/Corrected_FRN Collection Form_December_10_2024.csv")

usethis::use_data(raw_food, overwrite = TRUE)
