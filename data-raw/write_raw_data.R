library(usethis)
library(readr)

raw_food <- read_csv("data-raw/Corrected_FRN_Collection_Form_August_3_2025.csv")

usethis::use_data(raw_food, overwrite = TRUE)
