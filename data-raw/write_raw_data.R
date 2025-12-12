library(usethis)
library(readr)

raw_food <- read.csv("data-raw/Corrected_FRN_Collection_Form_December_12_2025.csv")

usethis::use_data(raw_food, overwrite = TRUE)
