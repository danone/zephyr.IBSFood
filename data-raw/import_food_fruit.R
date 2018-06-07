


food_fruit <- readr::read_delim("data-raw/foods_fruits.csv",
                           ";", escape_double = FALSE, trim_ws = TRUE)


devtools::use_data(food_fruit)
