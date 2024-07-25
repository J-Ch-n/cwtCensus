######################################################
length_at_age = read.csv("./data-raw/raw_length_at_age.csv")

usethis::use_data(length_at_age, overwrite = TRUE)
