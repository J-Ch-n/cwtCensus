######################################################
length_at_age = read.csv("./data-raw/raw_length_at_age.csv")

length_at_age = length_at_age[, c(2, 1, 3, 4)]
colnames(length_at_age) = c("age", "month", "mean", "sd")
usethis::use_data(length_at_age, overwrite = TRUE)
