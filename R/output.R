# Creates an object containing all datas for convenient querying.
create_output <- function(data) {
  # data$cohort
  # data$air_dt
  # data$els_dt
  # data$srr_dt

  # The inputs are provided per c(by, age, month).
  # This function converts each chunk of summary information into a list object keyed by c(by, age, month).
  output_obj = list()
  output_helper <- function(info) {
    name = paste('by', info[[1]], 'age', info[[2]], 'month', info[[3]], sep = "-")
    info = list(list(info))
    names(info) = name
    output_obj <<- append(output_obj, info)
  }

  dt_size = data$len
  # Apply a function that acts on the data.
  comb = data$cohort |>
         left_join(data$air_dt, by = c("by", "age")) |>
         left_join(data$els_dt, by = c("by" = "brood_year")) |>
         left_join(data$srr_dt, by = c("by"))
  by(comb, list(comb$by, comb$age, comb$month), output_helper)

  return(output_obj)
}
