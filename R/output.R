# Creates an object containing all datas for convenient querying.
create_output <- function(data, bootstrap, iter, detail = T) {
  # data$cohort
  # data$air_dt
  # data$els_dt
  # data$srr_dt

  # The inputs are provided per c(by, age, month).
  # This function converts each chunk of summary information into a list object keyed by c(by, age, month).
  # TODO: This could be optimized.
  output_obj = list()
  output_helper <- function(info) {
    name = paste('by', info[[1]], 'age', info[[2]], 'month', info[[3]], sep = "-")
    row_name = c(
      'ocean_abundance',
      'impact',
      'maturation',
      'natural_mort',
      'elsr',
      'srr')
    # Raw data
    raw_data = rbind(info[['ocean_abundance']] |> unlist(),
                     info[['impact']] |> unlist(),
                     info[['maturation']] |> unlist(),
                     info[['natural_mort']] |> unlist(),
                     info[['early_life_survival_rate']] |>  unlist(),
                     info[['srr']] |> unlist())
    raw_data |> dimnames() = list(row_name, 1 : iter)

    list(ocean_abundance = info[['ocean_abundance']] |> unlist(),
                  impact = info[['impact']] |> unlist(),
                  maturation = info[['maturation']] |> unlist(),
                  natural_mort = info[['natural_mort']] |> unlist(),
                  els_rate = info[['early_life_survival_rate']] |>  unlist(),
                  srr = info[['srr']] |> unlist())
    # Summary information
    oa_CrI = info[['ocean_abundance_CrI']] |> unlist()
    ip_CrI = info[['impact_CrI']] |> unlist()
    mt_CrI = info[['maturation_CrI']] |> unlist()
    nt_CrI = info[['natural_mort_CrI']] |> unlist()
    el_CrI = info[['early_life_survial_rate_CrI']] |> unlist()
    sr_CrI = info[['srr_CrI']] |> unlist()
    summary_info = matrix(data = c(info[['ocean_abundance_median']] |> unlist(),
                                   info[['impact_median']] |> unlist(),
                                   info[['maturation_median']] |> unlist(),
                                   info[['natural_mort_median']] |> unlist(),
                                   info[['early_life_survival_rate_median']] |> unlist(),
                                   info[['srr_median']] |> unlist(),
                                   info[['ocean_abundance_sd']] |> unlist(),
                                   info[['impact_sd']] |> unlist(),
                                   info[['maturation_sd']] |> unlist(),
                                   info[['natural_mort_sd']] |> unlist(),
                                   info[['early_life_survival_rate_sd']] |> unlist(),
                                   info[['srr_sd']] |> unlist(),
                                   oa_CrI[1],
                                   ip_CrI[1],
                                   mt_CrI[1],
                                   nt_CrI[1],
                                   el_CrI[1],
                                   sr_CrI[1],
                                   oa_CrI[2],
                                   ip_CrI[2],
                                   mt_CrI[2],
                                   nt_CrI[2],
                                   el_CrI[2],
                                   sr_CrI[2]),
                          ncol = 4,
                          dimnames = list(
                            row_name,
                            c(
                              'median',
                              'sd',
                              'CrI_low',
                              'CrI_high'
                            )
                          )) |>
      round(digits = 2)

    elem = list(list(summary = summary_info, raw = raw_data, debug = info))
    names(elem) = name
    output_obj <<- append(output_obj, elem)
  }

  # Apply a function that acts on the data.
  comb = data$cohort |>
         left_join(data$air_dt, by = c("by", "age")) |>
         left_join(data$els_dt, by = c("by" = "brood_year")) |>
         left_join(data$srr_dt, by = c("by"))
  by(comb, list(comb$by, comb$age, comb$month), output_helper)
  rm(data)

  return(output_obj)
}
