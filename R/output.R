# Creates an object containing all results for convenient querying.
create_output <- function(final_data) {
  # final_data$cohort
  # final_data$air_dt
  # final_data$els_dt
  # final_data$srr_dt
  dt_size = final_data$len
  # Apply a function that acts on the data.
  final_data$cohort |>
    left_join(final_data$air_dt) |>
    left_join(final_data$els_dt) |>
    left_join(final_data$srr_dt) |>
    group_by(by, age, month) |>
    mutate(temp = output_helper(by, age, month,
                                ocean_abundance, natural_mort, impact,
                                maturation, ocean_abundance_median, ocean_abundance_sd,
                                impact_median, impact_sd, maturation_median,
                                maturation_sd, natural_mort_median, natural_mort_sd,
                                ocean_abundance_CrI, impact_CrI, maturation_CrI,
                                natural_mort_CrI, maturation_rate, maturation_rate_median,
                                maturation_rate_sd, maturation_rate_CrI, proj_mat,
                                act_mat, srr, srr_median,
                                srr_sd, srr_CrI, impact_rate,
                                impact_rate_median, impact_rate_sd, impact_rate_CrI,
                                brood_year, total_release, early_abundance,
                                early_life_survival_rate, early_life_survival_rate_median,
                                early_life_survival_rate_sd,early_life_survvial_rate_CrI))

  return(final_data)
}
