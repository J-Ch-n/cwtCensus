### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, spawn, hat, river,
                      ocean_r, ocean_c, bootstrap, iter) {

  ######################
  ### Intermediate 1 ###
  ######################

  # Create the first intermediate data table with lazy data table.
  rel_reco_ldt = reco |>
    left_join(rel, by = 'tag_code') |>
    setDT() |>
    lazy_dt(immutable = F, key_by = c("brood_year", "month", "fishery", "location")) |>
    mutate(age = case_when(
      month >= birth_month ~ run_year - brood_year,
      TRUE ~ run_year - brood_year - 1
    )) |>
    group_by(brood_year, month, age, fishery, location) |>
    summarize(total_indiv = sum(est_num / prod_exp))

  # Materialize lazy data table.
  rel_reco_dt = as.data.table(rel_reco_ldt)

  # Find count for unique brood year.
  by_count = uniqueN(rel_reco_dt$brood_year)

  # Find min and max month.
  month_min = min(rel_reco_dt$month)
  month_max = max(rel_reco_dt$month)

  # Find min and max age.
  age_min = min(rel_reco_dt$age)
  age_max = max(rel_reco_dt$age)

  ######################
  ### Intermediate 2 ###
  ######################




}
