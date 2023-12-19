### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, spawn, hat, river,
                      ocean_r, ocean_c, bootstrap, iter) {
  # Left_join reco and rel and create a data table from the result.
  rel_reco_dt = reco |>
    left_join(rel, by = 'tag_code') |>
    setDT()

  # Create age column.
  rel_reco_dt[ , age := fcase(
    month >= birth_month, run_year - brood_year,
    month < birth_month, run_year - brood_year - 1
    # Group by BY, Month, Age, Fishery, Location.
  )][ , .(total_individual = sum(est_num) / prod_exp),
      by = c('brood_year', 'month', 'age', 'fishery', 'location'),
  ]
  # [, c('run_year', 'recovery_id', 'tag_code', 'length', 'sex', 'size_limit', 'release_month', 'prod_exp', 'birth_month') :=
  #     list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)]

  view(rel_reco_dt)
  #
  # rel_reco_ldt |>
  #   mutate(age = case_when(
  #     month >= birth_month ~ run_year - brood_year,
  #     TRUE ~ run_year - brood_year - 1
  #   )) |>
  #   group_by(brood_year, month, age, fishery, location) |>
  #   summarize(total_indiv = sum(est_num / prod_exp))
}
