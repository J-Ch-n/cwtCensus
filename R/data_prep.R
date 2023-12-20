### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, spawn, hatchery, river,
                      ocean_r, ocean_c, bootstrap, iter) {

  # Create mappings from column name to column indices.
  by_idx = 1
  mth_idx = 2
  age_idx = 3
  fshry_idx = 4
  loc_idx = 5
  total_idx = 6

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

  # Find min and max brood year.
  by_min = min(rel_reco_dt$brood_year)
  by_max = max(rel_reco_dt$brood_year)
  by_range_cnt = by_max - by_min + 1


  # Find min and max month.
  month_min = min(rel_reco_dt$month)
  month_max = max(rel_reco_dt$month)
  month_range_cnt = month_max - month_min + 1

  # Find min and max age.
  age_min = min(rel_reco_dt$age)
  age_max = max(rel_reco_dt$age)
  age_range_cnt = age_max - age_min + 1

  ######################
  ### Intermediate 2 ###
  ######################

  # Assume brood years are densely and contiguously distributed.
  # Declare and zero-fill a data table with columns "by", "age", and "maturation"
  # for storing maturation.

  m_init_vec = rep.int(0, by_range_cnt * age_range_cnt)

  maturation_dt = data.table(by = m_init_vec,
                             age = m_init_vec,
                             maturation = m_init_vec)

  # Declare and zero-fill a data table for storing impact and natural mortality.
  inm_init_vec = rep.int(0, by_range_cnt * age_range_cnt * month_range_cnt)

  impact_nat_mort_dt = data.table(by = inm_init_vec,
                                  month = inm_init_vec,
                                  age = inm_init_vec,
                                  impact = inm_init_vec,
                                  nat_mort = inm_init_vec)
  row_idx = 1
  maturation_temp = 0
  prev_year = rel_reco_dt[1, ..by_idx]
  prev_age = rel_reco_dt[1, ..age_idx]

  # Aggregate function for calculating impact, maturation, and natural mortality.
  find_imp_nat_mat <- function(record) {
    par_env = env_parent(cur_env())

    prev_year = record[by_idx]
    prev_age = record[age_idx]

    if (record[fshry_idx] %in% c(spawn, river, hatchery)) {
      # If fishery is spawning ground, river harvest, or hatchery escapement,
      # calculate maturation. Aggregate maturation to `maturation_temp` until we encounter
      #
      par_env(maturation_dt) |>
        set(i = row_idx, j = "maturation")

    } else if (record[fshry_id] %in% c(ocean_r, ocean_c)) {
      # If fishery is recreational or commercial ocean harvest, calculate natural
      # mortality and impact.
    }
  }
}
