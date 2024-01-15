### Data Preparation Function ###

data_prep <- function(rel, reco, size_at_age, rel_mort, nat_mort,
                      sex, spawn = 54, hatchery = 50, river = 46,
                      ocean_r = 10, ocean_c = 40, bootstrap, iter) {

  # Create mappings from column name to column indices.
  BY_IDX = 1
  MNTH_IDX = 2
  AGE_IDX = 3
  FSHRY_IDX = 4
  LOC_IDX = 5
  MAT_GRP_IDX = 6
  TOTAL_IDX = 7

  # Find the number of unique year - age pairs for a specific fishery in PROVIDED_FISHERIES.
  # PROVIDED_FISHERIES is passed in as a vector of fisheries.
  num_unique_year_age <- function(rel_reco_dt, provided_fisheries) {
    rel_reco_dt |>
      filter(fishery %in% provided_fisheries) |>
      distinct(brood_year, age) |>
      nrow()
  }

  # Find the hook and release mortality rate give LOCATION and MORT_MAP, which describes the
  # mapping between location and hook and releae mortality rate. MORT_MAP is a hashmap. LOCATION can be
  # any data type.
  # If MORT_MAP is NA, use the mapping provided in
  # "Sacramento River Winter Chinook Cohort Reconstruction: Analysis of Ocean Fishery Impacts"
  find_hook_rel_mort <- function(location, mort_map, recreational) {
    if (is.na(mort_map)) {
      if (recreational) {
        return(0.14)
      } else {
        return(0.26)
      }
    } else {
      # when constructing the hashmap, use (location, recreational) as key.
      return(mort_map[[c(location, recreational)]])
    }
  }

  # Find the percent harvestable for a specific MONTH and AGE given SIZE_LIMIT and SIZE_AGE_MAP.
  # Assume population distributes in normal distribution.
  find_percent_harvest<-function(Month, Age, size_limit, size_age_map){
    1-pnorm(size_limit, mean = size_age_map$mean[which(SizeAge$age == Age+1 & SizeAge$month == Month)], sd = size_age_map$sd[which(size_age_map$age == Age+1 & size_age_map$month == Month)])
  }

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
    mutate(maturation_grp = case_when(
      fishery %in% c(spawn, hatchery, river) ~ 1,
      TRUE ~ 2
    )) |>
    group_by(brood_year, month, age, fishery, location, maturation_grp) |>
    summarize(total_indiv = sum(est_num / prod_exp)) |>
    arrange(brood_year, maturation_grp, age, fishery)

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

  # Find min and max age.s
  age_min = min(rel_reco_dt$age)
  age_max = max(rel_reco_dt$age)
  age_range_cnt = age_max - age_min + 1

  yr_ag_cnt = num_unique_year_age(rel_reco_dt, c(spawn, hatchery, river))
  # print(yr_ag_cnt)

  ######################
  ### Intermediate 2 ###
  ######################

  # Declare and zero-fill a data table with columns "by", "age", and "maturation"
  # for storing maturation.

  m_init_vec = rep(0, yr_ag_cnt)

  maturation_dt = data.table(by = m_init_vec,
                             age = m_init_vec,
                             maturation = m_init_vec)

  # Declare and zero-fill a data table for storing impact and natural mortality.
  inm_init_vec = rep(0, by_range_cnt * age_range_cnt * month_range_cnt)

  impact_nat_mort_dt = data.table(by = inm_init_vec,
                                  month = inm_init_vec,
                                  age = inm_init_vec,
                                  impact = inm_init_vec,
                                  nat_mort = inm_init_vec)

  ### Variables for maturation ###



  prev_hat_esc = 0
  prev_sp_esc = 0
  prev_riv_harv = 0

  # The value is true if the previous year has data from spawn, hatchery,
  # or river, and false otherwise. The value defaults to false.
  prev_year_valid = F
  # view(rel_reco_dt)

  ### Variables for impact ###
  drop_off_mort = 0
  hook_rel_mort = 0
  harvest = 0


  ### Iterator variables ###
  prev_year = rel_reco_dt[1, ..BY_IDX] |> unlist()
  prev_age = rel_reco_dt[1, ..AGE_IDX] |> unlist()
  row_idx = 1L

  # Aggregate function for calculating impact, maturation, and natural mortality.
  find_imp_nat_mat <- function(record) {
    # print(record)
    par_env = env_parent(current_env())

    cur_year = record[BY_IDX] |> as.integer()
    cur_age = record[AGE_IDX] |> as.integer()

    cur_fishery = record[FSHRY_IDX] |> as.numeric()

    if (cur_fishery %in% c(spawn, river, hatchery)) {
      # if the record marks a changed brood year or age, push the maturation value onto the data table.
      if (((cur_year != prev_year && prev_year_valid)
           | (cur_year == prev_year && cur_age != prev_age))) {
        # print(record)
        par_env$maturation_dt |>
          set(i = row_idx, j = "maturation", value =
                par_env$prev_sp_esc +
                par_env$prev_riv_harv +
                par_env$prev_hat_esc)
        par_env$maturation_dt |>
          set(i = row_idx, j = "by", value = prev_year)
        par_env$maturation_dt |>
          set(i = row_idx, j = "age", value = prev_age)

        row_idx <<- row_idx + 1L
        prev_sp_esc = prev_riv_harv = prev_hat_esc = 0
        prev_year_valid <<- FALSE
      }
      # If fishery is spawning ground, river harvest, or hatchery escapement,
      # calculate maturation. Aggregate maturation to `maturation_temp` until we encounter
      # a new year.
      # print(c(cur_year, prev_year, prev_sp_esc, prev_riv_harv, prev_hat_esc))
      total_indiv = record[TOTAL_IDX] |> as.numeric()

      # Calculate number of individuals for each type of fishery.
      if (cur_fishery == spawn) {
        prev_sp_esc <<- prev_sp_esc + total_indiv
      } else if (cur_fishery == river) {
        prev_riv_harv <<- prev_riv_harv + total_indiv
      } else {
        prev_hat_esc <<- prev_hat_esc + total_indiv
      }

      prev_year_valid <<- TRUE

    } else if (record[FSHRY_IDX] %in% c(ocean_r, ocean_c)) {
      # If fishery is recreational or commercial ocean harvest, calculate natural
      # mortality and impact.

      # Calculate impact


    }

    prev_year <<- cur_year
    prev_age <<- cur_age
    return(NULL)
  }

  apply(rel_reco_dt, 1, find_imp_nat_mat)
  # The apply function doesn't have knowledge of the end of the data frame.
  # Thus, the solution is one off. We need to apply the last row of maturation here.
  maturation_dt |>
    set(i = row_idx, j = "maturation", value =
          prev_sp_esc +
          prev_riv_harv +
          prev_hat_esc)
  maturation_dt |>
    set(i = row_idx, j = "by", value = prev_year)
  maturation_dt |>
    set(i = row_idx, j = "age", value = prev_age)
  # view(maturation_dt)
  # view(rel_reco_dt)
}
