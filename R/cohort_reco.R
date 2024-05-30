### Reconstruction Functions ###

cohort_reconstruct <- function(maturation_dt, impact_dt, nat_mort,
                               birth_month, max_age_month_df, iter,
                               release_info, detail, impact_fisheries = c(40, 10), bootstrap = T,
                               alpha = 0.05) {
    # Natural mortality indices
    NM_AGE_IDX = 1
    NM_RATE_IDX = 2

    # Impact indices
    IP_BY_IDX = 1
    IP_MNTH_IDX = 2
    IP_AGE_IDX = 3
    IP_FSHRY_IDX = 4
    IP_IMP_IDX = 5

    # Maturation indices
    MA_BY_IDX = 1
    MA_AGE_IDX = 2
    MA_MA_IDX = 3

    ###########################################
    ### Step 1: Setup Cohort Reconstruction ###
    ###########################################
    nat_mort_hp = hashmap()
    num_by = nrow(max_age_month_df)
    num_by_age_month = sum(pmax((max_age_month_df$max_age - 2) * 12, 0) +
                             pmin(max_age_month_df$max_age - 1, 1) * ((max_age_month_df$month - birth_month) %% 12 + 1))
    if (num_by_age_month <= 0) {
      stop("Size is less than or equal to zero.")
    }

    init_vec = rep(0, num_by_age_month)

    if (detail) {
      cohort = data.table(by = init_vec,
                          age = init_vec,
                          month = init_vec,
                          ocean_abundance = init_vec,
                          natural_mort = init_vec,
                          impact = init_vec,
                          maturation = init_vec)
    } else {
      cohort = data.table(by = init_vec,
                          age = init_vec,
                          month = init_vec,
                          ocean_abundance = init_vec)
    }


    # Create a hash map from NAT_MORT. The resulting hash map NAT_MORT_MAP has AGE as its key.
    # Each key corresponds to the natural mortality rate of that age.
    create_nat_mort_map <- function(record) {
        record = unname(record)
        key = record[NM_AGE_IDX] |> unname() |> as.integer()
        value = record[NM_RATE_IDX] |> unname()
        nat_mort_hp[[key]] <<- value
    }

    apply(nat_mort, 1, create_nat_mort_map)

    ###########################################
    ### Step 2: Reconstruct Ocean Abundance ###
    ###########################################

    # Use default mortality rates from https://www.researchgate.net/publication/279530889_Sacramento_River_Winter_Chinook_Cohort_Reconstruction_Analysis_of_Ocean_Fishery_Impacts
    handle_missing_mort_rate <- function(age) {
      warning(paste0("Missing mortality rate for age ", age))
      if (age == 2) {
        return(0.0561)
      }

      return(0.0184)
    }

    # Find the confidence interval of a statistic in the bootstrap sampling distribution.
    find_ci <- function(col, alpha, center) {
      find_ci_helper <- function(vec, alpha, center) {
        alpha = alpha / 2
        unname(center - quantile(vec - center, c(1 - alpha, alpha)))
      }
      return(split(mapply(find_ci_helper, vec = col, alpha = alpha, center = center), rep(1 : length(col), each = 2)))
    }

    # Find the center of a bootstrap sampling distribution.
    find_bt_mean <- function(col) {
      return(mapply(mean, x = col))
    }

    # Find bootstrapped srr.
    find_srr <- function(proj, act) {
      srr = mapply(function(p, a) (p - a) / a, p = proj, a = act)
      return(split(replace(srr, is.nan(srr), 0), rep(1 : length(proj), each = iter)))
    }

    # Find bootstrapped maturation rate.
    find_mat_rate <- function(ocean_abundance, impact, maturation, natural_mort) {
      find_mat_rate_helper <- function(N, I, M, V) {
        rate = M / (N - I - V)
        rate[is.na(rate)] <- 0
        return(rate)
      }

      return(split(mapply(find_mat_rate_helper, N = ocean_abundance, I = impact, M = maturation, V = natural_mort), rep(1 : length(impact), each = iter)))
    }

    # Takes in MATURATION, MONTH, OCEAN_ABUNDANCE, NAT_MORT_RATE and calculate the corresponding mortality count.
    find_mortality <- function(maturation, abundance, rate) {
      return(abundance * (rate) / (1 - rate))
    }

    # Finds the morality rate of a particular AGE. If the age doesn't exist, invoke the error handler.
    find_mortality_rate <- function(age) {
      mort_rate = nat_mort_hp[[age]]
      if (is.null(mort_rate)) {
        mort_rate = handle_missing_mort_rate(age)
      }

      return(mort_rate)
    }

    # Variables for Reconstruction
    row_idx <- max_age_month_idx <- 1L

    cur_year = impact_dt[1, ..IP_BY_IDX] |> unlist() |> unname()
    cur_age = max_age_month_df$max_age[[max_age_month_idx]]
    cur_month = max_age_month_df$month[[max_age_month_idx]]

    impact_column <- maturation_column <- abundance_column <- mortality_column <- list()

    # Stores the previous month and its ocean abundance.
    prev_mnth_N = rep(0, times = iter)

    cohort_helper <- function(record) {
        # Local variables to accumulate maturation and impact.
        cur_maturation <- cur_impact <- cur_maturation_rows <- rep(0, iter)
        par_env = env_parent(current_env())
        adj_birth_month = (birth_month - 2L) %% 12L + 1L

        # If the age is about to change, query and account for maturation.
        if (cur_month == adj_birth_month) {
          cur_maturation_rows = maturation_dt[by == cur_year & age == cur_age, ..MA_MA_IDX]
          cur_maturation_mat = as.matrix(cur_maturation_rows)
          cur_maturation_mat_size = length(cur_maturation_mat)

          for (maturation in cur_maturation_mat[, 1]) {
            cur_maturation = cur_maturation + maturation
          }
        }

        # Query for natural mortality rate.
        nat_mort_rate = find_mortality_rate(as.integer(cur_age))
        cur_mortality = find_mortality(cur_maturation, prev_mnth_N, nat_mort_rate)

        # Every row needs an impact calculation.
        cur_impact_rows = impact_dt[by == cur_year & age == cur_age & month == cur_month, ..IP_IMP_IDX] # & (fishery %in% impact_fisheries)
        cur_impact_mat = as.matrix(cur_impact_rows)
        cur_impact_mat_size = length(cur_impact_mat)

        for (impact in cur_impact_mat[, 1]) {
          cur_impact = cur_impact + impact
        }

        cur_ocean_abundance = cur_impact + cur_mortality + prev_mnth_N + cur_maturation

        abundance_column <<- append(abundance_column, list(cur_ocean_abundance))

        par_env$cohort |>
          set(i = row_idx, j = "by", value = cur_year)
        par_env$cohort |>
          set(i = row_idx, j = "month", value = cur_month)
        par_env$cohort |>
          set(i = row_idx, j = "age", value = cur_age)

        if (detail) {
          mortality_column <<- append(mortality_column, list(cur_mortality))
          impact_column <<- append(impact_column, list(cur_impact))
          maturation_column <<- append(maturation_column, list(cur_maturation))
        }

        prev_mnth_N <<- cur_ocean_abundance
        row_idx <<- row_idx + 1L

        if (cur_month == birth_month) {
          cur_age <<- cur_age - 1
        }

        if (cur_age == 1) {
          if (cur_month == birth_month) {
            if (max_age_month_idx < num_by) {
              max_age_month_idx <<- max_age_month_idx + 1
            }
            cur_year <<- max_age_month_df$brood_year[[max_age_month_idx]]
          }

          cur_age <<- max_age_month_df$max_age[[max_age_month_idx]]
          cur_month <<- max_age_month_df$month[[max_age_month_idx]]
          prev_mnth_N <<- 0
        } else {
          cur_month <<- (cur_month - 2) %% 12 + 1
        }
    }

    # Conduct cohort reconstruction.
    if (bootstrap) {
      sapply(1 : nrow(cohort), cohort_helper)
      cohort[, 'ocean_abundance' := .(abundance_column)]

      if (detail) {
        cohort[, 'impact' := .(impact_column)]
        cohort[, 'maturation' := .(maturation_column)]
        cohort[, 'natural_mort' := .(mortality_column)]

        # Construct confidence interval for ocean abundance, impact, maturation, and natural mortality.
        cohort[, ':='(
          ocean_abundance_mean = find_bt_mean(ocean_abundance),
          impact_mean = find_bt_mean(impact),
          maturation_mean = find_bt_mean(maturation),
          natural_mort_mean = find_bt_mean(natural_mort)
        )][, ':='(
          ocean_abundance_ci = find_ci(ocean_abundance, alpha, ocean_abundance_mean),
          impact_ci = find_ci(impact, alpha, impact_mean),
          maturation_ci = find_ci(maturation, alpha, maturation_mean),
          natural_mort_ci = find_ci(natural_mort, alpha, natural_mort_mean)
        )][,
           maturation_rate := .(find_mat_rate(ocean_abundance, impact, maturation, natural_mort))
         ][,
          maturation_rate_mean := find_bt_mean(maturation_rate)
         ][,
          maturation_rate_ci := .(find_ci(maturation_rate, alpha, maturation_rate_mean))
         ]

        # Calculate annual impact rate and its confidence interval.
        annual_impact_rate_dt = cohort[,
                                       .('impact_rate' = .(rowSums(mapply(identity, x = impact)) / ocean_abundance[[length(ocean_abundance)]])),
                                       by = list(by, age)
                                      ][,
                                        impact_rate_mean := find_bt_mean(impact_rate)
                                      ][,
                                        impact_rate_ci := .(find_ci(impact_rate, alpha, impact_rate_mean))
                                      ]

        # Calculate spawner reduction rate and its confidence interval.
        srr_dt = cohort[,
                        .(proj_mat = .(rowSums(mapply('*', x = maturation_rate, ocean_abundance))),
                          act_mat = .(rowSums(mapply(identity, x = maturation)))),
                        by = list(by)
                        ][,
                          srr := .(find_srr(proj_mat, act_mat))
                        ][,
                          srr_mean := find_bt_mean(srr)
                        ][,
                          srr_ci := .(find_ci(srr, alpha, srr_mean))
                        ]

        # Calculate early life survival rate and its confidence interval.
        cohort_left_dt = cohort[,
                                .(early_abundance = .(ocean_abundance[[length(ocean_abundance)]])),
                                by = list(by)
                               ]
        els_dt = release_info[cohort_left_dt,
                              on = .(brood_year == by)
                              ][,
                                early_life_survival_rate := .(split(mapply('/', x = early_abundance, y = total_release),
                                rep(1 : length(early_abundance), each = iter)))
                              ][,
                                early_life_survival_rate_mean := find_bt_mean(early_life_survival_rate)
                              ][,
                                early_life_survvial_rate_ci := .(find_ci(early_life_survival_rate, alpha, early_life_survival_rate_mean))
                              ]
      }
    } else {
      apply(cohort, 1, cohort_helper)
      cohort[, 'ocean_abundance' := .(abundance_column)]

      if (detail) {
        cohort[, 'impact' := .(impact_column)]
        cohort[, 'maturation' := .(maturation_column)]
        cohort[, 'natural_mort' := .(mortality_column)]
        cohort[, 'mat_rate' := .(find_mat_rate(ocean_abundance,
                                               impact,
                                               maturation,
                                               natural_mort))]

        # Calculate annual impact rate.
        annual_impact_rate_dt = cohort[,
                                       .('impact_rate' = sum(unlist(impact)) / ocean_abundance[[length(ocean_abundance)]]),
                                       by = list(by, age)]

        # Calculate spawner reduction rate. Handle division by zero by assigning 0 to NaN.
        srr_dt = cohort[,
                        .('proj_mat' = sum(unlist(mat_rate) * unlist(ocean_abundance)),
                          'act_mat' = sum(unlist(maturation))),
                        by = list(by)
                       ][,
                         'srr' := fifelse(act_mat == 0, 0, (proj_mat - act_mat) / act_mat)
                       ]

        # Calculate early life survival rate.
        cohort_left_dt = cohort[,
                                .(early_abundance = ocean_abundance[[length(ocean_abundance)]]),
                                by = list(by)]
        els_dt = release_info[cohort_left_dt, on = .(brood_year == by)
                             ][,
                               early_life_survival_rate := early_abundance / total_release
                             ]
        rm(cohort_left_dt)
      }

      cohort = cohort |> mutate(ocean_abundance = round(as.numeric(ocean_abundance), 2),
                            natural_mort = round(as.numeric(natural_mort), 2),
                            impact = round(as.numeric(impact), 2),
                            maturation = round(as.numeric(maturation), 2))
    }

    if (detail) {
      return(list(cohort = cohort,
                  srr_dt = srr_dt,
                  els_dt = els_dt,
                  air_dt = annual_impact_rate_dt))
    }

    return(list(cohort = cohort))
  }


