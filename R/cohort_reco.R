### Reconstruction Functions ###

cohort_reconstruct <- function(maturation_dt, impact_dt, nat_mort, birth_month, max_age_month_df,
                               detail, impact_fisheries = c(40, 10), bootstrap = T, alpha = 0.05) {
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

    # Takes in MATURATION, MONTH, OCEAN_ABUNDANCE, NAT_MORT_RATE and calculate the corresponding mortality count.
    find_mortality <- function(maturation, abundance, rate) {
      # TODO: handle maturation edge case.
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

    impact_column = list()
    maturation_column = list()
    abundance_column = list()
    mortality_column = list()

    # Stores the previous month and its ocean abundance.
    prev_mnth_N = 0

    cohort_helper <- function(record) {
        # if (cur_year == 1995 && cur_age == 3 && cur_month == 4) {
        #   browser()
        # }
        # Local variables to accumulate maturation and impact.
        # browser()
        cur_maturation <- cur_impact <- cur_maturation_rows <- 0
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


        # if (bootstrap) {
        abundance_column <<- append(abundance_column, list(cur_ocean_abundance))
        # } else {
        #   par_env$cohort |>
        #     set(i = row_idx, j = "ocean_abundance", value = cur_ocean_abundance)
        # }

        par_env$cohort |>
          set(i = row_idx, j = "by", value = cur_year)
        par_env$cohort |>
          set(i = row_idx, j = "month", value = cur_month)
        par_env$cohort |>
          set(i = row_idx, j = "age", value = cur_age)

        if (detail) {
          # par_env$cohort |>
          #   set(i = row_idx, j = "natural_mort", value = cur_mortality)
          mortality_column <<- append(mortality_column, list(cur_mortality))
          # par_env$cohort |>
          #   set(i = row_idx, j = "impact", value = cur_impact)
          impact_column <<- append(impact_column, list(cur_impact))
          # par_env$cohort |>
          #   set(i = row_idx, j = "maturation", value = cur_maturation)
          maturation_column <<- append(maturation_column, list(cur_maturation))
          # } else {
          #   par_env$cohort |>
          #     set(i = row_idx, j = "natural_mort", value = cur_mortality)
          #   par_env$cohort |>
          #     set(i = row_idx, j = "impact", value = cur_impact)
          #   par_env$cohort |>
          #     set(i = row_idx, j = "maturation", value = cur_maturation)
          # }
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

    find_ci_helper <- function(vec, alpha, center) {
      alpha = alpha / 2
      # if (any(unname(center - quantile(vec - center, c(1 - alpha, alpha))) < 0)) {
      #   browser()
      # }
      unname(center - quantile(vec - center, c(1 - alpha, alpha)))
    }

    find_ci <- function(col, alpha, center) {
      # browser()
      split(mapply(find_ci_helper, vec = col, alpha = alpha, center = center), rep(1 : length(col), each = 2))
    }

    find_bt_mean <- function(col) {
      #browser()
      mapply(mean, x = col)
    }

    if (bootstrap) {
      sapply(1 : nrow(cohort), cohort_helper)
      cohort[, 'ocean_abundance' := .(abundance_column)]

      if (detail) {
        cohort[, 'impact' := .(impact_column)]
        cohort[, 'maturation' := .(maturation_column)]
        cohort[, 'natural_mort' := .(mortality_column)]
      }

      cohort = cohort[, `:=`(
        impact_mean = find_bt_mean(impact),
        maturation_mean = find_bt_mean(maturation),
        natural_mort_mean = find_bt_mean(natural_mort)
      )][, `:=`(
        impact_ci = find_ci(impact, alpha, impact_mean),
        maturation_ci = find_ci(maturation, alpha, maturation_mean),
        natural_mort_ci = find_ci(natural_mort, alpha, natural_mort_mean)
      )]

      view(cohort)
    } else {
      apply(cohort, 1, cohort_helper)
      cohort[, 'ocean_abundance' := .(abundance_column)]

      if (detail) {
        cohort[, 'impact' := .(impact_column)]
        cohort[, 'maturation' := .(maturation_column)]
        cohort[, 'natural_mort' := .(mortality_column)]
        view(cohort)
      }

      view(cohort |> mutate(ocean_abundance = round(as.numeric(ocean_abundance), 2),
                            natural_mort = round(as.numeric(natural_mort), 2),
                            impact = round(as.numeric(impact), 2),
                            maturation = round(as.numeric(maturation), 2)))
    }
  }


