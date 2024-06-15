reconstruct <- function(mat_dt, imp_dt, nat_mort, birth_month, max_ag_mnth_df,
                        iter, rel_info, detail, bootstrap, cr_level, hpd) {

    NM_AGE_IDX = 1
    NM_MNTH_IDX = 2
    NM_RATE_IDX = 3

    IP_BY_IDX = 1
    IP_MNTH_IDX = 2
    IP_AGE_IDX = 3
    IP_FSHRY_IDX = 4
    IP_IMP_IDX = 5

    MA_BY_IDX = 1
    MA_AGE_IDX = 2
    MA_MA_IDX = 3

# Setup Cohort Reconstruction ---------------------------------------------
    nat_mort_hp = r2r::hashmap()
    num_by = nrow(max_ag_mnth_df)
    num_by_age_month = sum(pmax((max_ag_mnth_df$max_age - 2) * 12, 0) +
                             pmin(max_ag_mnth_df$max_age - 1, 1) * ((max_ag_mnth_df$month - birth_month) %% 12 + 1))

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

    create_nat_mort_map <- function(record) {
        record = unname(record)
        key = c(record[NM_AGE_IDX] |> unname() |> as.integer(),
                record[NM_MNTH_IDX] |> unname() |> as.integer())
        value = record[NM_RATE_IDX] |> unname()
        nat_mort_hp[[key]] <<- value
    }

    apply(nat_mort, 1, create_nat_mort_map)


# Conduct Cohort Reconstruction -------------------------------------------

    # Use default mortality rates from
    # https://www.researchgate.net/publication/279530889_Sacramento_River_Winter_Chinook_Cohort_Reconstruction_Analysis_of_Ocean_Fishery_Impacts
    missing_mort_rate_handler <- function(age) {
      warning(paste0("Missing mortality rate for age ", age))

      if (age == 2) {
        return(0.0561)
      }

      return(0.0184)
    }

    find_CrI <- function(col, cr_level, center) {
      find_CrI_helper <- function(vec, cr_level, center) {
        l_cut_off = 0.5 - (cr_level / 2)
        return(quantile(vec, c(l_cut_off, 1 - l_cut_off)))
      }

      if (hpd) {
        return(split(mapply(hdi, object = col, credMass = cr_level), rep(1 : length(col), each = 2)))
      }

      return(split(mapply(find_CrI_helper, vec = col, cr_level = cr_level, center = center), rep(1 : length(col), each = 2)))
    }

    find_bt_median <- function(col) {
      return(mapply(median, x = col))
    }

    find_bt_sd <- function(col) {
      return(mapply(sd, x = col))
    }

    find_srr <- function(proj, act) {
      srr = mapply(function(p, a) (p - a) / a, p = proj, a = act)
      return(split(replace(srr, is.nan(srr), 0), rep(1 : length(proj), each = iter)))
    }

    find_mat_rate <- function(ocean_abundance, impact, maturation, natural_mort) {
      find_mat_rate_helper <- function(N, I, M, V) {
        rate = M / (N - I - V)
        rate[is.na(rate)] <- 0
        return(rate)
      }

      return(split(mapply(find_mat_rate_helper, N = ocean_abundance, I = impact, M = maturation, V = natural_mort), rep(1 : length(impact), each = iter)))
    }

    find_mortality <- function(maturation, abundance, rate) {
      return((abundance + maturation) * (rate) / (1 - rate))
    }

    find_mortality_rate <- function(age, month) {
      mort_rate = nat_mort_hp[[c(age, month)]]

      if (is.null(mort_rate)) {
        mort_rate = missing_mort_rate_handler(age)
      }

      return(mort_rate)
    }

    row_idx <- max_age_month_idx <- 1L
    prev_mnth_N = rep(0, times = iter)
    cur_yr = imp_dt[1, ..IP_BY_IDX] |> unlist() |> unname()
    cur_ag = max_ag_mnth_df$max_age[[max_age_month_idx]]
    cur_mnth = max_ag_mnth_df$month[[max_age_month_idx]]
    imp_col <- mat_col <- abd_col <- mort_col <- list()

    cohort_helper <- function(record) {
        cur_mat <- cur_imp <- cur_maturation_rows <- rep(0, iter)
        par_env = env_parent(current_env())
        adj_birth_month = (birth_month - 2L) %% 12L + 1L

        if (cur_mnth == adj_birth_month) {
          cur_maturation_rows = mat_dt[by == cur_yr & age == cur_ag, ..MA_MA_IDX]
          cur_maturation_mat = as.matrix(cur_maturation_rows)
          cur_maturation_mat_size = length(cur_maturation_mat)

          for (maturation in cur_maturation_mat[, 1]) {
            cur_mat = cur_mat + maturation
          }
        }

        nat_mort_rate = find_mortality_rate(as.integer(cur_ag), as.integer(cur_mnth))
        cur_mort = find_mortality(cur_mat, prev_mnth_N, nat_mort_rate)
        cur_impact_rows = imp_dt[by == cur_yr & age == cur_ag & month == cur_mnth, ..IP_IMP_IDX]
        cur_impact_mat = as.matrix(cur_impact_rows)
        cur_impact_mat_size = length(cur_impact_mat)

        for (impact in cur_impact_mat[, 1]) {
          cur_imp = cur_imp + impact
        }

        cur_ocean_abundance = cur_imp + cur_mort + prev_mnth_N + cur_mat

        abd_col <<- append(abd_col, list(cur_ocean_abundance))

        par_env$cohort |>
          set(i = row_idx, j = "by", value = cur_yr)
        par_env$cohort |>
          set(i = row_idx, j = "month", value = cur_mnth)
        par_env$cohort |>
          set(i = row_idx, j = "age", value = cur_ag)

        if (detail) {
          mort_col <<- append(mort_col, list(cur_mort))
          imp_col <<- append(imp_col, list(cur_imp))
          mat_col <<- append(mat_col, list(cur_mat))
        }

        prev_mnth_N <<- cur_ocean_abundance
        row_idx <<- row_idx + 1L

        if (cur_mnth == birth_month) {
          cur_ag <<- cur_ag - 1
        }

        if (cur_ag == 1) {
          if (cur_mnth == birth_month) {
            if (max_age_month_idx < num_by) {
              max_age_month_idx <<- max_age_month_idx + 1
            }
            cur_yr <<- max_ag_mnth_df$brood_year[[max_age_month_idx]]
          }

          cur_ag <<- max_ag_mnth_df$max_age[[max_age_month_idx]]
          cur_mnth <<- max_ag_mnth_df$month[[max_age_month_idx]]
          prev_mnth_N <<- 0
        } else {
          cur_mnth <<- (cur_mnth - 2) %% 12 + 1
        }
    }

    if (bootstrap) {
      sapply(1 : nrow(cohort), cohort_helper)
      cohort[, 'ocean_abundance' := .(abd_col)]
      cohort[, ':='(
        ocean_abundance_median = find_bt_median(ocean_abundance),
        ocean_abundance_sd = find_bt_sd(ocean_abundance)
      )][,
         ocean_abundance_CrI := find_CrI(ocean_abundance, cr_level, ocean_abundance_median)
      ]

      if (detail) {
        cohort[, 'impact' := .(imp_col)]
        cohort[, 'maturation' := .(mat_col)]
        cohort[, 'natural_mort' := .(mort_col)]

        cohort[, ':='(
          impact_median = find_bt_median(impact),
          impact_sd = find_bt_sd(impact),
          maturation_median = find_bt_median(maturation),
          maturation_sd = find_bt_sd(maturation),
          natural_mort_median = find_bt_median(natural_mort),
          natural_mort_sd = find_bt_sd(natural_mort)
        )][, ':='(
          impact_CrI = find_CrI(impact, cr_level, impact_median),
          maturation_CrI = find_CrI(maturation, cr_level, maturation_median),
          natural_mort_CrI = find_CrI(natural_mort, cr_level, natural_mort_median)
        )][,
          maturation_rate := .(find_mat_rate(ocean_abundance, impact, maturation, natural_mort))
         ][,
          maturation_rate_median := find_bt_median(maturation_rate)
         ][,
          maturation_rate_sd := find_bt_sd(maturation_rate)
         ][,
          maturation_rate_CrI := .(find_CrI(maturation_rate, cr_level, maturation_rate_median))
         ]

        annual_impact_rate_dt = cohort[,
                                       .('impact_rate' = .(rowSums(mapply(identity, x = impact))
                                                           / ocean_abundance[[length(ocean_abundance)]])),
                                       by = list(by, age)
                                      ][,
                                        impact_rate_median := find_bt_median(impact_rate)
                                      ][,
                                        impact_rate_sd := find_bt_sd(impact_rate)
                                      ][,
                                        impact_rate_CrI := .(find_CrI(impact_rate, cr_level, impact_rate_median))
                                      ]

        srr_dt = cohort[,
                        .(proj_mat = .(rowSums(mapply('*', x = maturation_rate, ocean_abundance))),
                          act_mat = .(rowSums(mapply(identity, x = maturation)))),
                        by = list(by)
                        ][,
                          srr := .(find_srr(proj_mat, act_mat))
                        ][,
                          srr_median := find_bt_median(srr)
                        ][,
                          srr_sd := find_bt_sd(srr)
                        ][,
                          srr_CrI := .(find_CrI(srr, cr_level, srr_median))
                        ]

        cohort_left_dt = cohort[,
                                .(early_abundance = .(ocean_abundance[[length(ocean_abundance)]])),
                                by = list(by)
                               ]
        els_dt = rel_info[cohort_left_dt,
                              on = .(brood_year == by)
                              ][,
                                els_rate := .(split(mapply('/', x = early_abundance, y = total_release),
                                rep(1 : length(early_abundance), each = iter)))
                              ][,
                                els_rate_median := find_bt_median(els_rate)
                              ][,
                                els_rate_sd := find_bt_sd(els_rate)
                              ][,
                                early_life_survial_rate_CrI := .(find_CrI(els_rate, cr_level, els_rate_median))
                              ]
      }
    } else {
      apply(cohort, 1, cohort_helper)
      cohort[, 'ocean_abundance' := .(abd_col)]

      if (detail) {
        cohort[, 'impact' := .(imp_col)]
        cohort[, 'maturation' := .(mat_col)]
        cohort[, 'natural_mort' := .(mort_col)]
        cohort[, 'mat_rate' := .(find_mat_rate(ocean_abundance,
                                               impact,
                                               maturation,
                                               natural_mort))]

        annual_impact_rate_dt = cohort[,
                                       .('impact_rate' = sum(unlist(impact)) / ocean_abundance[[length(ocean_abundance)]]),
                                       by = list(by, age)
                                       ]

        srr_dt = cohort[,
                        .('proj_mat' = sum(unlist(mat_rate) * unlist(ocean_abundance)),
                          'act_mat' = sum(unlist(maturation))),
                        by = list(by)
                       ][,
                        'srr' := fifelse(act_mat == 0, 0, (proj_mat - act_mat) / act_mat)
                       ]

        cohort_left_dt = cohort[,
                                .(early_abundance = ocean_abundance[[length(ocean_abundance)]]),
                                by = list(by)]
        els_dt = rel_info[cohort_left_dt, on = .(brood_year == by)
                             ][,
                              els_rate := early_abundance / total_release
                             ]
        rm(cohort_left_dt)
      }
    }

    if (detail) {
      return(list(cohort = cohort,
                  srr_dt = srr_dt,
                  els_dt = els_dt,
                  air_dt = annual_impact_rate_dt,
                  len = num_by_age_month))
    }

    return(list(cohort = cohort))
  }
