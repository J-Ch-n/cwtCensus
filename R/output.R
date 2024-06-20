# Creates an object containing all data for convenient querying.
# TODO: use indices to speed up query for each record.
create_output <- function(data, bootstrap, iter, birth_month, detail = T) {
  BY_IDX = 1
  AG_IDX = 2
  MNTH_IDX = 3
  ABD_IDX = 4
  NAT_MORT_IDX = 5
  IMP_IDX = 6
  MAT_IDX = 7
  ABD_MED_IDX = 8
  ABD_SD_IDX = 9
  ABD_CRI_IDX = 10
  IMP_MED_IDX = 11
  IMP_SD_IDX = 12
  MAT_MED_IDX = 13
  MAT_SD_IDX = 14
  NAT_MORT_MED_IDX = 15
  NAT_MORT_SD_IDX = 16
  IMP_CRI_IDX = 17
  MAT_CRI_IDX = 18
  NAT_MORT_CRI_IDX = 19
  MAT_RATE_IDX = 20
  MAT_RATE_MED_IDX = 21
  MAT_RATE_SD_IDX = 22
  MAT_RATE_CRI_IDX = 23
  IMP_RATE_IDX = 24
  IMP_RATE_MED_IDX = 25
  IMP_RATE_SD_IDX = 26
  IMP_RATE_CRI_IDX = 27
  TOTAL_REL_IDX = 28
  ERLY_ABD_IDX = 29
  ELS_RATE_IDX = 30
  ELS_RATE_MED_IDX = 31
  ELS_RATE_SD_IDX = 32
  ELS_RATE_CRI_IDX = 33
  PROJ_MAT_IDX = 34
  ACT_MAT_IDX = 35
  SRR_IDX = 36
  SRR_MED_IDX = 37
  SRR_SD_IDX = 38
  SRR_CRI_IDX = 39


  output_obj = list()
  output_helper <- function(info) {
    name = paste('by', info[[1]], 'age', info[[2]], 'month', info[[3]], sep = "-")
    summary_info = NA
    if (detail) {
      row_name = c(
        'ocean_abundance',
        'impact',
        'maturation',
        'natural_mort',
        'elsr',
        'srr')

      if (bootstrap) {
        data = rbind(info[[ABD_IDX]] |> unlist(),
                     info[[IMP_IDX]] |> unlist(),
                     info[[MAT_IDX]] |> unlist(),
                     info[[NAT_MORT_IDX]] |> unlist(),
                     info[[ELS_RATE_IDX]] |>  unlist(),
                     info[[SRR_IDX]] |> unlist()) |>
          round(digits = 2)

        data |> dimnames() = list(row_name, 1 : iter)

        oa_CrI = info[[ABD_CRI_IDX]] |> unlist()
        ip_CrI = info[[IMP_CRI_IDX]] |> unlist()
        mt_CrI = info[[MAT_CRI_IDX]] |> unlist()
        nt_CrI = info[[NAT_MORT_CRI_IDX]] |> unlist()
        el_CrI = info[[ELS_RATE_CRI_IDX]] |> unlist()
        sr_CrI = info[[SRR_CRI_IDX]] |> unlist()
        summary_info = matrix(data = c(info[[ABD_MED_IDX]] |> unlist(),
                                       info[[IMP_MED_IDX]] |> unlist(),
                                       info[[MAT_MED_IDX]] |> unlist(),
                                       info[[NAT_MORT_MED_IDX]] |> unlist(),
                                       info[[ELS_RATE_MED_IDX]] |> unlist(),
                                       info[[SRR_MED_IDX]] |> unlist(),
                                       info[[ABD_SD_IDX]] |> unlist(),
                                       info[[IMP_SD_IDX]] |> unlist(),
                                       info[[MAT_SD_IDX]] |> unlist(),
                                       info[[NAT_MORT_SD_IDX]] |> unlist(),
                                       info[[ELS_RATE_SD_IDX]] |> unlist(),
                                       info[[SRR_SD_IDX]] |> unlist(),
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
                                c('median',
                                  'sd',
                                  'CrI_low',
                                  'CrI_high')
                              )) |>
          round(digits = 2)
      } else {
        data = rbind(info[[4]] |> unlist(),
                     info[[6]] |> unlist(),
                     info[[7]] |> unlist(),
                     info[[5]] |> unlist(),
                     info[[12]] |>  unlist(),
                     info[[15]] |> unlist()) |>
          round(digits = 2)
        data |> dimnames() = list(row_name, 'value')
      }
    } else {
      if (bootstrap) {
        ABD_MED_IDX = 5
        ABD_SD_IDX = 6
        ABD_CRI_IDX = 7
        data = matrix(data = info[[ABD_IDX]] |> unlist(),
                          nrow = 1,
                          dimnames = list('ocean_abundance', 1 : iter))

        oa_CrI = info[[ABD_CRI_IDX]] |> unlist()
        summary_info = matrix(data = c(info[[ABD_MED_IDX]] |> unlist(),
                                       info[[ABD_SD_IDX]] |> unlist(),
                                       oa_CrI[1],
                                       oa_CrI[2]),
                              ncol = 4,
                              dimnames = list(
                                'ocean_abundance',
                                c('median',
                                  'sd',
                                  'CrI_low',
                                  'CrI_high')
                              )) |>
          round(digits = 2)
      }
    }

    elem = list(list(summary = summary_info, data = data))
    names(elem) = name
    output_obj <<- append(output_obj, elem)
  }

  if (detail) {
    comb = data$cohort |>
      left_join(data$air_dt, by = c("by", "age")) |>
      left_join(data$els_dt, by = c("by" = "brood_year")) |>
      left_join(data$srr_dt, by = c("by"))
    by(comb, list((comb$month - birth_month) %% 12 + 1, comb$age, comb$by), output_helper)
    rm(data)

    return(output_obj)
  } else if (bootstrap) {
    comb = data$cohort
    by(comb, list((comb$month - birth_month) %% 12 + 1, comb$age, comb$by), output_helper)

    return(output_obj)
  }

  return(data$cohort)
}

# Creates an object to display results by brood year.
create_output_by <- function(data, bootstrap, iter, detail = T) {
  output_obj = list()
  output_helper <- function(info) {
    name = paste('by', info[[1]][[1]], sep = "-")
    summary_info = NA

    if (detail) {
      data = list(ocean_abundance = info |> select(by, age, month, ocean_abundance),
                      natural_mort = info |> select(by, age, month, natural_mort),
                      impact = info |> select(by, age, month, impact),
                      maturation = info |> select(by, age, month, maturation))
      summary_info = info |> select(-c(ocean_abundance, natural_mort, impact, maturation))
    } else if (bootstrap) {
      data = list(ocean_abundance = info |> select(by, age, month, ocean_abundance))
      summary_info = info |> select(-c(ocean_abundance))
    }

    elem = list(list(summary = summary_info, data = data))#, debug = info))
    names(elem) = name
    output_obj <<- append(output_obj, elem)
  }

  if (detail) {
    comb = data$cohort |>
      left_join(data$air_dt, by = c("by", "age")) |>
      left_join(data$els_dt, by = c("by" = "brood_year")) |>
      left_join(data$srr_dt, by = c("by"))
    by(comb, list(comb$by), output_helper)
    rm(data)

    return(output_obj)
  } else if (bootstrap) {
    comb = data$cohort
    by(comb, list(comb$by), output_helper)

    return(output_obj)
  }

  return(data$cohort)
}

# Creates an object to display results by brood year and age.
create_output_by_age <- function(data, bootstrap, iter, detail = T) {
  output_obj = list()
  output_helper <- function(info) {
    name = paste('by', info[[1]][[1]], 'age', info[[2]][[1]], sep = "-")
    summary_info = NA
    row_name = paste('month', info[[3]], sep = "-")

    if (detail) {
      data = list(ocean_abundance = info |> select(by, age, month, ocean_abundance),
                      natural_mort = info |> select(by, age, month, natural_mort),
                      impact = info |> select(by, age, month, impact),
                      maturation = info |> select(by, age, month, maturation))
      summary_info = info |> select(-c(ocean_abundance, natural_mort, impact, maturation))
    } else if (bootstrap) {
      data = list(ocean_abundance = info |> select(by, age, month, ocean_abundance))
      summary_info = info |> select(-c(ocean_abundance))
    }

      elem = list(list(summary = summary_info, data = data))
      output_obj <<- append(output_obj, elem)
  }

  if (detail) {
    comb = data$cohort |>
      left_join(data$air_dt, by = c("by", "age")) |>
      left_join(data$els_dt, by = c("by" = "brood_year")) |>
      left_join(data$srr_dt, by = c("by"))
    by(comb, list(comb$age, comb$by), output_helper)
    rm(data)

    return(output_obj)

  } else if (bootstrap) {
    comb = data$cohort
    by(comb, list(comb$age, comb$by), output_helper)

    return(output_obj)
  }

  return(data$cohort)
}


cohort_summary <- function(data, bootstrap, detail) {
  if (bootstrap) {
    if (detail) {
      return(data$cohort |>
               left_join(data$air_dt, by = c("by", "age")) |>
               left_join(data$els_dt, by = c("by" = "brood_year")) |>
               left_join(data$srr_dt, by = c("by")) |>
               select(-c(ocean_abundance,
                         natural_mort,
                         impact,
                         maturation,
                         els_rate,
                         srr,
                         proj_mat,
                         act_mat)))
    } else {
      return(data$cohort |>
               select(-c(ocean_abundance)))

    }
  } else {
    if (detail) {
      return(data$cohort |>
               left_join(data$air_dt, by = c("by", "age")) |>
               left_join(data$els_dt, by = c("by" = "brood_year")) |>
               left_join(data$srr_dt, by = c("by")))

    } else {
      return(data$cohort)

    }
  }
}

cohort_data <- function(data, bootstrap, detail) {
  if (bootstrap) {
    if (detail) {
      return(data$cohort |>
               left_join(data$air_dt, by = c("by", "age")) |>
               left_join(data$els_dt, by = c("by" = "brood_year")) |>
               left_join(data$srr_dt, by = c("by")) |>
               select(c(ocean_abundance,
                         natural_mort,
                         impact,
                         maturation,
                         els_rate,
                         srr,
                         proj_mat,
                         act_mat)))
    } else {
      return(data$cohort |>
               select(c(ocean_abundance)))
    }
  } else {
    if (detail) {
      return(data$cohort |>
               left_join(data$air_dt, by = c("by", "age")) |>
               left_join(data$els_dt, by = c("by" = "brood_year")) |>
               left_join(data$srr_dt, by = c("by")))

    } else {
      return(data$cohort)

    }
  }
}
