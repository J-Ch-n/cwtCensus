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

  find_ag_imp <- function(info) {
    info[[IMP_IDX]] |> unlist() |> sum() / info[[IMP_IDX]][[1]] |> unlist()
  }

  find_ag_smry <- function(info) {
    row_name = c('s1')
    data = NA
    if (bootstrap) {
      data = rbind(info[1, ][[ELS_RATE_IDX]] |> unlist()) |>
        round(digits = 2)

      data |> dimnames() = list(row_name, 1 : iter)

      el_CrI = info[1, ][[ELS_RATE_CRI_IDX]] |> unlist()
      summary_info = matrix(data = c(info[1, ][[ELS_RATE_MED_IDX]] |> unlist(),
                                     info[1, ][[ELS_RATE_SD_IDX]] |> unlist(),
                                     el_CrI[1],
                                     el_CrI[2]),
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
      summary_info = rbind(info[1, ][[12]] |> unlist()) |>
        round(digits = 2)
      summary_info |> dimnames() = list(row_name, 1 : iter)
    }

    list(summary = summary_info, data = data)
  }

  find_by_smry <- function(info) {
    row_name = c('srr')
    data = NA
    if (bootstrap) {
      data = rbind(info[1, ][[SRR_IDX]] |> unlist()) |>
        round(digits = 2)

      dimnames(data) = list(row_name, 1 : iter)

      sr_CrI = info[1, ][[SRR_CRI_IDX]] |> unlist()
      summary_info = matrix(data = c(info[1, ][[SRR_MED_IDX]] |> unlist(),
                                     info[1, ][[SRR_SD_IDX]] |> unlist(),
                                     sr_CrI[1],
                                     sr_CrI[2]),
                            ncol = 4,
                            dimnames = list(
                              row_name,
                              c('median',
                                'sd',
                                'CrI_low',
                                'CrI_high'))) |>
        round(digits = 5)
    } else {
      summary_info = rbind(info[1, ][[15]] |> unlist()) |>
        round(digits = 5)

      dimnames(summary_info) = list(row_name, 1 : iter)
    }

    list(summary = summary_info, data = data)
  }

  month_helper <- function(info) {
      summary_info = NA
      if (detail) {
        row_name = c(
          'ocean_abundance',
          'impact',
          'maturation',
          'natural_mort')

        if (bootstrap) {
          data = rbind(info[[ABD_IDX]] |> unlist(),
                       info[[IMP_IDX]] |> unlist(),
                       info[[MAT_IDX]] |> unlist(),
                       info[[NAT_MORT_IDX]] |> unlist()) |>
            round(digits = 5)

          data |> dimnames() = list(row_name, 1 : iter)

          oa_CrI = info[[ABD_CRI_IDX]] |> unlist()
          ip_CrI = info[[IMP_CRI_IDX]] |> unlist()
          mt_CrI = info[[MAT_CRI_IDX]] |> unlist()
          nt_CrI = info[[NAT_MORT_CRI_IDX]] |> unlist()
          summary_info = matrix(data = c(info[[ABD_MED_IDX]] |> unlist(),
                                         info[[IMP_MED_IDX]] |> unlist(),
                                         info[[MAT_MED_IDX]] |> unlist(),
                                         info[[NAT_MORT_MED_IDX]] |> unlist(),
                                         info[[ABD_SD_IDX]] |> unlist(),
                                         info[[IMP_SD_IDX]] |> unlist(),
                                         info[[MAT_SD_IDX]] |> unlist(),
                                         info[[NAT_MORT_SD_IDX]] |> unlist(),
                                         oa_CrI[1],
                                         ip_CrI[1],
                                         mt_CrI[1],
                                         nt_CrI[1],
                                         oa_CrI[2],
                                         ip_CrI[2],
                                         mt_CrI[2],
                                         nt_CrI[2]),
                                ncol = 4,
                                dimnames = list(
                                  row_name,
                                  c('median',
                                    'sd',
                                    'CrI_low',
                                    'CrI_high')
                                )) |>
            round(digits = 5)
        } else {
          data = rbind(info[[4]] |> unlist(),
                       info[[6]] |> unlist(),
                       info[[7]] |> unlist(),
                       info[[5]] |> unlist()) |>
            round(digits = 5)
          dimnames(data) = list(row_name, 'value')
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
            round(digits = 5)
        }
      }

      return(list(data = data, summary = summary_info))
  }

  age_helper <- function(input) {
    if (detail) {
      append(by(input, input$month, month_helper), list(age_summary = find_ag_smry(input)))
    } else {
      by(input, input$month, month_helper)
    }
  }

  by_helper <- function(input) {
    if (detail) {
      append(by(input, input$age, age_helper), list(by_summary = find_by_smry(input)))
    } else {
      by(input, input$age, age_helper)
    }
  }

  if (detail) {
    comb = data$cohort |>
      dplyr::left_join(data$air_dt, by = c("by", "age")) |>
      dplyr::left_join(data$els_dt, by = c("by" = "brood_year")) |>
      dplyr::left_join(data$srr_dt, by = c("by"))

    result = by(comb, list(comb$by), by_helper)
    rm(comb)
    rm(data)

    return(result)
  } else if (bootstrap) {
    comb = data$cohort
    result = by(comb, list(comb$by), by_helper)
    rm(comb)
    rm(data)

    return(result)
  }

  data$cohort$ocean_abundance = data$cohort$ocean_abundance |>
    unlist()

  return(data$cohort)
}
