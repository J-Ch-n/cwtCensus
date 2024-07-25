test_that("check maturation only", {
  reco_mat_only = recovery[recovery$fishery == 54, ]
  expect_error(cohort_reconstruct(
    rel = release,
    reco = reco_mat_only,
    birth_month = 6L,
    last_month = 12L
  ))
})

test_that("check impact only", {
  reco_imp_only = recovery[recovery$fishery %in% c(40, 10), ]
  expect_error(cohort_reconstruct(
    rel = release,
    reco = reco_imp_only,
    birth_month = 6L,
    last_month = 12L
  ))
})

test_that("check one impact and one maturation", {
  reco_imp_mat_only = recovery[recovery$fishery == c(40, 54)
                           & recovery$run_year == 2002, ][c(1,6), ]
  expected_data <- data.table(
    by = rep(1999, 24),
    age = c(rep(3, 12), rep(2, 12)),
    month = c(5:1, 12:6, 5:1, 12:6),
    ocean_abundance = list(
      2.30973635880597, 2.35308841288518, 7.44154888516097, 7.5812212891012, 7.72351524142115,
      7.86847994665738, 8.01616553287944, 8.16662306902394, 8.31990458255385, 8.47606307744944,
      8.6351525525367, 8.79722802015991, 9.32033842002291, 9.8745545829533, 10.4617261538755,
      11.0838127633245, 11.7428905675308, 12.4411591773987, 13.1809489995034, 13.964729013606,
      14.795115012644, 15.6748783326975, 16.6069551020651, 17.5944560403198
    )
  )

  expect_equal(cohort_reconstruct(
    rel = release,
    reco = reco_imp_mat_only,
    birth_month = 6L,
    last_month = 12L,
    verbose = FALSE,
    bootstrap = FALSE,
    detail = FALSE
  ),
  expected_data)
})
