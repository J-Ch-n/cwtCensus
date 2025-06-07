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

test_that("check one impact, one maturation, and one brood year", {
  reco_imp_mat_only = recovery[recovery$fishery %in% c(40, 54) &
                                 recovery$tag_code == "67001", ][c(1, 3), ]
  rel_impa_mat_only = release[release$brood_year == 2007, ]

  expected_data <- data.table(
    by = c(rep(2007, 36)),
    age = c(rep(4, 12), rep(3, 12), rep(2, 12)),
    month = c(rep(c(5:1, 12:6), 3)),
    ocean_abundance = c(
      21.927778, 22.339346, 22.758639, 23.185802, 23.620983, 24.064331,
      24.516001, 24.976148, 25.444932, 42.746042, 43.548353, 44.365724,
      45.198436, 46.046777, 46.911042, 47.791527, 48.688539, 49.602387,
      50.533388, 51.481862, 52.448139, 53.432552, 54.435442, 55.457155,
      58.754809, 62.248552, 65.950043, 69.871637, 74.026421, 78.428261,
      83.091848, 88.032746, 93.267446, 98.813417, 104.689168, 110.914310
    ) |> round(2)
  )


  actual = cohort_reconstruct(
    rel = release,
    reco = reco_imp_mat_only,
    birth_month = 6L,
    last_month = 12L,
    verbose = FALSE,
    bootstrap = FALSE,
    detail = FALSE
  )

  actual$ocean_abundance = round(actual$ocean_abundance, 2)

  expect_equal(actual, expected_data)
})

test_that("check one impact, one maturation, and multiple brood year", {
  reco_imp_mat_mlt_by = recovery[recovery$fishery %in% c(40, 54) & recovery$tag_code %in% c("68640", "67001"), ][c(1, 3, 10, 14), ]
  rel_imp_mat_mlt_by = release[release$brood_year %in% c(2007, 2008), ]

  expected_value <- data.table(
    by = c(rep(2007, 36), rep(2008, 24)),
    age = c(rep(4, 12), rep(3, 12), rep(2, 12), rep(3, 12), rep(2, 12)),
    month = c(rep(c(5:1, 12:6), 3), rep(c(5:1, 12:6), 2)),
    ocean_abundance = c(
      21.927778, 22.339346, 22.758639, 23.185802, 23.620983, 24.064331,
      24.516001, 24.976148, 25.444932, 42.746042, 43.548353, 44.365724,
      45.198436, 46.046777, 46.911042, 47.791527, 48.688539, 49.602387,
      50.533388, 51.481862, 52.448139, 53.432552, 54.435442, 55.457155,
      58.754809, 62.248552, 65.950043, 69.871637, 74.026421, 78.428261,
      83.091848, 88.032746, 93.267446, 98.813417, 104.689168, 110.914310,
      3.447044, 3.511743, 3.577656, 3.644806, 3.713216, 3.782910,
      3.853913, 3.926248, 3.999941, 4.075017, 4.151502, 4.229422,
      10.288569, 10.900359, 11.548529, 12.235240, 12.962785, 13.733592,
      14.550234, 15.415436, 16.332086, 17.303242, 18.332146, 19.422233
    )
  )
  actual = cohort_reconstruct(
    rel = rel_imp_mat_mlt_by,
    reco = reco_imp_mat_mlt_by,
    birth_month = 6L,
    last_month = 12L,
    verbose = FALSE,
    bootstrap = FALSE,
    detail = FALSE
  )

  actual$ocean_abundance = round(actual$ocean_abundance, 6)
  expect_equal(actual, expected_value)

})

test_that("check gapped brood years", {
  rel_gap_by = release[release$brood_year %in% c(2007, 2009) & release$tag_code %in% c("67001", "68678"), ]

  expected_value <- data.table(
    by = c(rep(2007, 48), rep(2009, 48)),
    age = c(rep(5, 12), rep(4, 12), rep(3, 12), rep(2, 12), rep(5, 12), rep(4, 12), rep(3, 12), rep(2, 12)),
    month = c(5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6),
    ocean_abundance = c(
      28.92339, 29.46626, 30.01932, 30.58276, 31.15678, 31.74157, 32.33734, 60.39449, 61.52805, 71.91810, 73.26795, 74.64313,
      451.33605, 459.80730, 468.43755, 477.22977, 486.18703, 495.31240, 504.60905, 514.08019, 523.72910, 597.70863, 666.06586, 695.78660,
      1113.97805, 1144.92167, 1166.41101, 1188.30369, 1210.60727, 1233.32948, 1256.47817, 1280.06134, 1304.08715, 1328.56391, 1353.50008, 1378.90428,
      1478.00094, 1565.88745, 1658.99996, 1757.64924, 1862.16450, 1972.89456, 2090.20898, 2214.49927, 2346.18025, 2485.69139, 2633.49829, 2790.09425,
      270.96602, 276.05185, 281.23314, 286.51168, 291.88929, 297.36784, 302.94922, 308.63535, 325.11321, 565.58902, 832.44902, 1110.94202,
      3529.02371, 3655.79891, 3724.41557, 3794.32011, 3865.53671, 3938.08999, 4012.00505, 4553.19414, 6133.70826, 8101.47499, 12053.16450, 13811.76253,
      26041.68880, 27323.41975, 27836.26026, 28358.72641, 28890.99887, 29433.26168, 29985.70237, 30655.74284, 31794.30151, 32828.46629, 33765.23199, 34415.67304,
      42161.38038, 44668.42652, 47324.54938, 50138.61352, 53120.01063, 56278.69084, 59625.19594, 63170.69459, 66927.01957, 70906.70725, 75123.03947, 79590.08785
    )
  )

  actual = cohort_reconstruct(
    rel = rel_gap_by,
    reco = recovery,
    birth_month = 6L,
    last_month = 12L,
    verbose = FALSE,
    bootstrap = FALSE,
    detail = FALSE
  )

  actual$ocean_abundance = round(actual$ocean_abundance, 5)

  expect_equal(actual, expected_value)
})

if ("package:dplyr" %in% search()) detach("package:dplyr", unload = TRUE)
