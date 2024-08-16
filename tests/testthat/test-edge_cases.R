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
    by = rep(2007, 36),
    age = c(rep(4, 12), rep(3, 12), rep(2, 12)),
    month = rep(c(5:1, 12:6), 3),
    ocean_abundance = c(0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 16.82352, 17.13929, 17.46098,
                        39.71649, 40.46193, 41.22138, 41.99507, 42.78329, 43.58630, 44.40438, 45.23782, 46.08690, 46.95192, 47.83317, 48.73096,
                        51.62866, 54.69866, 57.95121, 61.39716, 65.04803, 68.91599, 73.01395, 77.35558, 81.95538, 86.82870, 91.99181, 97.46192)
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

  actual$ocean_abundance = round(actual$ocean_abundance, 5)

  expect_equal(actual, expected_data)
})

test_that("check one impact, one maturation, and multiple brood year", {
  reco_imp_mat_mlt_by = recovery[recovery$fishery %in% c(40, 54) & recovery$tag_code %in% c("68640", "67001"),
                                 ][c(1, 3, 10, 14), ]
  rel_imp_mat_mlt_by = release[release$brood_year %in% c(2007, 2008), ]

  expected_value <- data.table(
    by = c(rep(2007, 36), rep(2008, 24)),
    age = c(rep(4, 12), rep(3, 12), rep(2, 12), rep(3, 12), rep(2, 12)),
    month = c(rep(c(5:1, 12:6), 3), rep(c(5:1, 12:6), 2)),
    ocean_abundance = c(0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 16.823527, 17.139292, 17.460984,
                        39.716492, 40.461941, 41.221382, 41.995077, 42.783294, 43.586305, 44.404387, 45.237825, 46.086906, 46.951923, 47.833176, 48.730970,
                        51.628664, 54.698664, 57.951216, 61.397175, 65.048041, 68.915999, 73.013957, 77.355593, 81.955396, 86.828717, 91.991822, 97.461940,
                        3.447044, 3.511743, 3.577656, 3.644806, 3.713216, 3.782910, 3.853913, 3.926248, 3.999941, 4.075017, 4.151502, 4.229422,
                        10.288569, 10.900359, 11.548529, 12.235240, 12.962785, 13.733592, 14.550234, 15.415436, 16.332086, 17.303242, 18.332146, 19.422233)
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
    month = c(5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6, 5:1, 12:6),
    ocean_abundance = c(
      0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 27.45020, 27.96542, 37.72553, 38.43361, 39.15498,
      156.54859, 159.48689, 162.48034, 165.52998, 168.63685, 171.80204, 175.02664, 178.31176, 181.65854, 249.21767, 311.03397, 334.09102,
      1033.05034, 1062.47500, 1082.41688, 1102.73304, 1123.43053, 1144.51650, 1165.99823, 1187.88316, 1210.17886, 1232.89302, 1256.03352, 1279.60835,
      1372.80057, 1454.43154, 1540.91654, 1632.54420, 1729.62033, 1832.46891, 1941.43318, 2056.87680, 2179.18506, 2308.76615, 2446.05253, 2591.50238,
      0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 10.68500, 245.25923, 506.10687, 778.47466,
      2692.30032, 2803.37083, 2855.98804, 2909.59284, 2964.20376, 3019.83968, 3076.51985, 3600.15058, 5162.77677, 7112.31983, 11045.44362, 12785.12748,
      25415.64108, 26685.62157, 27186.49108, 27696.76154, 28216.60940, 28746.21442, 29285.75974, 29942.66280, 31067.83748, 32088.36706, 33011.24165, 33647.53086,
      41710.63295, 44190.87625, 46818.60250, 49602.58148, 52552.10446, 55677.01520, 58987.74281, 62495.33653, 66211.50262, 70148.64345, 74319.89886, 78739.19001
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
