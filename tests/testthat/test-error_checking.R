library(dplyr)
library(tidyr)

test_that("check missing", {
  # Should error when no function argument is provided.
  expect_error(ch_reco())

  # Should error when some function arguments are provided, but the required
  # ones are missing.
  expect_error(ch_reco(iter=1))
  # Should not error or warn when the required arguments are provided.
  expect_no_error(ch_reco(release, recovery))
  expect_no_warning(ch_reco(release, recovery))
})

test_that("check typ_err", {
  char = c("WRONG", "TYPE", "OF", "INPUT")
  int = 1
  int_vec = c(int, int)

  # Should error when all provided arguments are of incorrect types.
  expect_error(ch_reco(rel = char,
                         reco = char,
                         size_at_age = int_vec,
                         rel_mort = int_vec,
                         nat_mort = int_vec,
                         sex = int,
                         fisheries = char,
                         bootstrap = char,
                         iter = char))

  # Should error when some provided arguments are of incorrect types and others of correct types.
  expect_error(ch_reco(rel = char,
                       reco = char,
                       size_at_age = int_vec,
                       rel_mort = int_vec,
                       nat_mort = int_vec,
                       sex = int,
                       fisheries = char,
                       bootstrap = char,
                       iter = char))
  # Should error or warning when there's no incorrect type provided.
  expect_no_warning(ch_reco(rel = release,
                            reco = recovery))
  expect_no_error(ch_reco(rel = release,
                            reco = recovery))
})

test_that("check correct number of col", {
  rel_m = release[-2]
  rec_m = mutate(recovery, test = 1)

  # Should error when the required inputs have incorrect number of columns.
  expect_error(ch_reco(rel = rel_m, reco = rec_m))

  # Should error when optional inputs, when provided, have incorrect number of columns.
  expect_error(ch_reco(rel = release,
                       reco = recovery,
                       size_at_age = rel))

  # Should not error or warn when required inputs have the correct number of columns.
  expect_no_error(ch_reco(rel = release,
                          reco = recovery))
  expect_no_warning(ch_reco(rel = release,
                            reco = recovery))
})
#
# test_that("check unique", {
#   expect_warning(2 * 2, 4)
#   expect_no_warning()
# })
#
# test_that("check NaN", {
#   expect_warning(ch_reco(c(NA),c(NA)))
#   expect_no_warning(release)
# })


