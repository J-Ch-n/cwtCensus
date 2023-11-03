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


test_that("check unique simple", {
  # Create duplicate rows in release and recovery data frames.
  rel_dup = rbind(release, release)
  rec_dup = rbind(recovery, recovery)

  expect_warning(ch_reco(rel = rel_dup, reco = rec_dup), regex=".*")
  # # Use withCallingHandlers to capture warnings
  # withCallingHandlers({
  #   ch_reco(rel = rel_dup, reco = rec_dup)
  # }, warning = function(w) {
  #   # Expect a warning message about duplicates
  #   expect_match(conditionMessage(w), ".+")
  #   #expect_match(conditionMessage(w), "Recovery df has duplicates in column: recovery_id, dropping duplicates.")
  # })
})

test_that("check NaN", {
  # Create a release data frame that contains NA values.
  rel_nan = release
  rel_nan[1,1] = NA

  # Create a recovery data frame that contains NA values.
  rec_nan = recovery
  rec_nan[1,1] = NA

  # Should throw a warning when only release has NA values.
  expect_warning(ch_reco(rel_nan, recovery), regex=".*")

  # Should throw a warning when only recovery has NA values.
  expect_warning(ch_reco(release, rec_nan), regex=".*")

  # Should throw a warning when both release and recovery have NA values.
  expect_warning(ch_reco(rel_nan, rec_nan), regex=".*")

  # Should not throw error.
  expect_no_error(ch_reco(rel_nan, rec_nan))
})


