library(dplyr)

test_that("check missing", {
  expect_error(cohort_reconstruct())
  expect_error(cohort_reconstruct(iter=1))
  expect_no_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                     last_month = 4L, verbose = FALSE,
                                     bootstrap = FALSE))
})

test_that("check type error", {
  char = c("WRONG", "TYPE", "OF", "INPUT")
  int = 1
  int_vec = c(int, int)

  expect_error(cohort_reconstruct(rel = char,
                         reco = char,
                         last_month = int_vec,
                         birth_month = int_vec,
                         size_at_age = int_vec,
                         rel_mort = int_vec,
                         survival = int_vec,
                         sex = int,
                         fisheries = char,
                         bootstrap = char,
                         iter = char,
                         verbose = FALSE))
  expect_error(cohort_reconstruct(rel = char,
                       reco = char,
                       size_at_age = int_vec,
                       birth_month = int_vec,
                       last_month = int_vec,
                       rel_mort = int_vec,
                       survival = int_vec,
                       sex = int,
                       fisheries = char,
                       bootstrap = char,
                       iter = char,
                       verbose = FALSE,
                       d_mort = char,
                       hr_mort_com = char,
                       hr_mort_rec = char,
                       detail = char,
                       min_harvest_rate = char,
                       level = char,
                       hpd = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                     last_month = 4L, verbose = FALSE,
                                     rel_mort = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  survival = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  sex = int_vec))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  fisheries = TRUE))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  bootstrap = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  detail = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  d_mort = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  hr_mort_com = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  hr_mort_rec = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  min_harvest_rate = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  level = char))
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  hpd = char))

  expect_no_error(cohort_reconstruct(rel = release,
                            reco = recovery,
                            birth_month = 10L,
                            last_month = 12L,
                            iter = 10L,
                            verbose = FALSE))
})

test_that("check correct type but incorrect length", {
  expect_error(cohort_reconstruct(release, recovery, birth_month = 3L,
                                  last_month = 4L, verbose = FALSE,
                                  rel_mort = 1:100))
})
test_that("check correct number of col", {
  rel_m = release[-2]
  rec_m = mutate(recovery, test = 1)

  expect_error(cohort_reconstruct(rel = rel_m,
                                  reco = rec_m,
                                  birth_month = 10L,
                                  last_month = 12L,
                                  verbose = FALSE))

  expect_error(cohort_reconstruct(rel = release,
                       reco = recovery,
                       size_at_age = rel,
                       birth_month = 10L,
                       last_month = 10L,
                       verbose = FALSE))

  expect_no_error(cohort_reconstruct(rel = release,
                                     reco = recovery,
                                     birth_month = 10L,
                                     last_month = 12L,
                                     iter = 10L,
                                     verbose = FALSE))
})


test_that("check unique simple", {
  rel_dup = rbind(release, release)
  rec_dup = rbind(recovery, recovery)

  expect_warning(cohort_reconstruct(rel = rel_dup, reco = rec_dup,
                                    birth_month = 10L,
                                    last_month = 10L,
                                    bootstrap = FALSE,
                                    detail = FALSE,
                                    verbose = FALSE))
})

test_that("check NaN", {
  rel_nan = release
  rel_nan[2,1] = NA

  expect_warning(cohort_reconstruct(rel_nan, recovery,
                                    birth_month = 10L,
                                    last_month = 10L,
                                    bootstrap = FALSE,
                                    detail = FALSE,
                                    verbose = FALSE))
})

test_that("check empty joined table", {
  rel_empty = release[release$tag_code == "0601000304", ]
  rec_empty = recovery[recovery$tag_code == "0601000402", ]

  expect_error(cohort_reconstruct(rel_empty,
                                  rec_empty,
                                  birth_month = 10L,
                                  last_month = 10L,
                                  verbose = FALSE,
                                  iter = 10L))
})

test_that("check incorrect range", {
  for (bm in c(-1, 0, 13, 14)) {
    for (lm in c(-1, 0, 13, 14)) {
      expect_error(cohort_reconstruct(release,
                                      recovery,
                                      birth_month = bm,
                                      last_month = lm,
                                      verbose = FALSE))
    }
  }
})

test_that("check incorrect iter", {
  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = bm,
                                  last_month = lm,
                                  iter = 0L,
                                  verbose = FALSE))
  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = bm,
                                  last_month = lm,
                                  iter = 1L,
                                  verbose = FALSE))
})

test_that("check incorrect arg length", {
  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = 1L : 12L,
                                  last_month = 1L : 12L,
                                  iter = 1L : 10L,
                                  verbose = FALSE))
  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = 1L,
                                  last_month = 1L,
                                  iter = 1L,
                                  bootstrap = c(T, T, F),
                                  d_mort = 1:10,
                                  hr_mort_com = 1:10,
                                  hr_mort_com = 1:10,
                                  detail = c(T, T, F),
                                  min_harvest_rate = 1:10,
                                  level = c(T, T, F),
                                  hpd = c(T, T, T),
                                  verbose = c(T, T, T)))
})

test_that("check incorrect column order", {
  expect_error(cohort_reconstruct(release[, length(colnames(release)) : 1],
                                  recovery,
                                  birth_month = 1L,
                                  last_month = 10L,
                                  verbose = FALSE))
  expect_error(cohort_reconstruct(release,
                                  recovery[, length(colnames(recovery)) : 1],
                                  birth_month = 1L,
                                  last_month = 10L,
                                  verbose = FALSE))
  expect_error(cohrot_reconstruct(release,
                                  recovery,
                                  birth_month = 1L,
                                  last_month = 10L,
                                  size_at_age = length_at_age[, length(colnames(length_at_age)) : 1],
                                  verbose = FALSE))
  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = 1L,
                                  last_month = 10L,
                                  survival = survival_default[, length(colnames(survival_default)) : 1],
                                  verbose = FALSE))

  expect_error(cohort_reconstruct(release,
                                  recovery,
                                  birth_month = 1L,
                                  last_month = 10L,
                                  rel_mort = release_mort[, length(colnames(release_mort)) : 1],
                                  verbose = FALSE))
  expect_error(cohort_reconstruct(release[, length(colnames(release)) : 1],
                                  recovery[, length(colnames(recovery)) : 1],
                                  birth_month = 1L,
                                  last_month = 10L,
                                  size_at_age = length_at_age[, length(colnames(length_at_age)) : 1],
                                  survival = survival_default[, length(colnames(survival_default)) : 1],
                                  rel_mort = release_mort[, length(colnames(release_mort)) : 1],
                                  verbose = FALSE))
})
