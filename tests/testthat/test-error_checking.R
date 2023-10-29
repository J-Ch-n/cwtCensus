test_that("check missing", {
  expect_error(ch_reco())
  expect_error(ch_reco(iter=1))
  expect_no_error(ch_reco(release, recovery))
})

test_that("check typ_err", {
  expect_warning(ch_reco(c(1),c(1)))
  expect_no_warning(rel)
})

test_that("check correct # of col", {
  expect_warning(2 * 2, 4)
  expect_no_warning()
})

test_that("check unique", {
  expect_warning(2 * 2, 4)
  expect_no_warning()
})

test_that("check NaN", {
  expect_warning(ch_reco(c(NA),c(NA)))
  expect_no_warning(release)
})


