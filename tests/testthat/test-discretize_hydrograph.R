test_that("Hydrograph discretization works", {
  hg <- hyd_snow(100, 20, duration = 10)
  df <- discretize_hydrograph(hg, niter = 11)
  expect_equal(dim(df), c(11, 2))
  expect_equal(df$time, 0:10)
})
