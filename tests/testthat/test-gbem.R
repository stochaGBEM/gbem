test_that("gbem runs without crashing.", {
  hy <- hyd_snow(5, 2)
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
  g <- gbem(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01,
                      rootdepth = 0.35)
  g <- gbem(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
})

test_that("gbem0 is the same as gbem on constant hydrograph.", {
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
  h <- hyd_const(50, duration = 10)
  g0 <- gbem0(50, 10, cross_section = cs)
  g <- gbem(h, cross_section = cs)
  g$event <- NULL
  expect_equal(g0, g)
})

test_that("Two runs of gbem is the same as one.", {
  niter <- 300
  eps <- 10 / (niter - 1)
  cs <- cross_section(1, grad = 0.1, d50 = 0.5, d84 = 1, roughness = 0.02)
  df1 <- data.frame(time = c(0, 3, 10), flow = c(2, 10, 2))
  df2 <- data.frame(time = c(10, 13, 20) + eps, flow = c(2, 5, 2))
  df12 <- data.frame(time = c(0, 3, 10, 10 + eps, 13 + eps, 20 + eps),
                     flow = c(2, 10, 2, 2, 5, 2))
  h1 <- as_hydrograph(df1, times_from = "time", flows_from = "flow")
  h2 <- as_hydrograph(df2, times_from = "time", flows_from = "flow")
  h12 <- as_hydrograph(df12, times_from = "time", flows_from = "flow")
  plot(h1, xlim = c(0, 20), n = 1000)
  plot(h2, add = TRUE, n = 1000)
  plot(h12, add = TRUE, col = "blue", n = 1000)
  cs1 <- erode(gbem(h1, cs, niter = 1000))
  cs2 <- erode(gbem(h2, cs1, niter = 1000))
  cs12 <- erode(gbem(h12, cs, niter = 2000))
  # Should be the same, except for the duplicated flow at t = 10.
  expect_equal(cs12, cs2)
})
