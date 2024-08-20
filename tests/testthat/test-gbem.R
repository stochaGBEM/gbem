test_that("gbem workflow runs without crashing.", {
  hy <- hyd_snow(5, 2)
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.01)
  g <- gbem(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.01,
                      rootdepth = 0.35)
  g <- gbem(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
})

test_that("gbem0 is the same as gbem on constant hydrograph.", {
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.01)
  q <- eroding_flow(cs)
  h <- hyd_const(2 * q, duration = 10)
  g0 <- gbem0(2 * q, 10, cross_section = cs)
  g <- gbem(h, cross_section = cs)
  g$event <- NULL
  expect_equal(g0[c("dw_pred", "dw_const")], g[c("dw_pred", "dw_const")])
  expect_equal(erode(g0), erode(g))
})

test_that("Two runs of gbem is the same as one.", {
  niter <- 300
  delta_t <- 10 / (niter - 1)
  cs <- cross_section(10, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.02)
  q <- eroding_flow(cs)
  df1 <- data.frame(time = c(0, 3, 10), flow = c(q, 4 * q, q))
  df2 <- data.frame(time = c(10, 13, 20), flow = c(q, 3 * q, q))
  df12 <- data.frame(time = c(0, 3, 10, c(10, 13, 20) + delta_t),
                     flow = c(q, 4 * q, q, q, 3 * q, q))
  h1 <- as_hydrograph(df1, times_from = "time", flows_from = "flow")
  h2 <- as_hydrograph(df2, times_from = "time", flows_from = "flow")
  h12 <- as_hydrograph(df12, times_from = "time", flows_from = "flow")
  plot(h1, xlim = c(0, 20), n = 1000)
  plot(h2, add = TRUE, n = 1000)
  plot(h12, add = TRUE, col = "blue", n = 1000)
  cs1 <- erode(gbem(h1, cs, niter = niter))
  cs2 <- erode(gbem(h2, cs1, niter = niter))
  cs12 <- erode(gbem(h12, cs, niter = niter * 2))
  expect_equal(cs12, cs2)
})

test_that("Channel doesn't erode beyond stable width.", {
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  cs2 <- eroding_flow(cs) |>
    hyd_const() |>
    gbem(cross_section = cs) |>
    erode()
  expect_equal(cs, cs2)
  cs3 <- eroding_flow(cs) |>
    (\(f) hyd_const(f + 1))() |>
    gbem(cross_section = cs) |>
    erode()
  expect_gt(ch_width(cs3), ch_width(cs))
  cs4 <- eroding_flow(cs) |>
    (\(f) hyd_snow(f + 1, f - 1))() |>
    gbem(cross_section = cs) |>
    erode()
  expect_gt(ch_width(cs4), ch_width(cs))
})
