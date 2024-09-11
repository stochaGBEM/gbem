test_that("gbem workflow runs without crashing.", {
  hy <- hyd_snow(5, 2)
  w <- 3
  x <- sxchan::xt_sxc(rep(w, 2))
  ch <- sx_manning(
    x, grad = 0.01, d50 = 45, d84 = 90, roughness = c(0.01, 0.02)
  )
  expect_error(gbem(hy, ch, niter = 100, resistance = "ferguson"))
  g <- gbem(hy, ch, niter = 100, resistance = "manning")
  new_ch <- erode(g)
  expect_s3_class(new_ch, "sf")
  w2 <- sxchan::xt_width(new_ch)
  #expect_lt(w2[1], w2[2])
})

# test_that("gbem0 is the same as gbem on constant hydrograph.", {
#   w <- 3
#   x <- sxchan::xt_sxc(w)
#   ch <- sx_manning(x, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.01)
#   q <- eroding_flow(ch, resistance = "manning")
#   h <- hyd_const(2 * q, duration = 10)
#   g0 <- gbem0_manning(
#     flow = 2 * q, duration = 10, width = w, grad = ch$grad, d50 = ch$d50,
#     d84 = ch$d84, roughness = ch$roughness, rootdepth = ch$rootdepth
#   )
#   g <- gbem(h, ch, niter = 10, resistance = "manning")
#   expect_equal(g0, g)
#   expect_equal(erode(g0), erode(g))
# })

test_that("Two runs of gbem is the same as one.", {
  niter <- 300
  delta_t <- 10 / (niter - 1)
  w <- 10
  x <- sxchan::xt_sxc(w)
  ch <- sx_manning(x, grad = 0.01, d50 = 45, d84 = 90, roughness = 0.02)
  q <- eroding_flow(ch, resistance = "manning")
  df1 <- data.frame(time = c(0, 3, 10), flow = c(q, 4 * q, q))
  df2 <- data.frame(time = c(10, 13, 20), flow = c(q, 3 * q, q))
  df12 <- data.frame(
    time = c(0, 3, 10, c(10, 13, 20) + delta_t),
    flow = c(q, 4 * q, q, q, 3 * q, q)
  )
  h1 <- as_hydrograph(df1, times_from = "time", flows_from = "flow")
  h2 <- as_hydrograph(df2, times_from = "time", flows_from = "flow")
  h12 <- as_hydrograph(df12, times_from = "time", flows_from = "flow")
  plot(h1, xlim = c(0, 20), n = 1000)
  plot(h2, add = TRUE, n = 1000)
  plot(h12, add = TRUE, col = "blue", n = 1000)
  ch1 <- erode(gbem(h1, ch,  niter = niter, resistance = "manning"))
  ch2 <- erode(gbem(h2, ch1, niter = niter, resistance = "manning"))
  ch12 <- erode(gbem(h12, ch, niter = niter * 2, resistance = "manning"))
  expect_equal(ch12, ch2)
})

test_that("Channel doesn't erode beyond stable width.", {
  w <- 3
  x <- sxchan::xt_sxc(w)
  ch <- sx_manning(x, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  ch2 <- eroding_flow(ch, resistance = "manning") |>
    hyd_const() |>
    gbem(ch, niter = 100, resistance = "manning") |>
    erode()
  expect_equal(ch, ch2)
  ch3 <- eroding_flow(ch, resistance = "manning") |>
    (\(f) hyd_const(f + 1))() |>
    gbem(ch, niter = 100, resistance = "manning") |>
    erode()
  expect_gt(sxchan::xt_width(ch3), sxchan::xt_width(ch))
  ch4 <- eroding_flow(ch, resistance = "manning") |>
    (\(f) hyd_snow(f + 1, f - 1))() |>
    gbem(ch, niter = 100, resistance = "manning") |>
    erode()
  expect_gt(sxchan::xt_width(ch4), sxchan::xt_width(ch))
})
