test_that("Eroding flows and stable channel calcs line up.", {
  w <- 3
  x <- sxchan::xt_sxc(w)
  ch <- sx_manning(x, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  flow <- eroding_flow(ch, resistance = "manning")
  expect_gt(flow, 0)
  expect_equal(ch, min_stable_channel(ch, flow, resistance = "manning"))
  ch1 <- sxchan::xt_widen_by(ch, by = -w) # zero width
  expect_equal(min_stable_channel(ch, 0, resistance = "manning"), ch1)
  expect_equal(
    min_stable_channel(ch, flow, resistance = "manning"),
    min_stable_channel(
      sxchan::xt_widen_times(ch, 2), flow, resistance = "manning"
    )
  )
  expect_lt(
    sxchan::xt_width(min_stable_channel(ch, flow, resistance = "manning")),
    sxchan::xt_width(min_stable_channel(ch, flow + 1, resistance = "manning"))
  )
  expect_gt(
    sxchan::xt_width(min_stable_channel(ch, flow, resistance = "manning")),
    sxchan::xt_width(min_stable_channel(ch, flow - 1, resistance = "manning"))
  )
})

test_that("min_stable_channel anticipates weird inputs", {
  w <- 3
  x <- sxchan::xt_sxc(w)
  ch <- sx_manning(x, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  expect_error(min_stable_channel(cs, flow = -1, resistance = "manning"))
  expect_equal(
    sxchan::xt_width(min_stable_channel(ch, NA, resistance = "manning")),
    NA_real_
  )
})
