test_that("Eroding flows and stable channel calcs line up.", {
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  flow <- eroding_flow(cs)
  expect_gt(flow, 0)
  expect_equal(cs, min_stable_channel(cs, flow))
  cs0 <- cs
  ch_width(cs0) <- 0
  expect_equal(min_stable_channel(cs, 0), cs0)
  expect_equal(min_stable_channel(cs, flow), min_stable_channel(cs0, flow))
  expect_lt(
    ch_width(min_stable_channel(cs, flow)),
    ch_width(min_stable_channel(cs, flow + 1))
  )
  expect_gt(
    ch_width(min_stable_channel(cs, flow)),
    ch_width(min_stable_channel(cs, flow - 1))
  )
})

test_that("min_stable_channel anticipates weird inputs", {
  cs <- cross_section(3, grad = 0.01, d50 = 45, d84 = 80, roughness = 0.01)
  expect_error(min_stable_channel(cs, -1))
  expect_equal(ch_width(min_stable_channel(cs, NA)), NA_real_)
})
