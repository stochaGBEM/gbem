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
  expect_equal(g0$dw_pred, g$dw_pred)
})
