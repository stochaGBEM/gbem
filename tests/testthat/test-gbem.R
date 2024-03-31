test_that("gbem runs without crashing.", {
  hy <- hyd_snow(5, 2)
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
  g <- gbem2(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01,
                      rootdepth = 0.35)
  g <- gbem2(hy, cross_section = cs)
  new_cs <- erode(g)
  expect_s3_class(new_cs, "cross_section")
})
