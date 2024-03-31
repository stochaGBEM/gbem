test_that("Cross section inputs are valid", {
  expect_error(
    cross_section(1, grad = 1, d50 = 1, d84 = 0.5, roughness = 1, rootdepth = 1)
  )
  expect_error(
    cross_section(1, grad = -1, d50 = 0.5, d84 = 1, roughness = 1, rootdepth = 1)
  )
  expect_error(
    cross_section(-1, grad = 1, d50 = 0.5, d84 = 1, roughness = 1, rootdepth = 1)
  )
  expect_error(
    cross_section(1, grad = 1, d50 = -0.5, d84 = 1, roughness = 1, rootdepth = 1)
  )
  expect_error(
    cross_section(1, grad = 1, d50 = 0.5, d84 = -1, roughness = 1, rootdepth = 1)
  )
  expect_error(
    cross_section(1, grad = 1, d50 = 0.5, d84 = 1, roughness = -1, rootdepth = 1)
  )
  expect_error(
    cross_section(1, grad = 1, d50 = 0.5, d84 = 1, roughness = 1, rootdepth = -1)
  )
})
