test_that("Channel width getting and setting works", {
  cs <- cross_section(3, grad = 0.01, d50 = 0.1, d84 = 0.5, roughness = 0.01)
  expect_equal(ch_width(cs), cs$width)
  ch_width(cs) <- 10
  expect_equal(ch_width(cs), 10)
})
