test_that("peak() works", {
  expect_error(peak(sum))
  h <- hyd_snow(5, 2)
  expect_identical(peak(h), 5)
  attr(h, "peak") <- NULL
  expect_equal(peak(h), 5)
})
