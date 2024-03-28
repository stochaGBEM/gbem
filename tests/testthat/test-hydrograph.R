test_that("Hydrograph construction works", {
  df <- data.frame(
    time = c(0, 1, 4, 5, 10, 12),
    flow = c(2, 5, 3, 6, 2, 2)
  )
  hg <- as_hydrograph(df, times_from = time, flows_from = flow)
  plot(hg)
  expect_true(is_hydrograph(hg))
  hg2 <- hydrograph(
    2 ~ 0,
    5 ~ 1,
    3 ~ 4,
    6 ~ 5,
    2 ~ 10,
    2 ~ 12
  )
  expect_equal(hg, hg2)
  hg3 <- hydrograph(
    2 ~ 0 / 12,
    5 ~ 1 / 12,
    3 ~ 4 / 12,
    6 ~ 5 / 12,
    2 ~ 10 / 12,
    2 ~ 12 / 12,
    unit = 12
  )
  expect_equal(hg2, hg3)
  expect_error(hydrograph(4 ~ 0))
  expect_error(hydrograph(4 ~ 0, 4 ~ 0))
  expect_error(hydrograph(4 ~ -Inf, 5 ~ 0))
  expect_error(hydrograph(4 ~ 0, 5 ~ Inf))
})

test_that("Duration of hydrograph works.", {
  hg <- hydrograph(1 ~ 0, 1 ~ 10)
  expect_equal(range(hg), c(0, 10))
})

test_that("Rain hydrographs work as expected.", {
  pk <- 5
  base <- 2
  hg <- hydrograph_rain(pk, base, duration = 3)
  expect_equal(hg(1), pk)
  expect_equal(hg(c(0, 3)), c(base, base))
  t <- c(1 - 1e-3, 1 + 1e-3)
  expect_true(all(hg(t) < pk))
})

test_that("Snow hydrographs work as expected.", {
  pk <- 5
  base <- 2
  hg <- hydrograph_snow(pk, base, duration = 6)
  expect_equal(hg(2:3), c(pk, pk))
  expect_equal(hg(c(0, 6)), c(base, base))
  t <- c(2 - 1e-3, 3 + 1e-3)
  expect_true(all(hg(t) < pk))
})
