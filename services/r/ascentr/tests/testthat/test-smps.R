# calc_dlogDp() ------------------------------------------------------------

test_that("calc_dlogDp returns same length as midpoints", {
  expect_length(calc_dlogDp(c(10, 100, 1000)), 3)
  expect_length(calc_dlogDp(10^seq(1, 3, by = 0.5)), 5)
})

test_that("calc_dlogDp returns all positive values", {
  result <- calc_dlogDp(c(10, 100, 1000))
  expect_gt(min(result), 0)
})

test_that("calc_dlogDp returns uniform widths for evenly log-spaced midpoints (1-decade)", {
  # 3 bins at 10, 100, 1000 nm -> each bin spans exactly 1 log10 decade
  result <- calc_dlogDp(c(10, 100, 1000))
  expect_equal(result, c(1, 1, 1))
})

test_that("calc_dlogDp returns uniform widths for evenly log-spaced midpoints (half-decade)", {
  result <- calc_dlogDp(10^seq(1, 3, by = 0.5))
  expect_equal(result, rep(0.5, 5))
})

test_that("calc_dlogDp bin widths sum to the full log10 range plus one bin width", {
  mids <- c(10, 100, 1000)
  result <- calc_dlogDp(mids)
  # Edges extend half a bin beyond the first and last midpoint on each side,
  # so the total span = log10(last/first) + avg_diff
  avg_diff <- mean(diff(log10(mids)))
  expected_total <- log10(1000 / 10) + avg_diff
  expect_equal(sum(result), expected_total)
})


# calc_W() -----------------------------------------------------------------

test_that("calc_W returns a vector of length nrow(dWdlogDp)", {
  dWdlogDp <- matrix(1, nrow = 3, ncol = 4)
  dlogDp   <- rep(1, 4)
  expect_length(calc_W(dWdlogDp, dlogDp), 3)
})

test_that("calc_W integrates uniform distribution over unit bins", {
  # Each bin width = 1; uniform concentration = 1 -> total = n_bins
  dWdlogDp <- matrix(1, nrow = 1, ncol = 3)
  dlogDp   <- c(1, 1, 1)
  expect_equal(calc_W(dWdlogDp, dlogDp), 3)
})

test_that("calc_W weights by bin width correctly", {
  # Concentration = 1 everywhere; bins of widths 0.5, 1.0, 0.5 -> total = 2
  dWdlogDp <- matrix(1, nrow = 1, ncol = 3)
  dlogDp   <- c(0.5, 1.0, 0.5)
  expect_equal(calc_W(dWdlogDp, dlogDp), 2)
})

test_that("calc_W handles multiple rows independently", {
  dWdlogDp <- matrix(c(1, 2, 1,
                        4, 0, 4), nrow = 2, byrow = TRUE)
  dlogDp <- c(1, 1, 1)
  expect_equal(calc_W(dWdlogDp, dlogDp), c(4, 8))
})

test_that("calc_W with impulse distribution returns concentration times bin width", {
  # All concentration in the middle bin (width 1.0)
  dWdlogDp <- matrix(c(0, 100, 0), nrow = 1)
  dlogDp   <- c(0.5, 1.0, 0.5)
  expect_equal(calc_W(dWdlogDp, dlogDp), 100)
})

test_that("calc_W returns zero for all-zero concentration", {
  dWdlogDp <- matrix(0, nrow = 2, ncol = 5)
  dlogDp   <- rep(0.5, 5)
  expect_equal(calc_W(dWdlogDp, dlogDp), c(0, 0))
})
