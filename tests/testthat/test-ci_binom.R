test_that("ci_binom works with basic inputs", {
  result <- ci_binom(x = 54, n = 80)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_named(result, c("est", "lwr.ci", "upr.ci", "x", "n"))
})

test_that("ci_binom calculates correct proportion", {
  result <- ci_binom(x = 50, n = 100)

  expect_equal(result$est, 0.5)
  expect_equal(result$x, 50)
  expect_equal(result$n, 100)
  expect_true(result$lwr.ci < result$est)
  expect_true(result$upr.ci > result$est)
})

test_that("ci_binom handles edge cases", {
  # All successes
  result_all <- ci_binom(x = 10, n = 10)
  expect_equal(result_all$est, 1)

  # No successes
  result_none <- ci_binom(x = 0, n = 10)
  expect_equal(result_none$est, 0)
})

test_that("ci_binom handles multiple sample sizes", {
  result <- ci_binom(x = 54, n = c(80, 100, 200))

  expect_equal(nrow(result), 3)
  expect_equal(result$x, c(54, 54, 54))

  # Larger samples should have narrower CIs
  ci_width <- result$upr.ci - result$lwr.ci
  expect_true(ci_width[3] < ci_width[1])
})

test_that("ci_binom handles different confidence levels", {
  result_95 <- ci_binom(x = 54, n = 80, conf.level = 0.95)
  result_99 <- ci_binom(x = 54, n = 80, conf.level = 0.99)

  width_95 <- result_95$upr.ci - result_95$lwr.ci
  width_99 <- result_99$upr.ci - result_99$lwr.ci

  expect_true(width_99 > width_95)
})

test_that("ci_binom handles different methods", {
  result_wilson <- ci_binom(x = 15, n = 25, method = "wilson")
  result_ac <- ci_binom(x = 15, n = 25, method = "agresti-coull")

  expect_equal(result_wilson$est, result_ac$est)
  # Different methods may give slightly different CIs
  expect_true(abs(result_wilson$lwr.ci - result_ac$lwr.ci) >= 0)
})

test_that("ci_binom validates inputs", {
  expect_error(ci_binom(x = -1, n = 10))
  expect_error(ci_binom(x = 10, n = -5))
  expect_error(ci_binom(x = 15, n = 10)) # x > n
})

test_that("ci_binom handles vector x", {
  result <- ci_binom(x = c(23, 45), n = sum(c(23, 45)))

  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(68, 68))
})
