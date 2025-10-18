test_that("ci_mean_t_stat works with basic inputs", {
  result <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = 25)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 6)
  expect_named(result, c("group", "mean", "lwr.ci", "upr.ci", "sd", "n"))
})

test_that("ci_mean_t_stat calculates correct values", {
  result <- ci_mean_t_stat(mean_ = 100, sd_ = 15, n = 36)
  
  expect_equal(result$mean, 100)
  expect_equal(result$sd, 15)
  expect_equal(result$n, 36)
  expect_true(result$lwr.ci < result$mean)
  expect_true(result$upr.ci > result$mean)
  expect_true(result$upr.ci - result$lwr.ci > 0)
})

test_that("ci_mean_t_stat handles multiple groups", {
  result <- ci_mean_t_stat(
    mean_ = c(75, 80, 85),
    sd_ = c(10, 12, 8),
    n = c(25, 30, 20),
    group = c("A", "B", "C")
  )
  
  expect_equal(nrow(result), 3)
  expect_equal(as.character(result$group), c("A", "B", "C"))
  expect_equal(result$mean, c(75, 80, 85))
})

test_that("ci_mean_t_stat handles different sample sizes", {
  result <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = c(10, 50, 100))
  
  expect_equal(nrow(result), 3)
  # Larger samples should have narrower CIs
  ci_width <- result$upr.ci - result$lwr.ci
  expect_true(ci_width[3] < ci_width[2])
  expect_true(ci_width[2] < ci_width[1])
})

test_that("ci_mean_t_stat handles different confidence levels", {
  result_95 <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = 25, conf.level = 0.95)
  result_99 <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = 25, conf.level = 0.99)
  
  width_95 <- result_95$upr.ci - result_95$lwr.ci
  width_99 <- result_99$upr.ci - result_99$lwr.ci
  
  # 99% CI should be wider than 95% CI
  expect_true(width_99 > width_95)
})

test_that("ci_mean_t_stat handles vector recycling", {
  result <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = c(25, 30))
  
  expect_equal(nrow(result), 2)
  expect_equal(result$mean, c(75, 75))
})

test_that("ci_mean_t_stat preserves group names", {
  result <- ci_mean_t_stat(mean_ = 75, sd_ = 10, n = 25, group = "Test Group")
  
  expect_s3_class(result$group, "factor")
  expect_equal(as.character(result$group), "Test Group")
})
