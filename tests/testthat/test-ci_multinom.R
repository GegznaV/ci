test_that("ci_multinom works with named vector", {
  x <- c("A" = 20, "B" = 35, "C" = 25)
  result <- ci_multinom(x)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_named(result, c("group", "est", "lwr.ci", "upr.ci", "x", "n"))
})

test_that("ci_multinom calculates correct proportions", {
  x <- c(20, 30, 50)
  result <- ci_multinom(x)
  
  expect_equal(sum(result$est), 1)
  expect_equal(result$n, rep(100, 3))
  expect_equal(result$x, c(20, 30, 50))
})

test_that("ci_multinom handles unnamed vector", {
  result <- ci_multinom(c(20, 35, 54))
  
  expect_equal(nrow(result), 3)
  expect_s3_class(result$group, "factor")
})

test_that("ci_multinom preserves names", {
  x <- c("Small" = 20, "Medium" = 35, "Large" = 54)
  result <- ci_multinom(x)
  
  expect_equal(as.character(result$group), c("Small", "Medium", "Large"))
})

test_that("ci_multinom handles different methods", {
  x <- c(20, 35, 54)
  result_goodman <- ci_multinom(x, method = "goodman")
  result_sg <- ci_multinom(x, method = "sisonglaz")
  
  expect_equal(nrow(result_goodman), nrow(result_sg))
  # Different methods give different CIs
  expect_true(any(result_goodman$lwr.ci != result_sg$lwr.ci))
})

test_that("ci_multinom handles custom column name", {
  x <- c("Dog" = 65, "Cat" = 48, "Bird" = 22)
  result <- ci_multinom(x, gr_colname = "pet_type")
  
  expect_true("pet_type" %in% names(result))
  expect_false("group" %in% names(result))
})

test_that("ci_multinom validates inputs", {
  expect_error(ci_multinom(c(-1, 20, 30)))
  expect_error(ci_multinom(c(1.5, 2.5))) # non-integer
})

test_that("ci_multinom handles confidence levels", {
  x <- c(20, 35, 54)
  result_95 <- ci_multinom(x, conf.level = 0.95)
  result_99 <- ci_multinom(x, conf.level = 0.99)
  
  # 99% CIs should be wider
  width_95 <- mean(result_95$upr.ci - result_95$lwr.ci)
  width_99 <- mean(result_99$upr.ci - result_99$lwr.ci)
  expect_true(width_99 > width_95)
})
