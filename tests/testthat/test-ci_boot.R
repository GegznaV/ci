test_that("ci_boot works with basic inputs", {
  data(iris, package = "datasets")
  set.seed(123)

  result <- ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "perc")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true("median" %in% names(result))
  expect_true(all(c("lwr.ci", "upr.ci") %in% names(result)))
})

test_that("ci_boot works with grouped data", {
  data(iris, package = "datasets")
  set.seed(456)

  result <- iris |>
    dplyr::group_by(Species) |>
    ci_boot(Petal.Length, FUN = median, R = 100, bci.method = "perc")

  expect_equal(nrow(result), 3)
  expect_true("Species" %in% names(result))
})

test_that("ci_boot works with different functions", {
  data(iris, package = "datasets")
  set.seed(789)

  result_median <- ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "perc")
  result_sd <- ci_boot(iris, Petal.Length, FUN = sd, R = 100, bci.method = "perc")

  expect_true("median" %in% names(result_median))
  expect_true("sd" %in% names(result_sd))
})

test_that("ci_boot works with two variables (correlation)", {
  data(iris, package = "datasets")
  set.seed(101)

  result <- ci_boot(
    iris,
    Petal.Length, Petal.Width,
    FUN = cor, method = "pearson",
    R = 100, bci.method = "perc"
  )

  expect_s3_class(result, "tbl_df")
  expect_true("cor" %in% names(result))
})

test_that("ci_boot handles BCa method", {
  data(iris, package = "datasets")
  set.seed(111)

  # May produce warnings with small R, but should still work
  result <- suppressWarnings(
    ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "bca")
  )

  expect_s3_class(result, "tbl_df")
})

test_that("ci_boot handles different R values", {
  data(iris, package = "datasets")
  set.seed(222)

  result_100 <- ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "perc")
  result_500 <- ci_boot(iris, Petal.Length, FUN = median, R = 500, bci.method = "perc")

  # Both should work
  expect_s3_class(result_100, "tbl_df")
  expect_s3_class(result_500, "tbl_df")
})

test_that("ci_boot handles NA values with na.rm", {
  data(iris, package = "datasets")
  set.seed(333)

  # Add some NAs
  iris_na <- iris
  iris_na$Petal.Length[1:5] <- NA

  result <- ci_boot(
    iris_na,
    Petal.Length,
    FUN = median, na.rm = TRUE,
    R = 100, bci.method = "perc"
  )

  expect_s3_class(result, "tbl_df")
  expect_false(is.na(result$median))
})

test_that("ci_boot preserves reproducibility with set.seed", {
  data(iris, package = "datasets")

  set.seed(444)
  result1 <- ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "perc")

  set.seed(444)
  result2 <- ci_boot(iris, Petal.Length, FUN = median, R = 100, bci.method = "perc")

  expect_equal(result1$median, result2$median)
  expect_equal(result1$lwr.ci, result2$lwr.ci)
  expect_equal(result1$upr.ci, result2$upr.ci)
})
