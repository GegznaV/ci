test_that("ci_mean_t works with basic data frame", {
  data(npk, package = "datasets")
  result <- ci_mean_t(npk, yield)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true("mean" %in% names(result))
  expect_true("lwr.ci" %in% names(result))
  expect_true("upr.ci" %in% names(result))
})

test_that("ci_mean_t works with grouped data", {
  data(npk, package = "datasets")
  result <- npk |>
    dplyr::group_by(N) |>
    ci_mean_t(yield)

  expect_equal(nrow(result), 2)
  expect_true("N" %in% names(result))
})

test_that("ci_mean_t works with multiple grouping variables", {
  data(npk, package = "datasets")
  result <- npk |>
    dplyr::group_by(N, P) |>
    ci_mean_t(yield)

  expect_equal(nrow(result), 4)
  expect_true(all(c("N", "P") %in% names(result)))
})

test_that("ci_mean_t calculates correct bounds", {
  data(npk, package = "datasets")
  result <- ci_mean_t(npk, yield)

  expect_true(result$lwr.ci < result$mean)
  expect_true(result$upr.ci > result$mean)
})

test_that("ci_mean_t handles different confidence levels", {
  data(iris, package = "datasets")
  result_95 <- ci_mean_t(iris, Petal.Length, conf.level = 0.95)
  result_99 <- ci_mean_t(iris, Petal.Length, conf.level = 0.99)

  width_95 <- result_95$upr.ci - result_95$lwr.ci
  width_99 <- result_99$upr.ci - result_99$lwr.ci

  expect_true(width_99 > width_95)
})

test_that("ci_mean_t works with iris dataset", {
  data(iris, package = "datasets")
  result <- iris |>
    dplyr::group_by(Species) |>
    ci_mean_t(Petal.Length)

  expect_equal(nrow(result), 3)
  expect_equal(
    sort(as.character(result$Species)),
    sort(c("setosa", "versicolor", "virginica"))
  )
})
