test_that("mvn tidy labels use numeric p-values", {
  set.seed(321)
  data <- matrix(rnorm(60), ncol = 2)

  res_raw <- mvn(
    data,
    mvn_test = "hz",
    univariate_test = "SW",
    multivariate_outlier_method = "none",
    descriptives = FALSE,
    tidy = FALSE
  )
  res_tidy <- mvn(
    data,
    mvn_test = "hz",
    univariate_test = "SW",
    multivariate_outlier_method = "none",
    descriptives = FALSE,
    tidy = TRUE
  )

  mvn_expected <- ifelse(
    res_raw$multivariate_normality$p.value > 0.05,
    "\u2713 Normal",
    "\u2717 Not normal"
  )
  uni_expected <- ifelse(
    res_raw$univariate_normality$p.value > 0.05,
    "\u2713 Normal",
    "\u2717 Not normal"
  )

  expect_identical(res_tidy$multivariate_normality$MVN, mvn_expected)
  expect_identical(res_tidy$univariate_normality$Normality, uni_expected)
})
