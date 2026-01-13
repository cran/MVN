test_that("mv_outlier adj method respects alpha and seed", {
  set.seed(1)
  data <- rbind(
    matrix(rnorm(100), ncol = 2),
    matrix(rnorm(10, mean = 6), ncol = 2)
  )
  rownames(data) <- seq_len(nrow(data))

  seed <- 99
  alpha <- 0.1

  set.seed(seed)
  covr <- MASS::cov.mcd(data, method = "mcd")
  mah <- stats::mahalanobis(data, center = covr$center, cov = covr$cov)
  crt <- arw_adjustment(data, m0 = covr$center, c0 = covr$cov, alpha = alpha)$cn
  expected <- ifelse(mah > crt, "TRUE", "FALSE")
  names(expected) <- rownames(data)

  res <- mv_outlier(
    data,
    method = "adj",
    alpha = alpha,
    seed = seed,
    qqplot = FALSE
  )

  expected_ordered <- expected[res$outlier$Observation]
  names(expected_ordered) <- NULL
  expect_identical(res$outlier$Outlier, expected_ordered)
})
