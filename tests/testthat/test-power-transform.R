test_that("power_transform matches car transformations", {
  set.seed(123)
  df_pos <- data.frame(
    x = exp(rnorm(50)),
    y = exp(rnorm(50))
  )

  res_bc <- suppressWarnings(power_transform(df_pos, family = "bcPower", type = "optimal"))
  pt_bc <- suppressWarnings(car::powerTransform(df_pos, family = "bcPower"))
  expected_bc <- data.frame(
    x = car::bcPower(df_pos$x, pt_bc$lambda["x"], jacobian.adjusted = FALSE),
    y = car::bcPower(df_pos$y, pt_bc$lambda["y"], jacobian.adjusted = FALSE)
  )
  expect_equal(res_bc$lambda, pt_bc$lambda)
  expect_equal(res_bc$data, expected_bc, tolerance = 1e-10)

  df_any <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )
  res_yj <- suppressWarnings(power_transform(df_any, family = "yjPower", type = "rounded"))
  pt_yj <- suppressWarnings(car::powerTransform(df_any, family = "yjPower"))
  expected_yj <- data.frame(
    x = car::yjPower(df_any$x, pt_yj$roundlam["x"], jacobian.adjusted = FALSE),
    y = car::yjPower(df_any$y, pt_yj$roundlam["y"], jacobian.adjusted = FALSE)
  )
  expect_equal(res_yj$lambda, pt_yj$roundlam)
  expect_equal(res_yj$data, expected_yj, tolerance = 1e-10)
})

test_that("power_transform applies bcnPower with gamma", {
  set.seed(456)
  df_mixed <- data.frame(
    x = rnorm(50),
    y = rnorm(50) - 1
  )

  res_bcn <- suppressWarnings(power_transform(df_mixed, family = "bcnPower", type = "optimal"))
  pt_bcn <- suppressWarnings(car::powerTransform(df_mixed, family = "bcnPower"))
  names(pt_bcn$lambda) <- names(df_mixed)
  names(pt_bcn$gamma) <- names(df_mixed)
  expected_bcn <- data.frame(
    x = car::bcnPower(df_mixed$x, pt_bcn$lambda["x"], jacobian.adjusted = FALSE, gamma = pt_bcn$gamma["x"]),
    y = car::bcnPower(df_mixed$y, pt_bcn$lambda["y"], jacobian.adjusted = FALSE, gamma = pt_bcn$gamma["y"])
  )
  expect_equal(res_bcn$lambda, pt_bcn$lambda)
  expect_equal(res_bcn$data, expected_bcn, tolerance = 1e-10)
})
