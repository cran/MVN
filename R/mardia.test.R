mardia.test <-
function (data, cov = TRUE, qqplot = FALSE) 
{
  dname <- deparse(substitute(data))
  data <- as.matrix(data)
  n <- dim(data)[1]
  p <- dim(data)[2]
  data.org <- data
  data <- scale(data, scale = FALSE)
  if (cov) {
    S <- ((n - 1)/n) * cov(data)
  }
  else {
    S <- cov(data)
  }
  D <- data %*% solve(S) %*% t(data)
  g1p <- sum(D^3)/n^2
  g2p <- sum(diag((D^2)))/n
  df <- p * (p + 1) * (p + 2)/6
  k <- (p + 1) * (n + 1) * (n + 3)/(n * ((n + 1) * (p + 1) - 
                                           6))
  small.skew <- n * k * g1p/6
  skew <- n * g1p/6
  kurt <- (g2p - p * (p + 2)) * sqrt(n/(8 * p * (p + 2)))
  p.skew <- pchisq(skew, df, lower.tail = FALSE)
  p.small <- pchisq(small.skew, df, lower.tail = FALSE)
  p.kurt <- 2 * (1 - pnorm(abs(kurt)))
  if (qqplot) {
    d <- diag(D)
    r <- rank(d)
    chi2q <- qchisq((r - 0.5)/n, p)
    plot(chi2q, d, pch = 19, main = "Chi-Square Q-Q Plot", 
         xlab = "Chi-Square Quantile", ylab = "Squared Mahalanobis Distance")
    abline(0, 1, lwd = 2, col = "black")
  }
  h.test <- list(statistic = c(g1p = g1p, skew = skew, p.value.skew = p.skew, 
                               small.skew = small.skew, p.value.small = p.small, g2p = g2p, 
                               kurtosis = kurt, p.value.kurt = p.kurt), method = "Mardia's Multivariate Normality Test", 
                 data.name = dname)
  class(h.test) <- "htest"
  results <- list(h.test = h.test, data = data.org)
  print(h.test)
  class(results) <- "MVN"
  invisible(results)
}
