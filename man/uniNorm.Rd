\name{uniNorm}
\alias{uniNorm}

\title{
Univariate Normality Tests
}
\description{
This function performs univariate normality tests, including Sahapiro-Wilk, Cramer-von Mises, Lilliefors (Kolmogorov-Smirnov), Shapiro-Francia and Anderson-Darling.
}
\usage{
uniNorm(data, type = c("SW", "CVM", "Lillie", "SF", "AD"), desc = TRUE)
}

\arguments{
  \item{data}{
a vector, data frame or matrix
}
  \item{type}{
select one of the univariate normality tests: \code{SW}: Shapiro-Wilk, \code{CVM}:  Cramer-von Mises, \code{Lillie}: Lilliefors (Kolmogorov-Smirnov), \code{SF}: Shapiro-Francia, \code{AD}: Anderson-Darling
}
  \item{desc}{if \code{TRUE}, it displays descriptive statistics including mean, standard deviation, median, minimum, maximum, 25th and 75th percentiles, skewness and kurtosis.}

}

\details{
\code{SW} is default.
}



\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}


\seealso{
\code{\link[MVN]{mardiaTest}} \code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mvnPlot}} \code{\link[MVN]{mvOutlier}} \code{\link[MVN]{uniPlot}}
}
\examples{
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
uniNorm(setosa, type = "SW", desc = TRUE)
}

