\name{mardia.test}
\alias{mardia.test}
\title{
Mardia's Multivariate Normality Test
}
\description{
This function performs Mardia's Multivariate Normality Test. 
}
\usage{
mardia.test(data, cov = TRUE, plot = TRUE)
}
\arguments{
  \item{data}{a numeric matrix or data frame}
  \item{cov}{if \code{TRUE} covariance matrix normalized by \code{n}, if \code{FALSE} normalized by \code{n-1}}
  \item{plot}{if \code{TRUE} it draws a chi-square Q-Q plot}
}
\details{
Calculates the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical tests. For large sample size the multivariate skewness is asymptotically distributed as a Chi-square random variable; here it is corrected for small sample size. Likewise, the multivariate kurtosis it is distributed as a unit-normal.
}
\value{
\item{g1p}{Mardia's \code{g1p} estimate of multivariate skew}
\item{skew}{Mardia's skew statistic}
\item{p.value.skew }{\code{p-value} of skew statistic}
\item{small.skew}{Mardia's small sample skew statistic}
\item{p.value.small}{\code{p-value} of small sample skew statistic}
\item{g2p}{Mardia's \code{g2p} estimate of multivariate kurtosis}
\item{kurtosis}{Mardia's multivariate kurtosis statistic}
\item{p.value.kurt}{\code{p-value} of kurtosis statistic}
}
\references{
Trujillo-Ortiz, A. and R. Hernandez-Walls. (2003). Mskekur: Mardia's multivariate skewness and kurtosis coefficients and its hypotheses testing. A MATLAB file. URL \url{http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=3519}

Mardia, K. V. (1970), Measures of multivariate skewnees and kurtosis with applications. Biometrika, 57(3):519-530.
 
Mardia, K. V. (1974), Applications of some measures of multivariate skewness and kurtosis for testing normality and robustness studies. Sankhy A, 36:115-128.

Stevens, J. (1992), Applied Multivariate Statistics for Social Sciences. 2nd. ed. New-Jersey:Lawrance Erlbaum Associates Publishers. pp. 247-248.
}
\author{
  Selcuk KORKMAZ \email{selcuk.korkmaz@hacettepe.edu.tr}
}
\seealso{
\code{\link[MVN]{royston.test}} \code{\link[MVN]{HZ.test}}
}
\examples{
data=iris[1:50,1:4] # Iris data only for setosa and four variables
mardia.test(data)
}
