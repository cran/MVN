\name{mardiaTest}
\alias{mardiaTest}

\title{
Mardia's Multivariate Normality Test
}
\description{
This function performs Mardia's Multivariate Normality Test. 
}
\usage{
mardiaTest(data, cov = TRUE, qqplot = FALSE)
}

\arguments{
  \item{data}{
a numeric matrix or data frame
}
  \item{cov}{
if \code{TRUE} covariance matrix is normalized by \code{n}, if \code{FALSE} it is normalized by \code{n-1}
}
  \item{qqplot}{
if \code{TRUE} it creates a chi-square Q-Q plot
}
}
\details{
This function calculate the Mardia's multivariate skewness and kurtosis coefficients as well as their corresponding statistical significance. It can also calculate corrected version of skewness coefficient for small sample size (n< 20).

For multivariate normality, both p-values of skewness and kurtosis statistics should be greater than \code{0.05}. 

If sample size less than 20 then \code{p.value.small} should be used as significance value of skewness instead of \code{p.value.skew}.
}
\value{
\item{g1p}{Mardia's multivariate skewness statistic}
\item{chi.skew}{Chi-square value of the skewness statistic}
\item{p.value.skew }{\code{p-value} of the skewness statistic}
\item{g2p}{Mardia's multivariate kurtosis statistic}
\item{z.kurtosis}{z value of the kurtosis statistic}
\item{p.value.kurt}{\code{p-value} of kurtosis statistic}
\item{chi.small.skew}{Chi-square value of the small sample skewness statistic}
\item{p.value.small}{\code{p-value} of small sample skew statistic}
}
\references{
Trujillo-Ortiz, A. and R. Hernandez-Walls. (2003). Mskekur: Mardia's multivariate skewness and kurtosis coefficients and its hypotheses testing. A MATLAB file. URL \url{http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=3519}

Mardia, K. V. (1970), Measures of multivariate skewnees and kurtosis with applications. Biometrika, 57(3):519-530.
 
Mardia, K. V. (1974), Applications of some measures of multivariate skewness and kurtosis for testing normality and robustness studies. Sankhy A, 36:115-128.

Stevens, J. (1992), Applied Multivariate Statistics for Social Sciences. 2nd. ed. New-Jersey:Lawrance Erlbaum Associates Publishers. pp. 247-248.

}
\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}

\seealso{
\code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mvnPlot}} \code{\link[MVN]{mvOutlier}} \code{\link[MVN]{uniPlot}} \code{\link[MVN]{uniNorm}}
}
\examples{
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
result = mardiaTest(setosa, qqplot = TRUE)
result
}
