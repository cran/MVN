\name{HZ.test}
\alias{HZ.test}
\title{
Henze-Zirkler's Multivariate Normality Test
}
\description{
This function performs Henze-Zirkler's Multivariate Normality Test. 
}
\usage{
HZ.test(data, cov = TRUE, qqplot = FALSE)
}
\arguments{
  \item{data}{a numeric matrix or data frame}
  \item{cov}{if \code{TRUE} covariance matrix is normalized by \code{n}, if \code{FALSE} it is normalized by \code{n-1}}
  \item{qqplot}{if \code{TRUE} it draws a chi-square Q-Q plot}

}
\details{
The Henze-Zirkler test is based on a non-negative functional distance that
measures the distance between two distribution functions. If the data is multivariate normal, the test statistic HZ is 
approximately lognormally distributed. It proceeds to calculate the mean,
variance and smoothness parameter. Then, mean and variance are 
lognormalized and the p-value is estimated.
}
\value{
\item{HZ}{the value of Henze-Zirkler statistic at significance level \code{0.05}}
\item{p-value}{a p-value for the \code{HZ} test}
}
\references{
Trujillo-Ortiz, A., R. Hernandez-Walls, K. Barba-Rojo and L. Cupul-Magana.(2007). HZmvntest:Henze-Zirkler's Multivariate Normality Test. A MATLAB
   file. URL \url{http://www.mathworks.com/matlabcentral/fileexchange/loadFile.do?objectId=17931}

Henze, N. and Zirkler, B. (1990), A Class of Invariant Consistent Tests for Multivariate Normality. Commun. Statist.-Theor. Meth., 19(10): 35953618.
 
Henze, N. and Wagner, Th. (1997), A New Approach to the BHEP tests for multivariate normality. Journal of Multivariate Analysis, 62:1-23.

Johnson, R. A. and Wichern, D. W. (1992), Applied Multivariate Statistical Analysis. 3rd. ed. New-Jersey:Prentice Hall.
 
Mecklin, C. J. and Mundfrom, D. J. (2003), On Using Asymptotic Critical Values in Testing for Multivariate Normality. \url{http://interstat.statjournals.net/YEAR/2003/articles/0301001.pdf}

}
\author{
Selcuk Korkmaz \email{selcuk.korkmaz@hacettepe.edu.tr}, Dincer Goksuluk \email{dincer.goksuluk@hacettepe.edu.tr}
}

\seealso{
\code{\link[MVN]{royston.test}} \code{\link[MVN]{mardia.test}} \code{\link[MVN]{mvn.plot}}
}
\examples{
data = iris[1:50, 1:4] # Iris data only for setosa and four variables
result = HZ.test(data, qqplot = TRUE)
}