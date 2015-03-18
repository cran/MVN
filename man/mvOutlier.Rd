\name{mvOutlier}
\alias{mvOutlier}

\title{
Multivariate Outlier Detection
}
\description{
This function detects multivariate outliers based on Mahalanobis distance and adjusted Mahalanobis distance.
}
\usage{
mvOutlier(data, qqplot = TRUE, alpha = 0.5, method = c("quan", "adj.quan"))
}

\arguments{
  \item{data}{
a numeric matrix or data frame
}
  \item{qqplot}{
if \code{TRUE} it creates a chi-square Q-Q plot
}

 \item{alpha}{
a numeric parameter controlling the size of the subsets over which the determinant is minimized. Allowed values for the alpha are between 0.5 and 1 and the default is 0.5. 
}
  \item{method}{
\code{quan} for Mahalanobis distance and \code{adj.quan} for adjusted Mahalanobis distance. 
}
}

\value{
\item{outlier}{an outlier set}
\item{newdData}{new data set without possible outliers}
}

\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}


\seealso{
\code{\link[MVN]{mardiaTest}} \code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mvnPlot}} \code{\link[MVN]{uniPlot}} \code{\link[MVN]{uniNorm}}
}


\examples{
setosa = iris[1:50, 1:3] # Iris data only for setosa and three variables
result = mvOutlier(setosa, qqplot = TRUE, method = "quan")
result
}

