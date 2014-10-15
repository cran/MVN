\name{mvOutlier}
\alias{mvOutlier}

\title{
Multivariate Outlier Detection
}
\description{
This function detects multivariate outliers based on Mahalanobis distance and adjusted Mahalanobis distance.
}
\usage{
mvOutlier(data, qqplot = TRUE, method = c("quan", "adj.quan"))
}

\arguments{
  \item{data}{
a numeric matrix or data frame
}
  \item{qqplot}{
if \code{TRUE} it creates a chi-square Q-Q plot
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
Selcuk Korkmaz <\email{selcuk.korkmaz@hacettepe.edu.tr}> and Dincer Goksuluk <\email{dincer.goksuluk@hacettepe.edu.tr}>
}


\seealso{
\code{\link[MVN]{mardiaTest}} \code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mvnPlot}}
}


\examples{
Iris = iris[1:50, 1:3] # Iris data only for setosa and three variables
result = mvOutlier(Iris, qqplot = TRUE, method = "quan")
result
}

