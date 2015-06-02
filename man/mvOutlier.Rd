\name{mvOutlier}
\alias{mvOutlier}


\title{
Multivariate Outlier Detection
}
\description{
This function detects multivariate outliers based on Mahalanobis distance and adjusted Mahalanobis distance.
}
\usage{
mvOutlier(data, qqplot = TRUE, alpha = 0.5, tol = 1e-25, method = c("quan", "adj.quan"), 
          label = TRUE, position = NULL, offset = 0.5)

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

 \item{tol}{
a numeric tolerance value which isused for inversion of the covariance matrix (\code{default = 1e-25}).
}

  \item{method}{
\code{quan} for Mahalanobis distance and \code{adj.quan} for adjusted Mahalanobis distance. 
}


 \item{label}{
an optional term to display outlier labels (i.e. observation number) on the Q-Q plot.
}

 \item{position}{
a position specifier for the text. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.}

 \item{offset}{
when pos is specified, this value gives the offset of the label from the specified coordinate in fractions of a character width.}

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
result = mvOutlier(setosa, qqplot = TRUE, method = "quan", label = TRUE)
result
}

