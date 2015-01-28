\name{uniPlot}
\alias{uniPlot}

\title{
Univariate Plots
}
\description{
This function creates univariate plots, including Q-Q plot, histogram, box-plot and scatterplot matrices.
}
\usage{
uniPlot(data, type = c("qqplot", "histogram", "box", "scatter"), mfrow = NULL, ...)
}
\arguments{
  \item{data}{
a vector, data frame or matrix
}
  \item{type}{
select one of the univariate plots: \code{qqplot}: Q-Q plot, \code{histogram}: histogram with a normal curve, \code{box}: box-plot and \code{scatter}: scatterplot matrice
}
  \item{mfrow}{
multi-paneled plotting window
}
  \item{\dots}{
optional arguments
}
}
\details{
Box-Plots are based on standardized values, variables are centered and scaled before plotting. \code{qqplot} is default.
}

\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}

\seealso{
\code{\link[MVN]{mardiaTest}} \code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mvnPlot}} \code{\link[MVN]{mvOutlier}} \code{\link[MVN]{uniNorm}}
}
\examples{
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
uniPlot(setosa, type = "qqplot")
}
