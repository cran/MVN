\name{mvnPlot}
\alias{mvnPlot}

\title{
Perspective and Contour Plots
}
\description{
This function creates perspective and contour plots for a bivariate data set.
}
\usage{
mvnPlot(object, type=c("persp","contour"), default = TRUE, 
        plotCtrl = c(perspControl(), contourControl()), ...)
}

\arguments{
  \item{object}{
an object of \code{mardia}, \code{hz} or \code{royston} class
}
  \item{type}{
if type is selected as \code{persp} it creates a perspective plot, if type is selected as \code{contour} it creates a contour plot.
}
  \item{default}{
when \code{default} is \code{TRUE} it creates plots in default settings
}

  \item{plotCtrl}{
options for plot control
}

  \item{\dots}{
an optional argument
}
}

\details{
After set the \code{default=FALSE} option, users can define their own plot settings with changing \code{theta}, \code{phi}, \code{border} and \code{shade}. 	
}	

\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}
\note{
Please be careful that this function creates perspective and contour plots when there are only two variables.
}


\seealso{
\code{\link[MVN]{roystonTest}} \code{\link[MVN]{hzTest}} \code{\link[MVN]{mardiaTest}} \code{\link[MVN]{mvOutlier}} \code{\link[MVN]{uniPlot}} \code{\link[MVN]{uniNorm}}
}
\examples{
setosa = iris[1:50, 1:2] # Iris data only for setosa and two variables
result = hzTest(setosa)
### Perspective Plot ### 
mvnPlot(result, type = "persp", default = TRUE)
### Contour Plot ### 
mvnPlot(result, type = "contour", default = TRUE)
}
