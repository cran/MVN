\name{mvn.plot}
\alias{mvn.plot}
\title{
Perspective and Contour Plots
}
\description{
This function draws perspective and contour plots for a given MVN test.
}
\usage{
\method{mvn}{plot}(object, type = c("persp", "contour", "both"), default = TRUE, ...)
}

\arguments{
  \item{object}{an object of \code{MVN} class}
  \item{type}{if type is selected as \code{persp} it draws a perspective plot, if type is selected as \code{contour} it draws a contour plot, if type is selected as \code{both} it draws both perspective and contour plots}
  \item{default}{when \code{default} is \code{TRUE} it draws plots in default settings}
  \item{\dots}{ an optional argument}
}
\details{
This function draws perspective and contour plots for a given MVN test.
}

\author{
Selcuk Korkmaz\email{selcuk.korkmaz@hacettepe.edu.tr}, Dincer Goksuluk\email{dincer.goksuluk@hacettepe.edu.tr} 
}
\note{
Please be careful that this function draws perspective and contour plots when there are only two variables.
}


\seealso{
\code{\link[MVN]{royston.test}} \code{\link[MVN]{HZ.test}} \code{\link[MVN]{mardia.test}}
}
\examples{
data = iris[1:50, 1:2] # Iris data only for setosa and two variables
result = HZ.test(data)
### Perspective Plot ### 
mvn.plot(result, type = "persp", default = TRUE)
### Contour Plot ### 
mvn.plot(result, type = "contour", default = TRUE)
### Perspective and Contour Plot ### 
mvn.plot(result, type = "both", default = TRUE)
}
