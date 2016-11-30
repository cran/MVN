\name{contourControl}
\alias{contourControl}
\title{
Control options for contour plot in \code{mvnPlot}
}
\description{
This function includes options for contour plot in \code{mvnPlot} function
}
\usage{
contourControl(nlevels = 20, labels = NULL, xlab = NULL, ylab = NULL, labcex = 0.6, 
drawlabels = TRUE, method = c("simple", "edge", "flattest"), axes = TRUE, 
frame.plot = TRUE, col = par("fg"), lty = par("lty"), lwd = par("lwd"))
}
\arguments{
  \item{nlevels}{
 number of contour levels.
}
  \item{labels}{
a vector giving the labels for the contour lines.
}
  \item{xlab}{
label for x-axis.
}
  \item{ylab}{
label for y-axis.
}
  \item{labcex}{
cex for contour labelling.
}
  \item{drawlabels}{
Contours are labelled if \code{TRUE}.
}
  \item{method}{
character string specifying where the labels will be located. Possible values are "simple", "edge" and "flattest" (the default).
}
  \item{axes}{
logical indicating whether axes should be drawn.
}
  \item{frame.plot}{
logical indicating whether a box should be drawn.

}
  \item{col}{
color for the lines drawn.
}
  \item{lty}{
line type for the lines drawn.
}
  \item{lwd}{
line width for the lines drawn.
}
}

\details{

This function adapted from \code{graphics::contour}. Please see \code{?graphics::contour} for more details.
}

\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}
