\name{perspControl}
\alias{perspControl}
\title{
Control options for perspective plot in \code{mvnPlot}
}
\description{
This function includes options for perspective plot in \code{mvnPlot} function
}
\usage{
perspControl(theta = 1, phi = 30, r = sqrt(3), d = 1, scale = TRUE, expand = 1, 
col = "white", border = NULL, ltheta = -135, lphi = 0, shade = 0.5, box = TRUE, 
axes = TRUE, nticks = 5, ticktype = "simple", xlab = NULL, ylab = NULL, 
zlab = NULL, main = NULL)
}

\arguments{
  \item{theta}{
angles defining the azimuthal direction.
}
  \item{phi}{
angles defining the colatitude direction.
}
  \item{r}{
the distance of the eyepoint from the centre of the plotting box.
}
  \item{d}{
a value which can be used to vary the strength of the perspective transformation. Values of d greater than 1 will lessen the perspective effect and values less and 1 will exaggerate it.
}
  \item{scale}{
If scale is \code{TRUE} the x, y and z coordinates are transformed separately. If scale is \code{FALSE} the coordinates are scaled so that aspect ratios are retained. 
}
  \item{expand}{
an expansion factor applied to the z coordinates. 
}
  \item{col}{
the color(s) of the surface facets. 
}
  \item{border}{
the color of the line drawn around the surface facets.
}
  \item{ltheta, lphi}{
if finite values are specified for \code{ltheta} and \code{lphi}, the surface is shaded as though it was being illuminated from the direction specified by azimuth \code{ltheta} and colatitude \code{lphi}.
}

  \item{shade}{
he shade at a surface facet 
}
  \item{box}{
should the bounding box for the surface be displayed. The default is \code{TRUE}.
}
  \item{axes}{
should ticks and labels be added to the box. The default is \code{TRUE}.
}
  \item{nticks}{
the (approximate) number of tick marks to draw on the axes. Has no effect if ticktype is "simple".
}
  \item{ticktype}{
character: "simple" draws just an arrow parallel to the axis to indicate direction of increase; "detailed" draws normal ticks as per 2D plots.
}
  \item{xlab}{
label for x-axis.
}
  \item{ylab}{
label for y-axis.
}
  \item{zlab}{
label for z-axis.
}
  \item{main}{
main title.
}
}

\details{

This function adapted from \code{graphics::persp}. Please see \code{?graphics::persp} for more details.
}

\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}
