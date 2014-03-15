\name{mardia-class}
\Rdversion{1.1}
\docType{class}
\alias{mardia}
\alias{mardia-class}


\title{Class \code{"mardia"}}
\description{
An S4 class for Mardia's Multivariate Normality Test
}

\section{Slots}{
  \describe{
    \item{\code{g1p}:}{stores the Mardia's \code{g1p} estimate of multivariate skew}
    \item{\code{skew}:}{stores the Mardia's skew statistic}
    \item{\code{p.value.skew}:}{stores the \code{p-value} of skew statistic}
    \item{\code{small.skew}:}{stores the Mardia's small sample skew statistic}
    \item{\code{p.value.small}:}{stores the \code{p-value} of small sample skew statistic}
    \item{\code{g2p}:}{stores the Mardia's \code{g2p} estimate of multivariate kurtosis}
    \item{\code{kurtosis}:}{stores the Mardia's multivariate kurtosis statistic}
    \item{\code{p.value.kurt}:}{stores the \code{p-value} of kurtosis statistic}
    \item{\code{dname}:}{stores the data set name}
    \item{\code{dataframe}:}{stores the data set}
  }
}
\author{
Selcuk Korkmaz <\email{selcuk.korkmaz@hacettepe.edu.tr}> and Dincer Goksuluk <\email{dincer.goksuluk@hacettepe.edu.tr}>
}