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
    \item{\code{g1p}:}{stores the Mardia's multivariate skewness statistic}
    \item{\code{chi.skew}:}{stores the chi-square value of the skewness statistic}
    \item{\code{p.value.small}:}{stores the \code{p-value} of small sample skew statistic}
    \item{\code{g2p}:}{stores the Mardia's multivariate kurtosis statistic}
    \item{\code{z.kurtosis}:}{stores the z value of thekurtosis statistic}
    \item{\code{p.value.kurt}:}{stores the \code{p-value} of kurtosis statistic}
    \item{\code{p.value.skew}:}{stores the \code{p-value} of skewness statistic}
    \item{\code{chi.small.skew}:}{stores the chi-square value of the small sample skewness statistic}
    \item{\code{dname}:}{stores the data set name}
    \item{\code{dataframe}:}{stores the data set}
  }
}
\author{
Selcuk Korkmaz, Dincer Goksuluk, Gokmen Zararsiz
}