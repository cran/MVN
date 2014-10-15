## ----include=FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
concordance=TRUE
)

## ----echo=FALSE, message=FALSE-------------------------------------------
require(knitr)
opts_chunk$set(cache = TRUE, dev = "pdf")

## ----"display data"------------------------------------------------------
data(iris)
head(iris)

## ----"class and dimension of the data"-----------------------------------
class(iris)
dim(iris)

## ----"subset of the data"------------------------------------------------
Iris=iris[1:50, 1:4]
head(Iris)

## ----"load package", message=FALSE---------------------------------------
library(MVN)

## ----"Mardia test", message=FALSE----------------------------------------
result <- mardiaTest(Iris, cov = TRUE, qqplot = FALSE)
result

## ----"mardia slots", message=FALSE---------------------------------------
getSlots("mardia")

## ----"mardiaTest slots", message=FALSE-----------------------------------
result@p.value.skew
result@p.value.kurt

## ----"Henze-Zirkler test", message=FALSE---------------------------------
result <- hzTest(Iris, cov = TRUE, qqplot = FALSE)
result

## ----"hz slots", message=FALSE-------------------------------------------
getSlots("hz")

## ----"hzTest slots", message=FALSE---------------------------------------
result@HZ
result@p.value

## ----"Royston test", message=FALSE---------------------------------------
result <- roystonTest(Iris, qqplot = FALSE)
result

## ----"royston slots", message=FALSE--------------------------------------
getSlots("hz")

## ----"roystonTest slots", message=FALSE----------------------------------
result@H
result@p.value

## ----"qq-plot", message=FALSE, fig.width = 5, fig.height = 5-------------
result <- roystonTest(Iris, qqplot = TRUE)
result

## ----"perspective plot", message=FALSE, fig.width = 6.5, fig.height = 6.5----
Iris = iris[1:50, 1:2] 
result = hzTest(Iris)
mvnPlot(result, type = "persp", default = TRUE)

## ----"contour plot", message=FALSE, fig.width = 6.5, fig.height = 6.5----
mvnPlot(result, type = "contour", default = TRUE)

## ----"Mahalanobis", message=FALSE----------------------------------------
Iris = iris[1:50, 1:3]
result <- mvOutlier(Iris, qqplot = FALSE, method="quan")
head(result$outlier)
head(result$newData)

## ----"Adjusted Mahalanobis", message=FALSE-------------------------------
result <- mvOutlier(Iris, qqplot = FALSE, method="adj.quan")
head(result$outlier)
head(result$newData)

## ----"qq plot outlier", message=FALSE, fig.width = 5, fig.height = 5-----
result <- mvOutlier(Iris, qqplot = TRUE, method="adj.quan")

## ----"Session info"------------------------------------------------------
sessionInfo()

