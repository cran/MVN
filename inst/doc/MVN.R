
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


## ----"Henze-Zirkler test", message=FALSE---------------------------------
result <- hzTest(Iris, cov = TRUE, qqplot = FALSE)
result


## ----"Royston test", message=FALSE---------------------------------------
result <- roystonTest(Iris, qqplot = FALSE)
result


## ----"qq-plot", message=FALSE, fig.width = 5.8, fig.height = 5.8---------
result <- roystonTest(Iris, qqplot = TRUE)
result


## ----"perspective plot", message=FALSE, fig.width = 5.8, fig.height = 5.8----
Iris = iris[1:50, 1:2] 
result = hzTest(Iris)
mvnPlot(result, type = "persp", default = TRUE)


## ----"contour plot", message=FALSE, fig.width = 5.8, fig.height = 5.8----
mvnPlot(result, type = "contour", default = TRUE)


## ----"Session info"------------------------------------------------------
sessionInfo()


