
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
data=iris[1:50, 1:4]
head(data)


## ----"load package", message=FALSE---------------------------------------
library(MVN)


## ----"Mardia test", message=FALSE----------------------------------------
result <- mardia.test(data, cov = TRUE, qqplot = FALSE)


## ----"Henze-Zirkler test", message=FALSE---------------------------------
result <- HZ.test(data, cov = TRUE, qqplot = FALSE)


## ----"Royston test", message=FALSE---------------------------------------
result <- royston.test(data, qqplot = FALSE)


## ----"qq-plot", message=FALSE--------------------------------------------
result <- royston.test(data, qqplot = TRUE)


## ----"perspective plot", message=FALSE-----------------------------------
data = iris[1:50, 1:2] 
result = HZ.test(data)
mvn.plot(result, type = "persp", default = TRUE)


## ----"contour plot", message=FALSE, fig.width = 5.8, fig.height = 5.8----
data = iris[1:50, 1:2] 
result = mardia.test(data)
mvn.plot(result, type = "contour", default = TRUE)


## ----"perspective and contour plots", message=FALSE----------------------
mvn.plot(result, type = "both", default = TRUE)


## ----"Session info"------------------------------------------------------
sessionInfo()


