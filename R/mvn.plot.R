mvn.plot <-
function(object, type=c("persp","contour","both"), default=TRUE, ...){
  
  type <- match.arg(type)
  if (class(object) != "MVN") stop("Object must be in \"MVN\" class.")
  p <- ncol(object$data)
  if (p != 2) stop("Plots are available for bivariate normal distributions. Number of variables exceed 2.")
  
  data <- object$data
  data.kde <- kde2d(data[,1], data[,2], n=100)
  
  if (type == "persp"){
    if (default){
      persp(data.kde, theta = 0, phi = 30, border = NA, shade = 0.5,
            box=T, xlab = colnames(data)[1], ylab = colnames(data)[2],
            main = "", zlab = "Density")
    } 
    else {
      persp(data.kde, ...)
    }
  }
  
  if (type == "contour"){
    if (default){
      contour(data.kde, nlevels=20, xlab = colnames(data)[1], ylab = colnames(data)[2])
    }
    else {
      contour(data.kde, ...)
    }
  }
  
  if (type == "both"){
    par(mfrow = c(2,1))
    default = TRUE
    if (default){
      persp(data.kde, theta = 0, phi = 30, border = NA, shade = 0.5,
            box = T, xlab = colnames(data)[1], ylab = colnames(data)[2],
            main = "", zlab = "Density")
      
      contour(data.kde, nlevels = 20, xlab = colnames(data)[1], ylab = colnames(data)[2])
    }
    else {
      persp(data.kde, ...)
      contour(data.kde, ...)
    }
  }
}
