uniNorm <- function(data, type = c("SW", "CVM", "Lillie", "SF", "AD")){
  if (!is.data.frame(data) && !is.matrix(data) && !is.numeric(data)) stop(warning('Input must be one of classes \"vector\", \"data frame\" or \"matrix\"'))
  type = match.arg(type)
  
  if (type == "AD") TestName = "Anderson-Darling"
  if (type == "CVM") TestName = "Cramer-von Mises"
  if (type == "Lillie") TestName = "Lilliefors (Kolmogorov-Smirnov)"
  if (type == "SW") TestName = "Shapiro-Wilk"
  if (type == "SF") TestName = "Shapiro-Francia"
  
  if (is.data.frame(data) || is.matrix(data)){ 
    varNames = colnames(data)
    dims = dim(data)
    
    if (is.matrix(data)){
      data = data.frame(data)
    }
    
    if (dims[2] > 2){
      if (nrow(data) < 2) stop(warning("Too few number of observations (n < 2)."))
      if (is.null(varNames)) varNames = paste("Column",1:ncol(data),sep="")
      
      result = data.frame(matrix(NA, nrow = ncol(data), ncol=4))
      colnames(result) = c("Variable", "Statistic", "p-value", "Normality")
      result[,"Variable"] = varNames
      
      if (type == "AD") res = apply(data, 2, ad.test)
      if (type == "CVM") res = apply(data, 2, cvm.test)
      if (type == "Lillie") res = apply(data, 2, lillie.test)
      if (type == "SW") res = apply(data, 2, shapiro.test)
      if (type == "SF") res = apply(data, 2, sf.test)
      
      result[, 2:3] = round(ldply(res, .fun = function(x)cbind(x$stat, x$p.value))[,-1],4)
      result$Normality = ifelse(result[,3] > 0.05, "YES", "NO")
    }
    
  }
    
  if (!is.matrix(data) && !is.data.frame(data) && is.numeric(data)){ 
    if (type == "AD") res = ad.test(data)
    if (type == "CVM") res = cvm.test(data)
    if (type == "Lillie") res = lillie.test(data)
    if (type == "SW") res = shapiro.test(data)
    if (type == "SF") res = sf.test(data)
    result = res
  }
  
  {
  if(is.numeric(data)) return(result)
  else {
    cat("\n","  ", TestName, "'s test of Normality", sep="", "\n\n")
    return(result)
  }
  }
}

