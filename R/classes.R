setClass("mardia",
    representation(g1p = "numeric", skew="numeric", p.value.skew="numeric", small.skew="numeric",
        p.value.small="numeric", g2p="numeric", kurtosis="numeric", p.value.kurt="numeric", dname="character", dataframe="data.frame"))


setGeneric("mardia", function(object) standardGeneric("mardia"))


setMethod("show",
signature = "mardia",
definition = function(object) {
    cat("   Mardia's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------", "\n", sep = " ")
    cat("   data :", object@dname, "\n\n", sep = " ")
    cat("   g1p           :", object@g1p, "\n", sep = " ")
    cat("   skew          :", object@skew, "\n", sep = " ")
    cat("   p.value.skew  :", object@p.value.skew, "\n\n", sep = " ")
    cat("   g2p           :", object@g2p, "\n", sep = " ")
    cat("   kurtosis      :", object@kurtosis, "\n", sep = " ")
    cat("   p.value.kurt  :", object@p.value.kurt, "\n\n", sep = " ")
    cat("   small.skew    :", object@small.skew, "\n", sep = " ")
    cat("   p.value.small :", object@p.value.small, "\n\n", sep = " ")
    cat(if((object@p.value.skew > 0.05) & (object@p.value.kurt > 0.05)){"   Result        : Data is multivariate normal."}
        else {"   Result        : Data is not multivariate normal."},"\n")
    cat("---------------------------------------", "\n\n", sep = " ")
    
        invisible(NULL)
})


setClass("hz",
representation(HZ = "numeric", p.value="numeric", dname="character", dataframe="data.frame"))


setGeneric("hz", function(object) standardGeneric("hz"))


setMethod("show",
signature = "hz",
definition = function(object) {
    cat("  Henze-Zirkler's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------------", "\n", sep = " ")
    cat("  data :", object@dname, "\n\n", sep = " ")
    cat("  HZ      :", object@HZ, "\n", sep = " ")
    cat("  p-value :", object@p.value, "\n\n", sep = " ")
    cat(if(object@p.value > 0.05){"  Result  : Data is multivariate normal."}
        else {"  Result  : Data is not multivariate normal."},"\n")
    cat("---------------------------------------------", "\n\n", sep = " ")
    invisible(NULL)
})



setClass("royston",
representation(H = "numeric", p.value="numeric", dname="character", dataframe="data.frame"))


setGeneric("royston", function(object) standardGeneric("royston"))


setMethod("show",
signature = "royston",
definition = function(object) {
    cat("  Royston's Multivariate Normality Test", "\n", sep = " ")
    cat("---------------------------------------------", "\n", sep = " ")
    cat("  data :", object@dname, "\n\n", sep = " ")
    cat("  H       :", object@H, "\n", sep = " ")
    cat("  p-value :", object@p.value, "\n\n", sep = " ")
    cat(if(object@p.value > 0.05){"  Result  : Data is multivariate normal."}
        else {"  Result  : Data is not multivariate normal."},"\n")
    cat("---------------------------------------------", "\n\n", sep = " ")
    invisible(NULL)
})

