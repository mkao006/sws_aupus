##' Function to find the number of elements which are trended from
##' multiple elements.
##'
##' @param ... The columns of the multiple elements, see base::sum.

numberOfTrendingElement = function(...){
    listOfElements = list(...)
    isTrended = sapply(listOfElements, FUN = function(x){as.numeric(x == "T")})
    if(is.null(nrow(isTrended))){
        return(sum(isTrended))
    } else {
        return(rowSums(isTrended))
    }
}
