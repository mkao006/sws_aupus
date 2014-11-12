##' Function to find the number of elements which are trended from
##' multiple elements.
##'
##' @param ... The columns of the multiple elements, see base::sum.

numberOfTrendingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(x == "T")
                                     }
                   )
            )
}
