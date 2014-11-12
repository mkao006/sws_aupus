##' Function to find the number of elements which are missing from
##' multiple elements.
##'
##' @param ... The columns of the multiple elements, see base::sum
##' @export
##' 
numberOfMissingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(is.na(x))
                                     }
                   )
            )
}
