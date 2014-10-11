## Function to find the number of elements which are missing from
## multiple elements.
numberOfMissingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(is.na(x))
                                     }
                   )
            )
}
