

## Function to find the number of elements which are trended from
## multiple elements.
numberOfTrendingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(x == "T")
                                     }
                   )
            )
}
