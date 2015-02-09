##' The function calculates element 144 (consumption per day)
##'
##' @param element144Num The column corresponding to value of element
##' 144.
##' @param element144Symb The column corresponding to symbol of element
##' 144.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param population11Num The column corresponds to element 11 of
##' population.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle144 = function(element144Num, element144Symb,
    element141Num, population11Num, itemTypeCol, data){
    setnames(data,
             old = c(element144Num, element144Symb,
                 element141Num, population11Num),
             new = c("element144Num", "element144Symb",
                 "element141Num", "population11Num"))
    replaceIndex1 =
        with(data, 
             which(data[[itemTypeCol]] %in%
                   c(46, 47, 48, 51, 52, 58, 59, 60, 61) &
                   replaceable(element144Symb)))
    data[replaceIndex1,
         `:=`(c("element144Num", "element144Symb"),
              appendSymbol(computeRatio(element141Num, population11Num) *
                               1000, "C"))]
    replaceIndex2 =
        with(data,
             which(!data[[itemTypeCol]] %in%
                   c(46, 47, 48, 51, 52, 58, 59, 60, 61) &
              replaceable(element144Symb)))
    data[replaceIndex2, 
         `:=`(c("element144Num", "element144Symb"),
              appendSymbol(computeRatio(element141Num, population11Num),
                           "C"))]
    
    setnames(data,
             new = c(element144Num, element144Symb,
                 element141Num, population11Num),
             old = c("element144Num", "element144Symb",
                 "element141Num", "population11Num"))
    list(replaceIndex1, replaceIndex2)
}
