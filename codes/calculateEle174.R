##' The function calculates element 174 (consumption per caput sugar)
##'
##'
##' @param element174Num The column corresponding to value of element
##' 174.
##' @param element174Symb The column corresponding to symbol of element
##' 174.
##' @param element171Num The column corresponding to value of element
##' 171.
##' @param population11Num The column corresponds to element 11 of the
##' population.
##' @param data The data
##' @export
##' 


calculateEle174 = function(element174Num, element174Symb,
    element171Num, population11Num, data){
    setnames(data,
             old = c(element174Num, element174Symb, element171Num,
                 population11Num),
             new = c("element174Num", "element174Symb", "element171Num",
                 "population11Num"))
    replaceIndex1 = which(data[[key(data)[2]]] == 57 &
                         replaceable(data$element174Symb))
    data[replaceIndex1,
         `:=`(c("element174Num", "element174Symb"),
              appendSymbol(element171Num/population11Num, "C"))]
    
    setnames(data,
             new = c(element174Num, element174Symb, element171Num,
                 population11Num),
             old = c("element174Num", "element174Symb", "element171Num",
                 "population11Num"))
    replaceIndex1
}
