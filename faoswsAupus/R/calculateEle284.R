##' This function calculates element 284 (fat per day)
##'
##' @param element284Num The column corresponding to value of element
##' 284.
##' @param element284Symb The column corresponding to symbol of element
##' 284.
##' @param element261Num The column corresponding to value of element
##' 261.
##' @param population11Num The column corresponds to element 11 of the
##' population.
##' @param population21Num The column corresponds to element 21 of the
##' population.
##' @param data The data
##' @export
##' 

calculateEle284 = function(element284Num, element284Symb,
    element261Num, population11, population21, data){
    setnames(data,
             old = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             new = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))
    data[, validPopulation := population21]
    data[is.na(validPopulation), validPopulation := population11]
    replaceIndex1 = with(data, which(replaceable(element284Symb)))
    data[replaceIndex1,
         `:=`(c("element284Num", "element284Symb"),
              appendSymbol(computeRatio(element261Num, 365) *
                           computeRatio(1000, validPopulation), 
                           "C"))]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             old = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))
    replaceIndex1
}
