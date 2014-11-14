##' This function calculates element 264 (calorie per day)
##'
##' @param element264Num The column corresponding to value of element
##' 264.
##' @param element264Symb The column corresponding to symbol of element
##' 264.
##' @param element261Num The column corresponding to value of element
##' 261.
##' @param population11Num The column corresponds to element 11 of the
##' population.
##' @param population21Num The column corresponds to element 21 of the
##' population.
##' @param data The data
##' @export
##' 

calculateEle264 = function(element264Num, element264Symb,
    element261Num, population11Num, population21Num, data){
    setnames(data,
             old = c(element264Num, element264Symb,
                 element261Num, population11Num, population21Num),
             new = c("element264Num", "element264Symb",
                 "element261Num", "population11Num", "population21Num"))
    data[, validPopulation := population21Num]
    data[is.na(validPopulation), validPopulation := population11Num]
    replaceIndex1 = with(data, which(replaceable(element264Symb)))
    data[replaceIndex1,
         `:=`(c("element264Num", "element264Symb"),
              appendSymbol(element261Num/365 * 1000/validPopulation,
                           "C"))]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element264Num, element264Symb,
                 element261Num, population11Num, population21Num),
             old = c("element264Num", "element264Symb",
                 "element261Num", "population11Num", "population21Num"))
}
