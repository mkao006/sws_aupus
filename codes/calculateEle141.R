##' The function calculates element 141 (Consumption)
##'
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param element141Symb The column corresponding to symbol of element
##' 141.
##' @param element11Num The column corresponding to value of element
##' 11.
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param element61Num The column corresponding to value of element
##' 61.
##' @param element91Num The column corresponding to value of element
##' 91.
##' @param element95Num The column corresponding to value of element
##' 95.
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param ratio141Num The column corresponding to ratio of element
##' 141.
##' @param stotal The column corresponding to total supply.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle141 = function(element141Num, element141Symb,
    element11Num, element51Num, element61Num,
    element91Num, element95Num, element161Num,
    ratio141Num, stotal, itemTypeCol, data){
    setnames(data,
             old = c(element141Num, element141Symb,
                 element11Num, element51Num, element61Num,
                 element91Num, element95Num, element161Num,
                 ratio141Num, stotal),
             new = c("element141Num", "element141Symb",
                 "element11Num", "element51Num", "element61Num",
                 "element91Num", "element95Num", "element161Num",
                 "ratio141Num", "stotal"))
    replaceIndex1 =
        which(!data[[itemTypeCol]] %in% c(50, 58, 59, 60, 61) &
              replaceable(data$element141Symb))
    data[replaceIndex1,
         `:=`(c("element141Num", "element141Symb"),
              appendSymbol(ratio141Num * stotal/100, "C"))]

    ## NOTE(Michael): Calculation for commodity Jute (50) is not
    ##                replicated.
    replaceIndex2 = which(data[[itemTypeCol]] %in% c(58:61) &
                         replaceable(data$element141Symb))
    data[replaceIndex2,
         `:=`(c("element141Num", "element141Symb"),
              appendSymbol(element11Num + element51Num + element61Num -
                           element91Num - element95Num - element161Num,
                           "C"))]

    setnames(data,
             new = c(element141Num, element141Symb,
                 element11Num, element51Num, element61Num,
                 element91Num, element95Num, element161Num,
                 ratio141Num, stotal),
             old = c("element141Num", "element141Symb",
                 "element11Num", "element51Num", "element61Num",
                 "element91Num", "element95Num", "element161Num",
                 "ratio141Num", "stotal"))
    list(replaceIndex1, replaceIndex2)
}
