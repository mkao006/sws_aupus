##' The function calculates element 161 (Final existence)
##'
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param element161Symb The column corresponding to symbol of element
##' 161.
##' @param element11Num The column corresponding to value of element
##' 11.
##' @param element71Num The column corresponding to value of element
##' 71.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##'
##' 

calculateEle161 = function(element161Num, element161Symb,
    element11Num, element71Num, itemTypeCol, data){
    setnames(data,
             old = c(element161Num, element161Symb,
                 element11Num, element71Num),
             new = c("element161Num", "element161Symb", "element11Num",
                 "element71Num"))
    replaceIndex1 = which(data[[itemTypeCol]] == 57 &
                         replaceable(element161Symb))
    data[replaceIndex1,
         `:=`(c("element161Num", "element161Symb"),
              appendSymbol(element11Num + element71Num, "C"))]

    ## NOTE (Michael): It just says that the next year is forced to be
    ##                 processed if it is of type trade and also
    ##                 element 11 was already calculated for the
    ##                 following year.
    ##
    ## data[data[[itemTypeCol]] %in% c(2:13, 19:22, 25:30, 39), ]
    setnames(data,
             new = c(element161Num, element161Symb,
                 element11Num, element71Num),
             old = c("element161Num", "element161Symb", "element11Num",
                 "element71Num"))
    replaceIndex1
}
