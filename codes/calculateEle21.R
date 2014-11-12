##' This function calculates element 21 (potential producing factor)
##'
##' @param element21Num The column corresponding to value of element
##' 21.
##' @param element21Symb The column corresponding to symbol of element
##' 21.
##' @param element111Num The column corresponding to value of element
##' 111.
##' @param ratio171Num The column corresponding to ratio of element
##' 171.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 


calculateEle21 = function(element21Num, element21Symb, element11Num,
    element111Num, ratio171Num, itemTypeCol, data){
    setnames(data,
             old = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             new = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))
    ## If the item was population then copy from element 11
    replaceIndex = which(data[[key(data)[2]]] == 1 &
                         replaceable(data$element21Symb))
    data[replaceIndex,
         `:=`(c("element21Num", "element21Symb"),
              appendSymbol(element11Num, "/"))]
    
    ## NOTE (Michael): This formula is derived from the formula of
    ##                 element 111 which has the reverse calculation.
    replaceIndex2 = which(data[[itemTypeCol]] %in% c(2, 3, 9, 29, 30) &
                         replaceable(data$element21Symb))
    data[replaceIndex2,
         `:=`(c("element21Num", "element21Symb"),
              appendSymbol(element111Num * 1000/ratio171Num, "C"))]
    setnames(data,
             new = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             old = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))
    list(replaceIndex, replaceIndex2)
}
