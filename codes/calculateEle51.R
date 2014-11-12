##' The function calculates element 51
##'
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param element51Symb The column corresponding to symbol of element
##' 51.
##' @param element58Num The column corresponding to value of element
##' 58.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle51 = function(element51Num, element51Symb, element58Num,
    itemTypeCol, data){
    setnames(data,
             old = c(element51Num, element51Symb, element58Num),
             new = c("element51Num", "element51Symb", "element58Num"))
    replaceIndex1 = which(data[[itemTypeCol]] %in% c(55, 56) &
                         !is.na(data$element58Num) &
                         replaceable(data$element51Symb))
    data[replaceIndex1, 
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element58Num, "C"))]

    ## TODO (Michael): Need to index this and find the matching index.
    data[data[[key(data)[2]]] == 3183,
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(sum(data[data[[key(data)[2]]] %in%
                                    c(3158, 3159), element51Num]), "C"))]
    setnames(data,
             new = c(element51Num, element51Symb, element58Num),
             old = c("element51Num", "element51Symb", "element58Num"))
    replaceIndex1
}
