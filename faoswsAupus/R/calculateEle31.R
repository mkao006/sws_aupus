##' This function calculates element 31
##'
##' @param element31Num The column corresponding to value of element
##' 31.
##' @param element31Symb The column corresponding to symbol of element
##' 31.
##' @param inputNum The column corresponds to input from processing
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle31 = function(element31Num, element31Symb, inputNum,
    itemTypeCol, data){
    setnames(data,
             new = c("element31Num", "element31Symb", "inputNum"),
             old = c(element31Num, element31Symb, inputNum))
    ## NOTE (Michael): These are assumed from the names in Annex 3
    replaceIndex1 =
        which(data[[itemTypeCol]] %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                         28, 3, 32, 33, 5, 6, 7, 8, 9) &
              !is.na(data$inputNum) & 
              replaceable(data$element31Symb))
    data[replaceIndex1, 
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(inputNum, "C"))]
    ## If neither input from processing or manual value is available,
    ## then replace with NA.
    replaceIndex2 = 
        which(data[[itemTypeCol]] %in% c(3, 10, 13, 15, 16, 22, 26, 27,
                                         28, 3, 32, 33, 5, 6, 7, 8, 9) &
              is.na(data$inputNum) & 
              replaceable(data$element31Symb))
    data[replaceIndex2,
         `:=`(c("element31Num", "element31Symb"), list(NA, "M"))]
    setnames(data,
             old = c("element31Num", "element31Symb", "inputNum"),
             new = c(element31Num, element31Symb, inputNum))
    list(replaceIndex1, replaceIndex2)
}
