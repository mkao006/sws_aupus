##' This function calculates element 58 (production crop year)
##'
##' @param element58Num The column corresponding to value of element
##' 58.
##' @param element58Symb The column corresponding to symbol of element
##' 58.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle58 = function(element58Num, element58Symb, itemTypeCol,
    data){
    setnames(data,
             old = c(element58Num, element58Symb),
             new = c("element58Num", "element58Symb"))

    sumCropYear = function(data){
        unlist(data[data[[key(data)[2]]] == 3158, element58Num]) +
            unlist(data[data[[key(data)[2]]] == 3159, element58Num])
    }

    data[, cropYear := sumCropYear(.SD), by = c(key(data)[3])]
    replaceIndex1 = which(data[[itemTypeCol]] == 57 &
                          replaceable(data$element58Symb))
    data[replaceIndex1,
         `:=`(c("element58Num", "element58Symb"),
              appendSymbol(cropYear, "C"))]
    data[, cropYear := NULL]
    setnames(data,
             new = c(element58Num, element58Symb),
             old = c("element58Num", "element58Symb"))
    replaceIndex1
}

