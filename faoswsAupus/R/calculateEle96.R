##' This function calculates element 99 (standardized outflow)
##'
##' @param element41Num The column corresponding to value of element
##' 41.
##' @param element91Num The column corresponding to value of element
##' 91.
##' @param element96Num The column corresponding to value of element
##' 96.
##' @param element96Symb The column corresponding to symbol of element
##' 96.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##'



calculateEle96 = function(element41Num, element91Num, element96Num,
    element96Symb, shares, itemTypeCol, data, shareData){
    setnames(data,
             old = c(element41Num, element91Num, element96Num,
                 element96Symb),
             new = c("element41Num", "element91Num", "element96Num",
                     "element96Symb"))
    setnames(shareData,
             old = shares,
             new = "shares")    
    replaceIndex1 =
        which(data[[itemTypeCol]] %in% c(2:13, 19:22, 25:30, 39) &
              replaceable(data$element96Symb))
    data[replaceIndex1,
         `:=`(c("element96Num", "element96Symb"),
              appendSymbol(standardizeCommodityNetwork(shareData = shareData[Year == .BY[[1]], ],
                                                       aupus = data[Year == .BY[[1]], ],
                                                       extractionRate = "element41Num",
                                                       standardizeElement = "element91Num",
                                                       shares = "shares",
                                                       commodity = .SD$itemCode),
                           "C")),
         by = c(key(data)[3])]
    setnames(data,
             new = c(element41Num, element91Num, element96Num,
                 element96Symb),
             old = c("element41Num", "element91Num", "element96Num",
                     "element96Symb"))
        setnames(shareData,
             new = shares,
             old = "shares")
    replaceIndex1
}
