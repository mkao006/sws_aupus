##' This function calculates element 66 (standardized inflow)
##'
##' @param element41Num The column corresponding to value of element
##' 41.
##' @param element61Num The column corresponding to value of element
##' 61.
##' @param element66Num The column corresponding to value of element
##' 66.
##' @param element66Symb The column corresponding to symbol of element
##' 66.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle66 = function(element41Num, element61Num, element66Num,
    element66Symb, shares, itemTypeCol, data, shareData){
    setnames(data,
             old = c(element41Num, element61Num, element66Num,
                 element66Symb),
             new = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
    setnames(shareData,
             old = shares,
             new = "shares")
    ## NOTE (Michael): This can cause discrepancy if the manual data
    ##                 contradict each other.
    replaceIndex1 =
        which(data[[itemTypeCol]] %in% c(2:13, 19:22, 25:30, 39) &
              replaceable(data$element66Symb))
    data[replaceIndex1,
         `:=`(c("element66Num", "element66Symb"),
              appendSymbol(standardizeCommodityNetwork(shareData = shareData[Year == .BY[[1]], ],
                                                       aupus = data[Year == .BY[[1]], ],
                                                       extractionRate = "element41Num",
                                                       standardizeElement = "element61Num",
                                                       shares = "shares",
                                                       commodity = .SD$itemCode),
                           "C")),
         by = c(key(data)[3])]
    setnames(data,
             new = c(element41Num, element61Num, element66Num,
                 element66Symb),
             old = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
    setnames(shareData,
             new = shares,
             old = "shares")    
    replaceIndex1
}
