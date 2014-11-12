##' The function calculates element 151 (Reemployment other sector)
##'
##' @param element151Num The column corresponding to value of element
##' 151.
##' @param element151Symb The column corresponding to symbol of element
##' 151.
##' @param element131Num The column corresponding to value of element
##' 131.
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param ratio151Num The column corresponding to ratio of element
##' 151.
##' @param stotal The column corresponding to total supply.
##' @param data The data
##' @export
##' 

calculateEle151 = function(element151Num, element151Symb,
    element131Num, element51Num, ratio151Num, stotal, data){
    setnames(data,
             old = c(element151Num, element151Symb,
                 element131Num, element51Num, ratio151Num, stotal),
             new = c("element151Num", "element151Symb", "element131Num",
                 "element51Num", "ratio151Num", "stotal"))
    replaceIndex1 = which(data[[key(data)[2]]] != 1687 &
                         replaceable(data$element151Symb))
    data[replaceIndex1,
         `:=`(c("element151Num", "element151Symb"),
              appendSymbol(ratio151Num * stotal/100, "C"))]

    ## Item Charcoal (1687) ignored for now.
    ##
    ## tmp =
    ##     merge(data[itemCode == 1684,
    ##                list(itemCode, Year, element131Num)],
    ##           data[itemCode == 1687, list(itemCode, Year, element51Num)],
    ##           all = TRUE, by = c("itemCode", "Year"))
    ## tmp[, element151Calculated := element131Num - element51Num]
    ## tmp[, `:=`(c(element131Num, element51Num), NULL)]
    ## tmp[, itemCode:= 1687]
    ## data = merge(data, tmp, all = TRUE, by = c("itemCode", "Year"))
    ## data[itemCode == 1687, element151Num := element151Calculated]
    ## data[, element151Calculated := NULL]

    setnames(data,
             new = c(element151Num, element151Symb, element131Num,
                 element51Num, ratio151Num, stotal),
             old = c("element151Num", "element151Symb", "element131Num",
                 "element51Num", "ratio151Num", "stotal"))
    replaceIndex1
}
