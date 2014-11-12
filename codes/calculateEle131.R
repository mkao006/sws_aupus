##' The function calculates element 131 (Reemployment Same Sector)
##'
##' @param element131Num The column corresponding to value of element
##' 131.
##' @param element131Symb The column corresponding to symbol of element
##' 131.
##' @param ratio131Num The column corresponding to ratio of element
##' 131.
##' @param stotal The column corresponding to total supply.
##' @param data The data
##' @export
##' 

calculateEle131 = function(element131Num, element131Symb, ratio131Num,
    stotal, data){
    setnames(data,
             old = c(element131Num, element131Symb, ratio131Num, stotal),
             new = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))
    replaceIndex1 = with(data, which(replaceable(element131Symb)))
    data[replaceIndex1,
         `:=`(c("element131Num", "element131Symb"),
              appendSymbol(ratio131Num * stotal/100, "C"))]
    setnames(data,
             new = c(element131Num, element131Symb, ratio131Num, stotal),
             old = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))
    replaceIndex1
}
