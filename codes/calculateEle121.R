##' The function to calculate element 121 (Losses)
##'
##' @param element121Num The column corresponding to value of element
##' 121.
##' @param element121Symb The column corresponding to symbol of element
##' 121.
##' @param ratio121Num The column corresponding to ratio of element
##' 121.
##' @param stotal The column corresponding to total supply
##' @param data The data
##' @export
##' 

calculateEle121 = function(element121Num, element121Symb,
    ratio121Num, stotal, data){
    setnames(data,
             old = c(element121Num, element121Symb, ratio121Num, stotal),
             new = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
    replaceIndex1 = with(data, which(replaceable(element121Symb)))
    data[replaceIndex1,
         `:=`(c("element121Num", "element121Symb"),
              appendSymbol(ratio121Num * stotal/100, "C"))]
    setnames(data,
             new = c(element121Num, element121Symb, ratio121Num, stotal),
             old = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
    replaceIndex1
}
