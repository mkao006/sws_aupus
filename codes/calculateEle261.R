##' This function calculates element 261 (calories)
##'
##' @param element261Num The column corresponding to value of element
##' 261.
##' @param element261Symb The column corresponding to symbol of element
##' 261.
##' @param ratio261Num The column corresponding to ratio of element
##' 261.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param data The data
##' @export
##' 

calculateEle261 = function(element261Num, element261Symb, ratio261Num,
    element141Num, data){
    setnames(data,
             old = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             new = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
    replaceIndex1 = with(data, which(replaceable(element261Symb)))
    data[replaceIndex1,
         `:=`(c("element261Num", "element261Symb"),
              appendSymbol(ratio261Num * element141Num/100, "C"))]
    setnames(data,
             new = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             old = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
    replaceIndex1
}
