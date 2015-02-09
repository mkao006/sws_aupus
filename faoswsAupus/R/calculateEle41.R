##' The function calculates element 41
##'
##' @param element41Num The column corresponding to value of element
##' 41.
##' @param element41Symb The column corresponding to symbol of element
##' 41.
##' @param ratio41Num The column corresponding to ratio of element
##' 41.
##' @param data The data
##' @export
##' 

calculateEle41 = function(element41Num, element41Symb, ratio41Num, data){
    setnames(data,
             old = c(ratio41Num, element41Num, element41Symb),
             new = c("ratio41Num", "element41Num", "element41Symb"))
    
    replaceIndex1 =
        which(replaceable(data$element41Symb) &
              !is.na(ratio41Num))
    data[replaceIndex1,
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(ratio41Num * 100, "C"))]
    setnames(data,
             new = c(ratio41Num, element41Num, element41Symb),
             old = c("ratio41Num", "element41Num", "element41Symb"))
    replaceIndex1
}
