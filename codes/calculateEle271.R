##' This function calculates element 271 (protein)
##'
##' @param element271Num The column corresponding to value of element
##' 271.
##' @param element271Symb The column corresponding to symbol of element
##' 271.
##' @param ratio271Num The column corresponding to ratio of element
##' 271.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param data The data
##' @export
##' 

calculateEle271 = function(element271Num, element271Symb, ratio271Num,
    element141Num, data){
    setnames(data,
             old = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             new = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
    replaceIndex1 = with(data, which(replaceable(element271Symb)))
    data[replaceIndex1,
         `:=`(c("element271Num", "element271Symb"),
              appendSymbol(ratio271Num *
                               computeRatio(element141Num, 100) , "C"))]
    setnames(data,
             new = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             old = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
    replaceIndex1
}
