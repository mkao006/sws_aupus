##' This function calculates element 281 (fat)
##'
##' @param element281Num The column corresponding to value of element
##' 281.
##' @param element281Symb The column corresponding to symbol of element
##' 281.
##' @param ratio281Num The column corresponding to ratio of element
##' 281.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param data The data
##' @export
##' 

calculateEle281 = function(element281Num, element281Symb, ratio281Num,
    element141Num, data){
    setnames(data,
             old = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             new = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
    replaceIndex1 = with(data, which(replaceable(element281Symb)))
    data[replaceIndex1,
         `:=`(c("element281Num", "element281Symb"),
              appendSymbol(ratio281Num *
                               computeRatio(element141Num, 100), "C"))]
    setnames(data,
             new = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             old = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
    replaceIndex1
}
