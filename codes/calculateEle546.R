##' This function calculates element 546
##'
##' @param element546Num The column corresponding to value of element
##' 546.
##' @param element546Symb The column corresponding to symbol of element
##' 546.
##' @param element541Num The column corresponding to value of element
##' 541.
##' @param element151Num The column corresponding to value of element
##' 151.
##' @param element191Num The column corresponding to value of element
##' 191.
##' @param data The data
##' @export
##' 

calculateEle546 = function(element546Num, element546Symb, element541Num,
    element151Num, element191Num, data){
    setnames(data,
             old = c(element546Num, element546Symb, element541Num,
                 element151Num, element191Num),
             new = c("element546Num", "element546Symb", "element541Num",
                 "element151Num", "element191Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element541Num, element151Num,
                                    element191Num)]
    replaceIndex1 = with(data, which(replaceable(element546Symb)))
    data[replaceIndex1,
         `:=`(c("element546Num", "element546Symb"),
              appendSymbol(rowSums(.SD[, list(element541Num,
                                              element151Num,
                                              element191Num)],
                                   na.rm = TRUE), "C"))]
    replaceIndex2 = with(data, which(numberOfMissingElements == 3 &
                                    replaceable(element546Symb)))
    data[replaceIndex2,
         `:=`(c("element546Num", "element546Symb"), list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]

    setnames(data,
             new = c(element546Num, element546Symb, element541Num,
                 element151Num, element191Num),
             old = c("element546Num", "element546Symb", "element541Num",
                 "element151Num", "element191Num"))
    list(replaceIndex1, replaceIndex2)
}
