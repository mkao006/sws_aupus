##' This function calculates element 63 (inflow unit value)
##'
##' @param element61Num The column corresponding to value of element
##' 61.
##' @param element62Num The column corresponding to value of element
##' 62.
##' @param element63Num The column corresponding to value of element
##' 63.
##' @param element63Symb The column corresponding to symbol of element
##' 63.
##' @param data The data
##' @export
##' 


calculateEle63 = function(element61Num, element62Num,
    element63Num, element63Symb, data){
    setnames(data,
             old = c(element61Num, element62Num, element63Num,
                 element63Symb),
             new = c("element61Num", "element62Num",
                 "element63Num", "element63Symb"))
    ## Calculate element 63 from element 61 and 62 if both are
    ## available.
    replaceIndex1 = with(data,
        which(!is.na(element61Num) &
              !is.na(element62Num) &
              replaceable(element63Symb)))
    data[replaceIndex1,
         `:=`(c("element63Num", "element63Symb"),
              appendSymbol(element61Num * 1000/element62Num, "C"))]
    ## If any one of them is missing, then the new calculation would
    ## be missing. Therefore, replace with zero.
    replaceIndex2 = with(data,
        which((is.na(element61Num) |
               is.na(element62Num)) &
              replaceable(element63Symb)))
    data[replaceIndex2,
         `:=`(c("element63Num", "element63Symb"),
              list(NA, "M"))]
    setnames(data,
             new = c(element61Num, element62Num,
                 element63Num, element63Symb),
             old = c("element61Num", "element62Num",
                 "element63Num", "element63Symb"))
    list(replaceIndex1, replaceIndex2)
}
