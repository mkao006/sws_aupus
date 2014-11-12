##' This function calculates element 93 (outflow unit value)
##'
##' @param element91Num The column corresponding to value of element
##' 91.
##' @param element92Num The column corresponding to value of element
##' 92.
##' @param element93Num The column corresponding to value of element
##' 93.
##' @param element93Symb The column corresponding to symbol of element
##' 93.
##' @param data The data
##' @export
##' 

calculateEle93 = function(element91Num, element92Num,
    element93Num, element93Symb, data){
    setnames(data,
             old = c(element91Num, element92Num, element93Num,
                 element93Symb),
             new = c("element91Num", "element92Num", "element93Num",
                     "element93Symb"))
    ## Calculate value
    replaceIndex1 = with(data,
        which(!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49,
                               50, 51, 52) &
              !is.na(element91Num) & !is.na(element92Num) &
                  replaceable(element93Symb)))
    data[replaceIndex1,
         `:=`(c("element93Num", "element93Symb"),
              appendSymbol(element91Num * 1000/element92Num, "C"))]
    replaceIndex2 = with(data,
        which(!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49,
                               50, 51, 52) &
        replaceable(element93Symb) &
        (is.na(element91Num) | is.na(element92Num))))
    data[replaceIndex2,
         `:=`(c("element93Num", "element93Symb"),
              list(NA, "M"))]
    
    setnames(data,
             new = c(element91Num, element92Num, element93Num,
                     element93Symb),
             old = c("element91Num", "element92Num", "element93Num",
                     "element93Symb"))
    list(replaceIndex1, replaceIndex2)
}
