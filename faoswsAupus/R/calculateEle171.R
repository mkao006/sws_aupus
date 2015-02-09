##' The function calculates element 171 (consumption sugar)
##'
##' @param element171Num The column corresponding to value of element
##' 171.
##' @param element171Symb The column corresponding to symbol of element
##' 171.
##' @param element101Num The column corresponding to value of element
##' 101.
##' @param element121Num The column corresponding to value of element
##' 121.
##' @param element131Num The column corresponding to value of element
##' 131.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param element151Num The column corresponding to value of element
##' 151.
##' @param data The data
##' @export
##' 


calculateEle171 = function(element171Num, element171Symb, element101Num,
    element121Num, element131Num, element141Num, element151Num, data){
    setnames(data,
             old = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             new = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))
    replaceIndex1 = which(data[[key(data)[2]]] == 57 &
                         replaceable(data$element171Symb))
    data[replaceIndex1,
         `:=`(c("element171Num", "element171Symb"),
              appendSymbol(rowSums(.SD[, list(element101Num,
                                              element121Num,
                                              element131Num,
                                              element141Num,
                                              element151Num)],
                                   na.rm = TRUE), "C"))]

    setnames(data,
             new = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             old = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))
    replaceIndex1
}
