##' This function calculates element 71 (from initial existence)
##'
##' @param element71Num The column corresponding to value of element
##' 71.
##' @param element71Symb The column corresponding to symbol of element
##' 71.
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param element61Num The column corresponding to value of element
##' 61.
##' @param element91Num The column corresponding to value of element
##' 91.
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
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 



calculateEle71 = function(element71Num, element71Symb,
    element51Num, element61Num,
    element91Num, element101Num, element121Num, element131Num,
    element141Num, element151Num, element161Num, itemTypeCol, data){
    setnames(data,
             old = c(element71Num, element71Symb,
                 element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             new = c("element71Num", "element71Symb",
                 "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
    replaceIndex1 = which(data[[itemTypeCol]] == 58 &
                         replaceable(data$element71Symb))
    data[replaceIndex1,
         `:=`(c("element71Num", "element71Symb"),
              appendSymbol(element51Num + element61Num -
                   element91Num - element101Num - element121Num -
                   element131Num - element141Num - element151Num, "C"))]
    replaceIndex2 = which(data[[itemTypeCol]] %in% c(59, 60, 61) &
                         replaceable(data$element71Symb))

    data[replaceIndex2,
         `:=`(c("element71Num", "element71Symb"),
              appendSymbol(element161Num - element101Num, "C"))]
    
    ## NOTE (Michael): Ignoring element 57
    setnames(data,
             new = c(element71Num, element71Symb,
                 element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             old = c("element71Num", "element71Symb",
                 "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
    list(replaceIndex1, replaceIndex2)
}
