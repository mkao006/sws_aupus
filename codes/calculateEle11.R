##' Function to calculate element 11 (Initial existence)
##'
##' @param element11Num The column corresponding to value of element
##' 11.
##' @param element11Symb The column corresponding to symbol of element
##' 11.
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param element161Symb The column corresponding to symbol of element
##' 161.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateEle11 = function(element11Num, element11Symb,
    element161Num, element161Symb, itemTypeCol, data){
    setnames(data,
             old = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             new = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
    ## NOTE (Michael): The item Code list for stock type is contained
    ##                 in appendix A
    replaceIndex1 =
        which(data[[itemTypeCol]] %in%
              c(2:10, 13, 19:22, 25:28, 30, 57) &
              replaceable(data$element11Symb))
    data[replaceIndex1,
         `:=`(c("element11Num", "element11Symb"),
              trendOnce(Num = element161Num, Symb = element161Symb,
                        transfer = TRUE)),
         by = c(key(data)[2])]
    setnames(data,
             new = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             old = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
    replaceIndex1
}
