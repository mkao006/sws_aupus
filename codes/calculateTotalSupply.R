##' This function calculates total supply
##'
##' @param element11Num The column corresponding to value of element
##' 11.
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param element58Num The column corresponding to value of element
##' 58.
##' @param element61Num The column corresponding to value of element
##' 61.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateTotalSupply = function(element11Num, element51Num,
    element58Num, element61Num, element66Num, itemTypeCol, data){
    setnames(data,
             old = c(element11Num, element51Num,
                 element58Num, element61Num, element66Num),
             new = c("element11Num", "element51Num",
                 "element58Num", "element61Num", "element66Num"))
    data[data[[itemTypeCol]] %in% c(51, 58, 59, 61),
         TOTAL_SUPPLY:= element11Num + element51Num + element58Num +
             element61Num + element66Num]
    data[!data[[itemTypeCol]] %in% c(51, 58, 59, 61),
         TOTAL_SUPPLY:= element51Num + element58Num + element61Num +
             element66Num]
    setnames(data,
             new = c(element11Num, element51Num,
                 element58Num, element61Num, element66Num),
             old = c("element11Num", "element51Num",
                 "element58Num", "element61Num", "element66Num"))
}
