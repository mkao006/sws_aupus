##' The function calculates element 101 (Use for animal)
##'
##' @param element101Num The column corresponding to value of element
##' 101.
##' @param element101Symb The column corresponding to symbol of
##' element 101.
##' @param ratio101Num The column corresponding to the ratio of
##' element 101.
##' @param stotal The column corresponding to the total supply.
##' @param data The data
##' @export
##'


calculateEle101 = function(element101Num, element101Symb,
    ratio101Num, stotal, data){
    setnames(data,
             old = c(element101Num, element101Symb, ratio101Num, stotal),
             new = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))
    replaceIndex1 = which(replaceable(data$element101Symb))
    data[replaceIndex1,
         `:=`(c("element101Num", "element101Symb"),
              appendSymbol(ratio101Num * computeRatio(stotal, 100),
                           "C"))]
    setnames(data,
             new = c(element101Num, element101Symb, ratio101Num, stotal),
             old = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))
    replaceIndex1
}
