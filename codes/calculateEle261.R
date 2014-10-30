calculateEle261 = function(element261Num, element261Symb, ratio261Num,
    element141Num, data){
    setnames(data,
             old = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             new = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
    data[, `:=`(c("element261Num", "element261Symb"),
                appendSymbol(ratio261Num * element141Num/100, "C"))]
    setnames(data,
             new = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             old = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
}
