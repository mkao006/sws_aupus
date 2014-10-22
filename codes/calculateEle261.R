calculateEle261 = function(element261Num, element261Symb, ratio261Num,
    element141Num, data){
    setnames(data,
             old = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             new = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
    data[, element261Num := ratio261Num * element141Num/100]
    data[!is.na(element261Num) & element261Symb == "M",
         element261Symb := "C"]
    setnames(data,
             new = c(element261Num, element261Symb, ratio261Num,
                 element141Num),
             old = c("element261Num", "element261Symb", "ratio261Num",
                 "element141Num"))
}
