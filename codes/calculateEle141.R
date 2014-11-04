calculateEle141 = function(element141Num, element141Symb,
    element11Num, element51Num, element61Num,
    element91Num, element95Num, element161Num,
    ratio141Num, stotal, itemTypeCol, data){
    setnames(data,
             old = c(element141Num, element141Symb,
                 element11Num, element51Num, element61Num,
                 element91Num, element95Num, element161Num,
                 ratio141Num, stotal),
             new = c("element141Num", "element141Symb",
                 "element11Num", "element51Num", "element61Num",
                 "element91Num", "element95Num", "element161Num",
                 "ratio141Num", "stotal"))
    data[!data[[itemTypeCol]] %in% c(50, 58, 59, 60, 61),
         `:=`(c("element141Num", "element141Symb"),
              appendSymbol(ratio141Num * stotal/100, "C"))]

    ## NOTE(Michael): Calculation for commodity Jute (50) is not
    ##                replicated.
    data[data[[itemTypeCol]] %in% c(58:61),
         `:=`(c("element141Num", "element141Symb"),
              appendSymbol(element11Num + element51Num + element61Num -
                           element91Num - element95Num - element161Num,
                           "C"))]

    setnames(data,
             new = c(element141Num, element141Symb,
                 element11Num, element51Num, element61Num,
                 element91Num, element95Num, element161Num,
                 ratio141Num, stotal),
             old = c("element141Num", "element141Symb",
                 "element11Num", "element51Num", "element61Num",
                 "element91Num", "element95Num", "element161Num",
                 "ratio141Num", "stotal"))    
}
