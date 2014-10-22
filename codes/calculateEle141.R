calculateEle141 = function(element141Num, element141Symb,
    element11Num, element51Num, element61Num,
    element91Num, element95Num, element161Num,
    ratio141Num, stotal, data){
    setnames(data,
             old = c(element141Num, element141Symb,
                 element11Num, element51Num, element61Num,
                 element91Num, element95Num, element161Num,
                 ratio141Num, stotal),
             new = c("element141Num", "element141Symb",
                 "element11Num", "element51Num", "element61Num",
                 "element91Num", "element95Num", "element161Num",
                 "ratio141Num", "stotal"))
    data[!itemType %in% c(50, 58, 59, 60, 61),
         element141Num := ratio141Num * stotal/100]
    data[!itemType %in% c(50, 58, 59, 60, 61) &
         !is.na(element141Num) & element141Symb == "M",
         element141Symb := "C"]
    ## For commodity Jute (50)
    ## Define: calcType
    ## switch(calcTye,
    ##        `1` = {},
    ##        `2` = {},
    ##        `3` = {}
    ## )
    data[itemType %in% c(58:61),
         element141Num := element11Num + element51Num + element61Num -
             element91Num - element95Num - element161Num]
    data[itemType %in% c(58:61) &
         !is.na(element141Num) & element141Symb == "M",
         element141Symb := "C"]
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
