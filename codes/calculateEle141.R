calculateEle141 = function(element11Num, element51Num, element61Num,
    element91Num, element95Num, element141Num, element161Num,
    ratio141Num, stotal, data){
    data[!itemCode %in% c(50, 58, 59, 60, 61),
         element141Num := ratio141Num * stotal/100]
    ## For commodity Jute (50)
    ## Define: calcType
    ## switch(calcTye,
    ##        `1` = {},
    ##        `2` = {},
    ##        `3` = {}
    ## )
    data[itemCode %in% c(58:61),
         element141Num := element11Num + element51Num + element61Num -
             element91Num - element95Num - element161Num]
}
