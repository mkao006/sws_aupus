calculateEle51 = function(){
    data[itemCode %in% c(55, 56) &
         !is.na(element58Num), element51Num := element58Num]
    data[itemCode == 3183, element51Num :=
             sum(data[itemCode %in% c(3158, 3159), element51Num],
                 na.rm = TRUE)]

    ## This is actually documented in the "Production Element
    ## Calculation"

    data[itemCode == 55, element51Num := element31Num * element41Num]
    data[itemCode %in% c(58, 59, 61),
         element51Num := element31Num * element41Num/1000]
    data[!itemCode %in% c(55, 56, 58, 59,61),
         element51Num := element31Num * element41Num/10000]
}
