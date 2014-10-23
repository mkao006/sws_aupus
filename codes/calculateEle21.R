calculateEle21 = function(element21Num, element21Symb, element11Num,
    element111Num, ratio171Num, data){
    setnames(data,
             old = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             new = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))
    ## NOTE (Michael): The denormalize of population should be done
    ##                 after the update of element 21.
    data[itemCode == 1, element21Num := element11Num]

    ## NOTE (Michael): This formula is derived from the formula of
    ##                 element 111 which has the reverse calculation.
    data[itemType %in% c(2, 3, 9, 29, 30),
         element21Num := element111Num * 1000/ratio171Num]
    setnames(data,
             new = c(element21Num, element21Symb, element11Num,
                 element111Num, ratio171Num),
             old = c("element21Num", "element21Symb", "element11Num",
                 "element111Num", "ratio171Num"))    
}
