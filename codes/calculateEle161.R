calculateEle161 = function(element161Num, element161Symb,
    element11Num, element71Num, data){
    setnames(data,
             old = c(element161Num, element161Symb,
                 element11Num, element71Num),
             new = c("element161Num", "element161Symb", "element11Num",
                 "element71Num"))
    data[itemType == 57, element161Num := element11Num + element71Num]
    data[itemType == 57 & !is.na(element161Num) & element161Symb == "M",
         element161Symb := "C"]
    ## NOTE (Michael): It just says that the next year is forced to be
    ## processed if it is of type trade and also element 11 was
    ## already calculated for the following year.
    ##
    ## data[itemType %in% c(2:13, 19:22, 25:30, 39), ]
    setnames(data,
             new = c(element161Num, element161Symb,
                 element11Num, element71Num),
             old = c("element161Num", "element161Symb", "element11Num",
                 "element71Num"))    
}
