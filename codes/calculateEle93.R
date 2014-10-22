calculateEle93 = function(element91Num, element92Num,
    element93Num, element93Symb, data){
    setnames(data,
             old = c(element91Num, element92Num, element93Num),
             new = c("element91Num", "element92Num", "element93Num"))
    ## Calculate value
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         !is.na(element91Num) & !is.na(element92Num),
         element93Num := element91Num * 1000/element92Num]
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         is.na(element91Num) | is.na(element92Num),
         element93Num := 0]

    ## Set flags
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         !is.na(element93Num) & element93Symb == "M",
         element93Symb := "C"]
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         is.na(element91Num) | is.na(element92Num),
         element93Symb := "M"]    
    
    setnames(data,
             new = c(element91Num, element92Num, element93Num),
             old = c("element91Num", "element92Num", "element93Num"))    
}
