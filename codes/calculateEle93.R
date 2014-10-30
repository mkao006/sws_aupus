calculateEle93 = function(element91Num, element92Num,
    element93Num, element93Symb, data){
    setnames(data,
             old = c(element91Num, element92Num, element93Num,
                 element93Symb),
             new = c("element91Num", "element92Num", "element93Num",
                     "element93Symb"))
    ## Calculate value
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         !is.na(element91Num) & !is.na(element92Num),
         `:=`(c("element93Num", "element93Symb"),
              appendSymbol(element91Num * 1000/element92Num, "C"))]
    data[!itemType %in% c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         is.na(element91Num) | is.na(element92Num),
         `:=`(c("element93Num", "element93Symb"),
              list(NA, "M"))]
    
    setnames(data,
             new = c(element91Num, element92Num, element93Num,
                     element93Symb),
             old = c("element91Num", "element92Num", "element93Num",
                     "element93Symb"))
}
