
calculateEle93 = function(element91Num, element92Num, element93Num,
    data){
    setnames(data,
             old = c(element91Num, element92Num, element93Num),
             new = c("element91Num", "element92Num", "element93Num"))
    data[!itemCode in c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         !is.na(element91Num) & !is.na(element92Num),
         element93Num := element91Num * 1000/element92Num]
    data[!itemCode in c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         is.na(element91Num) | is.na(element92Num),
         element93Num := 0]
    setnames(data,
             new = c(element91Num, element92Num, element93Num),
             old = c("element91Num", "element92Num", "element93Num"))    
}
