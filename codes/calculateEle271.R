

calculateEle271 = function(element271Num, ratio271Num, element141Num,
    data){
    setnames(data,
             old = c(element271Num, ratio271Num, element141Num),
             new = c("element271Num", "ratio271Num", "element141Num"))
    data[, element271Num := ration271Num * element141Num/1000]
    setnames(data,
             new = c(element271Num, ratio271Num, element141Num),
             old = c("element271Num", "ratio271Num", "element141Num"))
}
