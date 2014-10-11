

calculateEle261 = function(element261Num, ratio261Num, element141Num,
    data){
    setnames(data,
             old = c(element261Num, ratio261Num, element141Num),
             new = c("element261Num", "ratio261Num", "element141Num"))
    data[, element261Num := ration261Num * element141Num/100]
    setnames(data,
             new = c(element261Num, ratio261Num, element141Num),
             old = c("element261Num", "ratio261Num", "element141Num"))
}
