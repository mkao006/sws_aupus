
calculateEle281 = function(element281Num, ratio281Num, element141Num,
    data){
    setnames(data,
             old = c(element281Num, ratio281Num, element141Num),
             new = c("element281Num", "ratio281Num", "element141Num")
    data[, element281Num := ration281Num * element141Num/1000]
    setnames(data,
             new = c(element281Num, ratio281Num, element141Num),
             old = c("element281Num", "ratio281Num", "element141Num")
}
