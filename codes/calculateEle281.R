calculateEle281 = function(element281Num, element281Symb, ratio281Num,
    element141Num, data){
    setnames(data,
             old = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             new = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
    data[, element281Num := ratio281Num * element141Num/100]
    data[!is.na(element281Num) & element281Symb == "M",
         element281Symb := "C"]
    setnames(data,
             new = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             old = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
}
