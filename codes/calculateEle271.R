calculateEle271 = function(element271Num, element271Symb, ratio271Num,
    element141Num, data){
    setnames(data,
             old = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             new = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
    data[, element271Num := ratio271Num * element141Num/100]
    data[!is.na(element271Num) & element271Symb == "M",
         element271Symb := "C"]
    setnames(data,
             new = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             old = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
}
