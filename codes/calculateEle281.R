calculateEle281 = function(element281Num, element281Symb, ratio281Num,
    element141Num, data){
    setnames(data,
             old = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             new = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
    replaceIndex = with(data, which(replaceable(element281Symb)))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element281Num", "element281Symb"),
              appendSymbol(ratio281Num * element141Num/100, "C"))]
    setnames(data,
             new = c(element281Num, element281Symb, ratio281Num,
                 element141Num),
             old = c("element281Num", "element281Symb", "ratio281Num",
                 "element141Num"))
}
