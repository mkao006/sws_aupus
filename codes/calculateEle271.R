calculateEle271 = function(element271Num, element271Symb, ratio271Num,
    element141Num, data){
    setnames(data,
             old = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             new = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
    replaceIndex = with(data, which(replaceable(element271Symb)))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element271Num", "element271Symb"),
              appendSymbol(ratio271Num * element141Num/100, "C"))]
    setnames(data,
             new = c(element271Num, element271Symb, ratio271Num,
                 element141Num),
             old = c("element271Num", "element271Symb", "ratio271Num",
                 "element141Num"))
}
