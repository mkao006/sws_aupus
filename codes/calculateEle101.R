calculateEle101 = function(element101Num, element101Symb,
    ratio101Num, stotal, data){
    setnames(data,
             old = c(element101Num, element101Symb, ratio101Num, stotal),
             new = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))
    replaceIndex = replaceable(data$element101Symb)
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element101Num", "element101Symb"),
              appendSymbol(ratio101Num * stotal/100, "C"))]
    setnames(data,
             new = c(element101Num, element101Symb, ratio101Num, stotal),
             old = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))    
}
