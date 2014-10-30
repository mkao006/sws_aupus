calculateEle101 = function(element101Num, element101Symb,
    ratio101Num, stotal, data){
    setnames(data,
             old = c(element101Num, element101Symb, ratio101Num, stotal),
             new = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))
    data[!is.na(ratio101Num),
         `:=`(c("element101Num", "element101Symb"),
              appendSymbol(ratio101Num * stotal/100, "C"))]
    setnames(data,
             new = c(element101Num, element101Symb, ratio101Num, stotal),
             old = c("element101Num", "element101Symb",
                 "ratio101Num", "stotal"))    
}
