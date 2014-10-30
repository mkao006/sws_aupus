calculateEle121 = function(element121Num, element121Symb,
    ratio121Num, stotal, data){
    setnames(data,
             old = c(element121Num, element121Symb, ratio121Num, stotal),
             new = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
    data[, `:=`(c("element121Num", "element121Symb"),
                list(ratio121Num * stotal/100, "C"))]
    setnames(data,
             new = c(element121Num, element121Symb, ratio121Num, stotal),
             old = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
}
