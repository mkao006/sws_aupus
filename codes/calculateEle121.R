calculateEle121 = function(element121Num, element121Symb,
    ratio121Num, stotal, data){
    setnames(data,
             old = c(element121Num, element121Symb, ratio121Num, stotal),
             new = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
    data[, element121Num := ratio121Num * stotal/100]
    data[!is.na(element121Num) & element121Symb == "M",
         element121Symb := "C"]
    setnames(data,
             new = c(element121Num, element121Symb, ratio121Num, stotal),
             old = c("element121Num", "element121Symb",
                 "ratio121Num", "stotal"))
}
