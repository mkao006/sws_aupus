calculateEle131 = function(element131Num, element131Symb, ratio131Num,
    stotal, data){
    setnames(data,
             old = c(element131Num, element131Symb, ratio131Num, stotal),
             new = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))    
    data[, `:=`(c("element131Num", "element131Symb"),
                appendSymbol(ratio131Num * stotal/100, "C"))]
    setnames(data,
             new = c(element131Num, element131Symb, ratio131Num, stotal),
             old = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))        
}
