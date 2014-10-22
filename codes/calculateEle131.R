calculateEle131 = function(element131Num, element131Symb, ratio131Num,
    stotal, data){
    setnames(data,
             old = c(element131Num, element131Symb, ratio131Num, stotal),
             new = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))    
    data[, element131Num := ratio131Num * stotal/100]
    data[!is.na(element131Num) & element131Symb == "M",
         element131Symb := "C"]
    setnames(data,
             new = c(element131Num, element131Symb, ratio131Num, stotal),
             old = c("element131Num", "element131Symb",
                 "ratio131Num", "stotal"))        
}
