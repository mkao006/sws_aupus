
calculateEle131 = function(element131Num, ratio131Num, stotal, data){
    setnames(data,
             old = c(element131Num, ratio131Num, stotal),
             new = c("element131Num", "ratio131Num", "stotal"))    
    data[, element131Num := ratio131Num * stotal/100]
    setnames(data,
             new = c(element131Num, ratio131Num, stotal),
             old = c("element131Num", "ratio131Num", "stotal"))        
}
