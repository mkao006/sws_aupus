calculateEle171 = function(element171Num, element171Symb, element101Num,
    element121Num, element131Num, element141Num, element151Num, data){
    setnames(data,
             old = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             new = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))

    data[data[[key(data)[2]]] == 57,
         `:=`(c("element171Num", "element171Symb"),
              appendSymbol(element101Num + element121Num +
                           element131Num + element141Num +
                           element151Num, "C"))]

    setnames(data,
             new = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             old = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))    
}
