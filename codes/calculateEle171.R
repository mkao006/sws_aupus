calculateEle171 = function(element171Num, element171Symb, element101Num,
    element121Num, element131Num, element141Num, element151Num, data){
    setnames(data,
             old = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             new = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))

    data[itemCode == 57,
         `:=`(c("element171Num", "element171Symb"),
              list(element101Num + element121Num + element131Num +
                   element141Num + element151Num, "C"))]

    setnames(data,
             new = c(element171Num, element171Symb, element101Num,
                 element121Num,
                 element131Num, element141Num, element151Num),
             old = c("element171Num", "element171Symb", "element101Num",
                 "element121Num",
                 "element131Num", "element141Num", "element151Num"))    
}
