calculateEle71 = function(element71Num, element71Symb,
    element51Num, element61Num,
    element91Num, element101Num, element121Num, element131Num,
    element141Num, element151Num, element161Num, itemTypeCol, data){
    setnames(data,
             old = c(element71Num, element71Symb,
                 element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             new = c("element71Num", "element71Symb",
                 "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
    
    data[data[[itemTypeCol]] == 58,
         `:=`(c("element71Num", "element71Symb"),
              appendSymbol(element51Num + element61Num -
                   element91Num - element101Num - element121Num -
                   element131Num - element141Num - element151Num, "C"))]
    data[data[[itemTypeCol]] %in% c(59, 60, 61),
         `:=`(c("element71Num", "element71Symb"),
              appendSymbol(element161Num - element101Num, "C"))]
    
    ## NOTE (Michael): Ignoring element 57
    setnames(data,
             new = c(element71Num, element71Symb,
                 element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             old = c("element71Num", "element71Symb",
                 "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
}
