calculateEle71 = function(element71Num, element51Num, element61Num,
    element91Num, element101Num, element121Num, element131Num,
    element141Num, element151Num, element161Num, data){
    setnames(data,
             old = c(element71Num, element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             new = c("element71Num", "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
    data[itemCode == 58, element71Num := element51Num + element61Num -
             element91Num - element101Num - element121Num -
                 element131Num - element141Num - element151Num]
    data[itemCode %in% c(59, 60, 61),
         element71Num := element161Num - element101Num]
    ## NOTE (Michael): Ignoring element 57
    setnames(data,
             new = c(element71Num, element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             old = c("element71Num", "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num"))
}
