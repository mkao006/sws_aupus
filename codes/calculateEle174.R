calculateEle174 = function(element174Num, element174Symb,
    element171Num, population, data){
    setnames(data,
             old = c(element174Num, element174Symb, element171Num,
                 population),
             new = c("element174Num", "element174Symb", "element171Num",
                 "population"))
    replaceIndex = which(data[[key(data)[2]]] == 57 &
                         replaceable(data$element174Symb))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element174Num", "element174Symb"),
              appendSymbol(element171Num * population, "C"))]
    
    setnames(data,
             new = c(element174Num, element174Symb, element171Num,
                 population),
             old = c("element174Num", "element174Symb", "element171Num",
                 "population"))
}
