calculateEle284 = function(element284Num, element284Symb,
    element261Num, population11, population21, data){
    setnames(data,
             old = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             new = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))
    data[, validPopulation := population21]
    data[is.na(validPopulation), validPopulation := population11]
    replaceIndex = with(data, which(replaceable(element284Symb)))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element284Num", "element284Symb"),
              appendSymbol(element261Num/365 * 1000/validPopulation,
                           "C"))]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             old = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))    
}
