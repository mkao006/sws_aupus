calculateEle284 = function(element284Num, element284Symb,
    element261Num, population11, population21, data){
    setnames(data,
             old = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             new = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))
    data[, validPopulation := population21]
    data[is.na(validPopulation), validPopulation := population11]
    data[, element284Num := element261Num/365 * 1000/validPopulation]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element284Num, element284Symb,
                 element261Num, population11, population21),
             old = c("element284Num", "element284Symb",
                 "element261Num", "population11", "population21"))    
}
