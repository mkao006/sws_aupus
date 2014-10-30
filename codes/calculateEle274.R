calculateEle274 = function(element274Num, element274Symb,
    element261Num, population11, population21, data){
    setnames(data,
             old = c(element274Num, element274Symb,
                 element261Num, population11, population21),
             new = c("element274Num", "element274Symb",
                 "element261Num", "population11", "population21"))
    data[, validPopulation := population21]
    data[is.na(validPopulation), validPopulation := population11]
    data[, `:=`(c("element274Num", "element274Symb"),
                list(element261Num/365 * 1000/validPopulation, "C"))]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element274Num, element274Symb,
                 element261Num, population11, population21),
             old = c("element274Num", "element274Symb",
                 "element261Num", "population11", "population21"))    
}
