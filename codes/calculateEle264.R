calculateEle264 = function(element264Num, element264Symb,
    element261Num, population11, population21, data){
    setnames(data,
             old = c(element264Num, element264Symb,
                 element261Num, population11, population21),
             new = c("element264Num", "element264Symb",
                 "element261Num", "population11", "population21"))
    data[, validPopulation := population21]
    data[is.na(validPopulation), validPopulation := population11]
    data[, element264Num := element261Num/365 * 1000/validPopulation]
    data[, validPopulation := NULL]
    setnames(data,
             new = c(element264Num, element264Symb,
                 element261Num, population11, population21),
             old = c("element264Num", "element264Symb",
                 "element261Num", "population11", "population21"))    
}
