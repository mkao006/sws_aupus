

calculateEle284 = function(element21Num, element11Num, element284Num,
    element281Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element284Num,
                 element281Num),
             new = c("element21Num", "element11Num", "element284Num",
                 "element281Num"))       
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element284Num := element281Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element284Num,
                 element281Num),
             old = c("element21Num", "element11Num", "element284Num",
                 "element281Num"))    
}
