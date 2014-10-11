


calculateEle274 = function(element21Num, element11Num, element274Num,
    element271Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element274Num,
                 element271Num),
             new = c("element21Num", "element11Num", "element274Num",
                 "element271Num"))
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element274Num := element271Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element274Num,
                 element271Num),
             nold = c("element21Num", "element11Num", "element274Num",
                 "element271Num"))    
}
