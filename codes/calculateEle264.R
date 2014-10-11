

calculateEle264 = function(element21Num, element11Num, element264Num,
    element261Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element264Num,
                 element261Num),
             new = c("element21Num", "element11Num", "element264Num",
                 "element261Num"))
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element264Num := element261Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element264Num,
                 element261Num),
             old = c("element21Num", "element11Num", "element264Num",
                 "element261Num"))
}
