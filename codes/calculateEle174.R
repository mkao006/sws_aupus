
calculateEle174 = function(element174Num, elemenet171Num, population,
    data){
    setnames(data,
             old = c(element174Num, elemenet171Num, population),
             new = c("element174Num", "elemenet171Num", "population"))
    ## Assumes 171 calculated
    data[itemCode == 57, element174Num := element171Num * population]
    setnames(data,
             new = c(element174Num, elemenet171Num, population),
             old = c("element174Num", "elemenet171Num", "population"))
}
