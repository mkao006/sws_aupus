calculateEle174 = function(element174Num, element174Symb,
    element171Num, population, data){
    setnames(data,
             old = c(element174Num, element174Symb, element171Num,
                 population),
             new = c("element174Num", "element174Symb", "element171Num",
                 "population"))
    ## Assumes 171 calculated
    data[itemCode == 57, element174Num := element171Num * population]
    data[itemCode == 57 & !is.na(element174Num) & element174Symb == "M",
         element174Symb := "C"]
    
    setnames(data,
             new = c(element174Num, element174Symb, element171Num,
                 population),
             old = c("element174Num", "element174Symb", "element171Num",
                 "population"))
}
