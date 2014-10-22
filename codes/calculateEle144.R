calculateEle144 = function(element144Num, element144Symb,
    element141Num, population, data){
    setnames(data,
             old = c(element144Num, element144Symb,
                 element141Num, population),
             new = c("element144Num", "element144Symb",
                 "element141Num", "population"))
    ## Assumes total consumption (element141Num) has already been
    ## calculated.
    data[itemType %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61),
         element144Num := element141Num/population * 1000]
    data[!itemType %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61),
         element144Num := element141Num/population]
    data[!is.na(element144Num) & element144Symb == "M",
         element144Symb := "C"]
    
    setnames(data,
             new = c(element144Num, element144Symb,
                 element141Num, population),
             old = c("element144Num", "element144Symb",
                 "element141Num", "population"))    
}
