calculateEle144 = function(element144Num, element144Symb,
    element141Num, population, data){
    setnames(data,
             old = c(element144Num, element144Symb,
                 element141Num, population),
             new = c("element144Num", "element144Symb",
                 "element141Num", "population"))
    replaceIndex =
        which(itemType %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61) &
              replaceable(data$element144Symb))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element144Num", "element144Symb"),
              appendSymbol(element141Num/population * 1000, "C"))]
    replaceIndex =
        which(!itemType %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61) &
              replaceable(data$element144Symb))
    data[replaceIndex, 
         `:=`(c("element144Num", "element144Symb"),
              appendSymbol(element141Num/population, "C"))]
    
    setnames(data,
             new = c(element144Num, element144Symb,
                 element141Num, population),
             old = c("element144Num", "element144Symb",
                 "element141Num", "population"))    
}
