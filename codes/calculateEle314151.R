## Function to balance element 31, 41, 51 after each has been
## calculated/updated.
calculateEle314151 = function(element31Num, element41Num, element51Num,
    element31Symb, element41Symb, element51Symb, data){
    setnames(data,
             old = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             new = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    ## Calculate conversion factor
    data[itemType == 55, fd := 1]
    data[itemType %in% c(58, 59, 61), fd := 1000]
    data[!itemType %in% c(55, 58, 59, 61), fd := 10000]
    
    ## Calculate condition statistics
    data[, numberOfMissingElements :=
             numberOfMissingElement(element31Num, element41Num,
                                    element51Num)]
    data[, numberOfTrendingElements :=
             numberOfTrendingElement(element31Symb, element41Symb,
                                     element51Symb)]    
    ## Start the balancing if there is only one missing value
    data[is.na(element31Num) & numberOfMissingElements == 1,
         element31Num := element51Num/element41Num * fd]
    data[is.na(element41Num) & numberOfMissingElements == 1,
         element41Num := element51Num/element31Num * fd]
    data[is.na(element51Num) & numberOfMissingElements == 1,
         element51Num := element31Num * element41Num * fd]

    ## Change the symbol
    data[!is.na(element31Num) & numberOfMissingElements == 1 &
         element31Symb == "M", element31Symb := "C"]
    data[!is.na(element41Num) & numberOfMissingElements == 1 &
         element41Symb == "M", element41Symb := "C"]
    data[!is.na(element51Num) & numberOfMissingElements == 1 &
         element51Symb == "M", element51Symb := "C"]    

    ## Recalculate the trend if there is only one trending value
    data[, element31Num :=
             trendOnce(element31Num,
                       which(numberOfTrendingElements == 1)),
         by = c("itemCode", "Year")]
    data[, element41Num :=
             trendOnce(element41Num,
                       which(numberOfTrendingElements == 1)),
         by = c("itemCode", "Year")]
    data[, element51Num :=
             trendOnce(element51Num,
                       which(numberOfTrendingElements == 1)),
         by = c("itemCode", "Year")]
    
    ## NOTE (Michael): Although not mentioned in the documentation,
    ## here we trend then balance in order to satisfy the equation.
    data[,
         element31Num := trendOnce(element31Num, 1),
         by = c("itemCode", "Year")]
    data[!is.na(element31Num) & 
         is.na(element41Num) & !is.na(element51Num),
         element41Num := element51Num/element41Num * fd]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         element51Num := element31Num * element41Num* fd]    
    data[numberOfTrendingElements >= 2,
         element41Num := trendOnce(element41Num, 1),
         by = c("itemCode", "Year")]
    data[is.na(element31Num) & 
         !is.na(element41Num) & !is.na(element51Num),
         element31Num := element51Num/element41Num * fd]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         element51Num := element31Num * element41Num * fd]

    data[, `:=`(c("numberOfMissingElements", "numberOfTrendingElements",
                  "fd"),
                NULL)]
    
    setnames(data,
             new = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             old = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    
}
