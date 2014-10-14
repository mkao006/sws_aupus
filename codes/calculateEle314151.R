## Function to balance element 31, 41, 51 after each has been
## calculated/updated.
calculateEle314151 = function(element31Num, element41Num, element51Num,
    element31Symb, element41Symb, element51Symb, data){
    setnames(data,
             old = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             new = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    
    ## Calculate condition statistics
    data[, numberOfMissingElements :=
             numberOfMissingElement(element31Num, element41Num,
                                    element51Num)]
    data[, numberOfTrendingElements :=
             numberOfTrendingElement(element31Symb, element41Symb,
                                     element51Symb)]    

    ## Start the balancing if there is only one missing value
    data[is.na(element31Num) & numeberOfMissingElements == 1,
         element31Num := element51Num/element41Num]
    data[is.na(element41Num) & numeberOfMissingElements == 1,
         element41Num := element51Num/element31Num]
    data[is.na(element51Num) & numeberOfMissingElements == 1,
         element51Num := element31Num * element41Num]

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
         element41Num := element51Num/element41Num]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         element51Num := element31Num * element41Num]    
    data[numberOfTrendingElements >= 2,
         element41Num := trendOnce(element41Num, 1),
         by = c("itemCode", "Year")]
    data[is.na(element31Num) & 
         !is.na(element41Num) & !is.na(element51Num),
         element31Num := element51Num/element41Num]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         element51Num := element31Num * element41Num]

    data[, `:=`(c(numberOfMissingElements, numberOfTrendingElements),
                NULL)]
    
    setnames(data,
             new = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             old = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    
}
