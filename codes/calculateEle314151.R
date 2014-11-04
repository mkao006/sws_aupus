## Function to balance element 31, 41, 51 after each has been
## calculated/updated.
calculateEle314151 = function(element31Num, element41Num, element51Num,
    element31Symb, element41Symb, element51Symb, itemTypeCol, data){
    setnames(data,
             old = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             new = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    ## Assign conversion factor
    data[data[[itemTypeCol]] == 55, fd := 1]
    data[data[[itemTypeCol]] %in% c(58, 59, 61), fd := 1000]
    data[!data[[itemTypeCol]] %in% c(55, 58, 59, 61), fd := 10000]
    
    ## Calculate condition statistics
    data[, numberOfMissingElements :=
             numberOfMissingElement(element31Num, element41Num,
                                    element51Num)]
    data[, numberOfTrendingElements :=
             numberOfTrendingElement(element31Symb, element41Symb,
                                     element51Symb)]
    
    ## Start the balancing if there is only one missing or trending
    ## value
    data[is.na(element31Num) &
         (numberOfMissingElements == 1 | numberOfTrendingElements),
         `:=`(c("element31Num", "element31Symb"),
             appendSymbol(element51Num/element41Num * fd, "C"))]
    ## data[is.na(element31Num) &
    ##      (numberOfMissingElements == 1 | numberOfTrendingElements) &
    ##      element31Symb == "M", element31Symb := "C"]
    data[is.na(element41Num) &
         (numberOfMissingElements == 1 | numberOfTrendingElements),
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(element51Num/element31Num * fd, "C"))]
    ## data[is.na(element41Num) &
    ##      (numberOfMissingElements == 1 | numberOfTrendingElements) &
    ##      element41Symb == "M", element41Symb := "C"]
    data[is.na(element51Num) &
         (numberOfMissingElements == 1 | numberOfTrendingElements),
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num * element41Num/fd, "C"))]
    ## data[is.na(element51Num) &
    ##      (numberOfMissingElements == 1 | numberOfTrendingElements) &
    ##      element51Symb == "M", element51Symb := "C"]    


    ## Remove prior trended value
    data[numberOfTrendingElements > 1 & element31Symb == "T",
         `:=`(c("element31Num", "element31Symb"),
              list(NA, "M"))]
    data[numberOfTrendingElements > 1 & element41Symb == "T",
         `:=`(c("element41Num", "element41Symb"),
              list(NA, "M"))]
    data[numberOfTrendingElements > 1 & element51Symb == "T",
         `:=`(c("element51Num", "element51Symb"),
              list(NA, "M"))]

    ## Re-trend the values
    ##
    ## NOTE (Michael): Although not mentioned in the documentation,
    ##                 here we trend then balance in order to satisfy
    ##                 the equation.
    ##
    ## NOTE (Michael): Only item in 0:1200 and 1455:1700 are trended
    ##                 as according to the documentation.
    data[data[[key(data)[2]]] %in% c(0:1200, 1455:1700),
         `:=`(c("element31Num", "element31Symb"),
              trendOnce(element31Num, element31Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    data[!is.na(element31Num) & 
         is.na(element41Num) & !is.na(element51Num),
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(element51Num/element31Num * fd, "C"))]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num * element41Num* fd, "C"))]
    data[data[[key(data)[2]]] %in% c(0:1200, 1455:1700),
         `:=`(c("element41Num", "element41Symb"),
              trendOnce(element41Num, element41Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    data[is.na(element31Num) & 
         !is.na(element41Num) & !is.na(element51Num),
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(element51Num/element41Num * fd, "C"))]
    data[!is.na(element31Num) & 
         !is.na(element41Num) & is.na(element51Num),
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num * element41Num * fd, "C"))]
    data[data[[key(data)[2]]] %in% c(0:1200, 1455:1700),
         `:=`(c("element51Num", "element51Symb"),
              trendOnce(element51Num, element51Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    data[is.na(element31Num) & 
             !is.na(element41Num) & !is.na(element51Num),
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(element51Num/element41Num * fd, "C"))]
    data[!is.na(element31Num) & 
             !is.na(element41Num) & is.na(element51Num),
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(element51Num/element31Num * fd, "C"))]
    
    data[, `:=`(c("numberOfMissingElements", "numberOfTrendingElements",
                  "fd"),
                NULL)]
    
    setnames(data,
             new = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             old = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    
}
