##' This function balance and fill in missing element 31, 41, and 51.
##'
##' @param element31Num The column corresponding to value of element
##' 31.
##' @param element31Symb The column corresponding to symbol of element
##' 31.
##' @param element41Num The column corresponding to value of element
##' 41.
##' @param element41Symb The column corresponding to symbol of element
##' 41.
##' @param element51Num The column corresponding to value of element
##' 51.
##' @param element51Symb The column corresponding to symbol of element
##' 51.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

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
    replaceIndex =
        with(data,
             which(is.na(element31Num) &
                   replaceable(element31Symb) &
                   (numberOfMissingElements == 1 |
                    numberOfTrendingElements)))
    data[replaceIndex,
         `:=`(c("element31Num", "element31Symb"),
             appendSymbol(computeRatio(element51Num, element41Num) * fd,
                          "C"))]

    replaceIndex2 =
        with(data,
             which(is.na(element41Num) &
                   replaceable(element41Symb) &
                   (numberOfMissingElements == 1 |
                    numberOfTrendingElements)))
    data[replaceIndex2,
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(computeRatio(element51Num, element31Num) * fd,
                           "C"))]

    replaceIndex3 =
        with(data,
             which(is.na(element51Num) &
                   replaceable(element51Symb) &
                   (numberOfMissingElements == 1 |
                    numberOfTrendingElements)))
    data[replaceIndex3, 
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num *
                               computeRatio(element41Num, fd), "C"))]


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
    replaceIndex4 = which(data[[key(data)[2]]] %in% c(0:1200, 1455:1700) &
                         replaceable(data$element31Symb))
    data[replaceIndex4,
         `:=`(c("element31Num", "element31Symb"),
              trendOnce(element31Num, element31Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    replaceIndex5 = with(data,
        which(!is.na(element31Num) & 
              is.na(element41Num) &
              !is.na(element51Num) &
              replaceable(element41Symb)))
    data[replaceIndex5, 
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(computeRatio(element51Num, element31Num) *
                               fd, "C"))]
    replaceIndex6 = with(data,
        which(!is.na(element31Num) & 
              !is.na(element41Num) &
              is.na(element51Num) &
              replaceable(element51Symb)))
    data[replaceIndex6,
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num * element41Num* fd, "C"))]
    replaceIndex7 =
        which(data[[key(data)[2]]] %in% c(0:1200, 1455:1700) &
                  replaceable(data$element41Symb))
    data[replaceIndex7,
         `:=`(c("element41Num", "element41Symb"),
              trendOnce(element41Num, element41Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    replaceIndex8 = with(data,
        which(is.na(element31Num) & 
              !is.na(element41Num) &
              !is.na(element51Num) &
              replaceable(element31Symb)))
    data[replaceIndex8,
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(computeRatio(element51Num, element41Num) *
                               fd, "C"))]
    replaceIndex9 = with(data,
        which(!is.na(element31Num) & 
              !is.na(element41Num) &
              is.na(element51Num) &
              replaceable(element51Symb)))
    data[replaceIndex9,
         `:=`(c("element51Num", "element51Symb"),
              appendSymbol(element31Num * element41Num * fd, "C"))]
    replaceIndex10 = which(data[[key(data)[2]]] %in% c(0:1200, 1455:1700) &
                         replaceable(data$element51Symb))
    data[replaceIndex10,
         `:=`(c("element51Num", "element51Symb"),
              trendOnce(element51Num, element51Symb,
                        which(numberOfTrendingElements > 1))),
         by = c(key(data)[2])]
    replaceIndex11 = with(data,
        which(is.na(element31Num) & 
              !is.na(element41Num) &
              !is.na(element51Num) &
              replaceable(element31Symb)))
    data[replaceIndex11, 
         `:=`(c("element31Num", "element31Symb"),
              appendSymbol(computeRatio(element51Num, element41Num) *
                               fd, "C"))]
    replaceIndex12 = with(data,
        which(!is.na(element31Num) & 
              !is.na(element41Num) &
              is.na(element51Num) &
              replaceable(element41Symb)))
    data[replaceIndex12,
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(computeRatio(element51Num, element31Num) *
                               fd, "C"))]
    
    data[, `:=`(c("numberOfMissingElements", "numberOfTrendingElements",
                  "fd"),
                NULL)]
    
    setnames(data,
             new = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             old = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))
    list(replaceIndex, replaceIndex2, replaceIndex3, replaceIndex4,
         replaceIndex5, replaceIndex6, replaceIndex7, replaceIndex8,
         replaceIndex9, replaceIndex10, replaceIndex11, replaceIndex12)
}
