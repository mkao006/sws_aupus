calculateEle41 = function(ratio41Num, 
    element41Num, element41Symb, data){
    setnames(data,
             old = c(ratio41Num, element41Num, element41Symb),
             new = c("ratio41Num", "element41Num", "element41Symb"))
    ## Do the new calculation
    newCalculation = data[, ratio41Num] * 100

    ## if new calculation is not possible, then set as missing
    newCalculation[is.na(newCalculation)] = NA
    
    ## Find the index for which the values were previously calculated
    previousCalculation =
        is.calculated(data[, element41Symb])
    
    ## Replace data which were previously calculated.
    data[previousCalculation,
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(newCalculation[previousCalculation], "C"))]
    setnames(data,
             new = c(ratio41Num, element41Num, element41Symb),
             old = c("ratio41Num", "element41Num", "element41Symb"))    
}
