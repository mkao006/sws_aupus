calculateEle546 = function(element546Num, element546Symb, element541Num,
    element151Num, element191Num, data){
    setnames(data,
             old = c(element546Num, element546Symb, element541Num,
                 element151Num, element191Num),
             new = c("element546Num", "element546Symb,", "element541Num",
                 "element151Num", "element191Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element541Num, element151Num,
                                    element191Num)]
    data[, `:=`(c("element546Num", "element546Symb"),
                appendSymbol(rowSums(.SD[, list(element541Num,
                                                element151Num,
                                                element191Num)],
                                     na.rm = TRUE), "C"))]
    data[numberOfMissingElements == 3,
         `:=`(c("element546Num", "element546Symb"), list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]

    setnames(data,
             new = c(element546Num, element546Symb, element541Num,
                 element151Num, element191Num),
             old = c("element546Num", "element546Symb,", "element541Num",
                 "element151Num", "element191Num"))
}
