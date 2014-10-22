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
    data[, element541Num := element541Num + element151Num +
             element191Num]
    data[numberOfMissingElements == 0, element541Num := 0]
    data[numberOfMissingElements != 0 & !is.na(element541Num),
         element541Symb := "C"]
    data[numberOfMissingElements == 0, element541Symb := "M"]
    data[, numberOfMissingElements := NULL]
    setnames(data,
             new = c(element546Num, element546Symb, element541Num,
                 element151Num, element191Num),
             old = c("element546Num", "element546Symb,", "element541Num",
                 "element151Num", "element191Num"))
}
