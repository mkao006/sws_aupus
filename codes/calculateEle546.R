
calculateEle546 = function(element541Num, element151Num, element191Num,
    data){
    setnames(data,
             old = c(element541Num, element151Num, element191Num),
             new = c("element541Num", "element151Num", "element191Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element541Num, element151Num,
                                    element191Num)]
    data[, element541Num := element541Num + element151Num +
             element191Num]
    data[numberOfMissingElements == 0, element541Num := 0]
    data[, numberOfMissingElements := NULL]
    setnames(data,
             new = c(element541Num, element151Num, element191Num),
             old = c("element541Num", "element151Num", "element191Num"))
}
