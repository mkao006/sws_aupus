calculateEle63 = function(element61Num, element62Num,
    element63Num, element63Symb, data){
    setnames(data,
             old = c(element61Num, element62Num, element63Num,
                 element63Symb),
             new = c("element61Num", "element62Num",
                 "element63Num", "element63Symb"))
    ## Calculate element 63 from element 61 and 62 if both are
    ## available.
    replaceIndex = with(data,
        which(!is.na(element61Num) &
              !is.na(element62Num) &
              replaceable(element63Symb)))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element63Num", "element63Symb"),
              appendSymbol(element61Num * 1000/element62Num, "C"))]
    ## If any one of them is missing, then the new calculation would
    ## be missing. Therefore, replace with zero.
    replaceIndex = with(data,
        which((is.na(element61Num) |
               is.na(element62Num)) &
              replaceable(element63Symb)))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element63Num", "element63Symb"),
              list(NA, "M"))]
    setnames(data,
             new = c(element61Num, element62Num,
                 element63Num, element63Symb),
             old = c("element61Num", "element62Num",
                 "element63Num", "element63Symb"))
}
