calculateEle63 = function(element61Num, element62Num,
    element63Num, data){
    setnames(data,
             old = c(element61Num, element62Num, element63Num),
             new = c("element61Num", "element62Num", "element63Num"))
    ## Calculate element 63 from element 61 and 62 if both are
    ## available.
    data[!is.na(element61Num) & !is.na(element62Num),
         element63Num := element61Num * 1000/element62Num]
    ## If any one of them is missing, then the new calculatino would
    ## be missing. Therefore, replace with zero.
    data[is.na(element61Num) | is.na(element62Num),
         element63Num := 0]
    setnames(data,
             new = c(element61Num, element62Num, element63Num),
             old = c("element61Num", "element62Num", "element63Num"))    
}
