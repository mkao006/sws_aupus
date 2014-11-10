calculateEle41 = function(ratio41Num, 
    element41Num, element41Symb, data){
    setnames(data,
             old = c(ratio41Num, element41Num, element41Symb),
             new = c("ratio41Num", "element41Num", "element41Symb"))
    
    replaceIndex =
        which(replaceable(data$element41Symb) &
              !is.na(ratio41Num))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element41Num", "element41Symb"),
              appendSymbol(ratio41Num * 100, "C"))]
    setnames(data,
             new = c(ratio41Num, element41Num, element41Symb),
             old = c("ratio41Num", "element41Num", "element41Symb"))    
}
