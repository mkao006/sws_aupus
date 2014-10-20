
## Need to double check whether the name input131Num is appropriate.
calculateEle31 = function(element31Num, element31Symb, input131Num,
    data){
    setnames(data,
             old = c("element31Num", "element31Symb", "input131Num"),
             new = c(element31Num, element31Symb, input131Num))
    ## NOTE (Michael): These are assumed from the names in the Annex 3   
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         is.calculated(element31Symb) & !is.na(input131Num),
         element31Num := input131Num]
    
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         is.calculated(element31Symb) & is.na(input131Num),
         element31Num := 0]
    setnames(data,
             new = c("element31Num", "element31Symb", "input131Num"),
             old = c(element31Num, element31Symb, input131Num))    
}
