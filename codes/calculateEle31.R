calculateEle31 = function(element31Num, element31Symb, inputNum,
    data){
    setnames(data,
             new = c("element31Num", "element31Symb", "inputNum"),
             old = c(element31Num, element31Symb, inputNum))
    ## NOTE (Michael): These are assumed from the names in the Annex 3   
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         (is.calculated(element31Symb) | element31Symb == "M") &
         !is.na(inputNum),
         element31Num := inputNum]
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         (is.calculated(element31Symb) | element31Symb == "M") &
         !is.na(inputNum),                             
         element31Symb := "C"]    
    
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         (is.calculated(element31Symb) | element31Symb == "M") &
         is.na(inputNum),
         element31Num := 0]
    data[itemType %in% c(3, 10, 13, 15, 16, 22, 26, 27, 28, 3,
                         32, 33, 5, 6, 7, 8, 9) &
         (is.calculated(element31Symb) | element31Symb == "M") &
         is.na(inputNum),
         element31Symb := "M"]
    setnames(data,
             old = c("element31Num", "element31Symb", "inputNum"),
             new = c(element31Num, element31Symb, inputNum))    
}
