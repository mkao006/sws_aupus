calculateTotalSupply = function(element11Num, element51Num,
    element58Num, element61Num, element66Num, itemTypeCol, data){
    setnames(data,
             old = c(element11Num, element51Num,
                 element58Num, element61Num, element66Num),
             new = c("element11Num", "element51Num",
                 "element58Num", "element61Num", "element66Num"))
    data[data[[itemTypeCol]] %in% c(51, 58, 59, 61),
         TOTAL_SUPPLY:= element11Num + element51Num + element58Num +
             element61Num + element66Num]
    data[!data[[itemTypeCol]] %in% c(51, 58, 59, 61),
         TOTAL_SUPPLY:= element51Num + element58Num + element61Num +
             element66Num]
    setnames(data,
             new = c(element11Num, element51Num,
                 element58Num, element61Num, element66Num),
             old = c("element11Num", "element51Num",
                 "element58Num", "element61Num", "element66Num"))
}
