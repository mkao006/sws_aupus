calculateEle66 = function(element41Num, element61Num, element66Num,
    element66Symb, data, share){
    setnames(data,
             old = c(element41Num, element61Num, element66Num,
                 element66Symb),
             new = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         `:=`(c("element66Num", "element66Symb"),
              list(standardizeCommodityNetwork(shares = share[Year == .BY[[1]], ],
                                               aupus = data[Year == .BY[[1]], ],
                                               extractionRate = "element41Num",
                                               standardizeElement = "element61Num",
                                               commodity = .SD$itemCode),
                   "C")),
         by = "Year"]
    setnames(data,
             new = c(element41Num, element61Num, element66Num,
                 element66Symb),
             old = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
}
