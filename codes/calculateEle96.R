calculateEle96 = function(element41Num, element91Num, element96Num,
    element96Symb, data, share){
    setnames(data,
             old = c(element41Num, element91Num, element96Num,
                 element96Symb),
             new = c("element41Num", "element91Num", "element96Num",
                     "element96Symb"))
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         `:=`(c("element96Num", "element96Symb"),
              appendSymbol(standardizeCommodityNetwork(shares = share[Year == .BY[[1]], ],
                                                       aupus = data[Year == .BY[[1]], ],
                                                       extractionRate = "element41Num",
                                                       standardizeElement = "element91Num",
                                                       commodity = .SD$itemCode),
                           "C")),
         by = "Year"]
    setnames(data,
             new = c(element41Num, element91Num, element96Num,
                 element96Symb),
             old = c("element41Num", "element91Num", "element96Num",
                     "element96Symb"))
}
