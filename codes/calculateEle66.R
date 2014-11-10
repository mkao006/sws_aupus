calculateEle66 = function(element41Num, element61Num, element66Num,
    element66Symb, itemTypeCol, data, share){
    setnames(data,
             old = c(element41Num, element61Num, element66Num,
                 element66Symb),
             new = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
    ## NOTE (Michael): This can cause discrepancy if the manual data
    ##                 contradict each other.
    replaceIndex =
        which(data[[itemTypeCol]] %in% c(2:13, 19:22, 25:30, 39) &
              replaceable(data$element66Symb))
    print(replaceIndex)
    data[replaceIndex,
         `:=`(c("element66Num", "element66Symb"),
              appendSymbol(standardizeCommodityNetwork(shares = share[Year == .BY[[1]], ],
                                                       aupus = data[Year == .BY[[1]], ],
                                                       extractionRate = "element41Num",
                                                       standardizeElement = "element61Num",
                                                       commodity = .SD$itemCode),
                           "C")),
         by = c(key(data)[3])]
    setnames(data,
             new = c(element41Num, element61Num, element66Num,
                 element66Symb),
             old = c("element41Num", "element61Num", "element66Num",
                     "element66Symb"))
}
