calculateEle66 = function(element41Num, element61Num, element66Num,
    data, share){
    setnames(data,
             old = c(element41Num, element61Num, element66Num),
             new = c("element41Num", "element61Num", "element66Num"))
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         element66Num :=
             standardizeCommodityNetwork(shares = share[Year == .BY[[1]], ],
                                         aupus = data[Year == .BY[[1]], ],
                                         extractionRate = "element41Num",
                                         standardizeElement = "element61Num",
                                         commodity = .SD$itemCode),
         by = "Year"]
    setnames(data,
             new = c(element41Num, element61Num, element66Num),
             old = c("element41Num", "element61Num", "element66Num"))
}
