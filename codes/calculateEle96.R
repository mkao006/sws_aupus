calculateEle96 = function(element41Num, element91Num, element96Num,
    data, share){
    setnames(data,
             old = c(element41Num, element91Num, element96Num),
             new = c("element41Num", "element91Num", "element96Num"))
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         element96Num :=
             standardizeCommodityNetwork(shares = share[Year == .BY[[1]], ],
                                         aupus = data[Year == .BY[[1]], ],
                                         extractionRate = "element41Num",
                                         standardizeElement = "element91Num",
                                         commodity = .SD$itemCode),
         by = "Year"]
    setnames(data,
             new = c(element41Num, element91Num, element96Num),
             old = c("element41Num", "element91Num", "element96Num"))
}
