calculateEle6696 = function(data, shares, element41Num,
    element61Num, element66Num, element91Num, element96Num){
    setnames(data,
             old = c(element41Num, element61Num, element66Num,
                 element91Num, element96Num),
             new = c("element41Num", "element61Num", "element66Num",
                 "element91Num", "element96Num"))
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         element66Num :=
             standardizeCommodityNetwork(shares = shares[Year == .BY[[2]], ],
                                         aupus = data[Year == .BY[[2]], ],
                                         extractionRate = "element41Num",
                                         standardizeElement = "element61Num",
                                         commodity = as.character(.BY[[1]])
                                         ),
         by = c("itemCode", "Year")]
    data[itemType %in% c(2:13, 19:22, 25:30, 39),
         element96Num :=
             standardizeCommodityNetwork(shares = shares[Year == .BY[[2]], ],
                                         aupus = data[Year == .BY[[2]], ],
                                         extractionRate = "element41Num",
                                         standardizeElement = "element91Num",
                                         commodity = as.character(.BY[[1]])
                                         ),
         by = c("itemCode", "Year")]
    setnames(data,
             new = c(element41Num, element61Num, element66Num,
                 element91Num, element96Num),
             old = c("element41Num", "element61Num", "element66Num",
                 "element91Num", "element96Num"))
}
