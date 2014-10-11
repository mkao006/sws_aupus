calculateEle151 = function(element151Num, element131Num, element51Num,
    ratio151Num, stotal, data){
    setnames(data,
             old = c(element151Num, element131Num, element51Num,
                 ratio151Num, stotal),
             new = c("element151Num", "element131Num", "element51Num",
                 "ratio151Num", "stotal"))
    data[itemCode != 1697, element151Num := ratio151Num * stotal/100]

    tmp = merge(data[itemCode == 1684,
        list(itemCode, Year, element131Num)],
        data[itemCode == 1687, list(itemCode, Year, element51Num)],
        all = TRUE, by = c("itemCode", "Year"))
    tmp[, element151Calculated := element131Num - element51Num]
    tmp[, `:=`(c(element131Num, element51Num), NULL)]
    tmp[, itemCode:= 1687]
    data = merge(data, tmp, all = TRUE, by = c("itemCode", "Year"))
    data[itemCode == 1687, element151Num := element151Calculated]
    data[, element151Calculated := NULL]
    setnames(data,
             new = c(element151Num, element131Num, element51Num,
                 ratio151Num, stotal),
             old = c("element151Num", "element131Num", "element51Num",
                 "ratio151Num", "stotal"))    
}
