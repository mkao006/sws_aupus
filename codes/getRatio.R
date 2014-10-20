getRatio = function(countryCode, conn){
    ratioQuery =
        paste0("SELECT area, item, ele, yr, ratio
                FROM aupus_ratios
                WHERE area in (0, ", countryCode, ")")
    ratio =
        data.table(dbGetQuery(conn = conn, ratioQuery))
    ratio[, ELE := paste0("RATIO_", ELE)]
    castedRatio =
        dcast.data.table(data = ratio,
                         formula = AREA + ITEM + YR ~ ELE,
                         value.var = "RATIO")
    setnames(castedRatio,
             old = c("AREA", "ITEM", "YR"),
             new = c("areaCode", "itemCode", "Year"))

    specific = castedRatio[areaCode != 0 & Year != 0, ]
    setkeyv(specific, c("areaCode", "itemCode", "Year"))
    yearWildCard = castedRatio[areaCode != 0 & Year == 0,
        !"Year", with = FALSE]
    setkeyv(yearWildCard, c("areaCode", "itemCode"))
    areaYearWildCard = castedRatio[areaCode == 0 & Year == 0,
        !c("areaCode", "Year"), with = FALSE]
    setkeyv(areaYearWildCard, "itemCode")
    list(specific = specific, 
         yearWildCard = yearWildCard,
         areaYearWildCard = areaYearWildCard)
}
