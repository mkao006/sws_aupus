getBalanceElement = function(countryCode, database = c("new", "old"),
    conn){
    database = match.args(database)
    if(database == "old"){
        balanceElementQuery =
            paste0("SELECT area, item, ele, yr
                FROM aupus_ratios
                WHERE area in (0, ", countryCode, ")
                AND balance_ind = 'Y'")
        balanceElement =
            data.table(dbGetQuery(conn = conn, balanceElementQuery))
        setnames(balanceElement,
                 old = c("AREA", "ITEM", "ELE", "YR"),
                 new = c("areaCode", "itemCode", "balanceElement",
                     "Year"))
        specific = balanceElement[areaCode != 0 & Year != 0, ]
        setkeyv(specific, c("areaCode", "itemCode", "Year"))
        yearWildCard = balanceElement[areaCode != 0 & Year == 0,
            !"Year", with = FALSE]
        setkeyv(yearWildCard, c("areaCode", "itemCode"))
        areaYearWildCard = balanceElement[areaCode == 0 & Year == 0,
            !c("areaCode", "Year"), with = FALSE]
        setkeyv(areaYearWildCard, "itemCode")
        balanceElementFull = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){
        balanceElementAll =
            GetData(key = ratioDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = ratioPivot)

        balanceElement = balanceElementAll[flagRatio == "Y",
            list(geographicAreaFS, measuredItemFS, measuredElementFS,
                 timePointYearsSP)]
        setnames(balanceElement,
                 old = c("geographicAreaFS", "measuredItemFS",
                     "measuredElementFS", "timePointYearsSP"),
                 new = c("areaCode", "itemCode", "balanceElement",
                     "Year"))

        tmp = lapply(balanceElement[, colnames(balanceElement),
            with = FALSE], as.numeric)
        balanceElement[, `:=`(c(colnames(balanceElement)), tmp)]
        balanceElementKey = c("areaCode", "itemCode", "Year")
        setkeyv(balanceElement, balanceElementKey)

        specific = balanceElement[areaCode != 0 & Year != 0, ]
        setkeyv(specific, c("areaCode", "itemCode", "Year"))
        yearWildCard = balanceElement[areaCode != 0 & Year == 0,
            !"Year", with = FALSE]
        setkeyv(yearWildCard, c("areaCode", "itemCode"))
        areaYearWildCard = balanceElement[areaCode == 0 & Year == 0,
            !c("areaCode", "Year"), with = FALSE]
        setkeyv(areaYearWildCard, "itemCode")

        balanceElementFull = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    }
    balanceElementFull
}
