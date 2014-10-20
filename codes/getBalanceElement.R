getBalanceElement = function(countryCode, conn){
    balanceElementQuery =
        paste0("SELECT area, item, ele, yr
                FROM aupus_ratios
                WHERE area in (0, ", countryCode, ")
                AND balance_ind = 'Y'")
    balanceElement =
        data.table(dbGetQuery(conn = conn, balanceElementQuery))
    setnames(balanceElement,
             old = c("AREA", "ITEM", "ELE", "YR"),
             new = c("areaCode", "itemCode", "balanceElement", "Year"))

    specific = balanceElement[areaCode != 0 & Year != 0, ]
    setkeyv(specific, c("areaCode", "itemCode", "Year"))
    yearWildCard = balanceElement[areaCode != 0 & Year == 0,
        !"Year", with = FALSE]
    setkeyv(yearWildCard, c("areaCode", "itemCode"))
    areaYearWildCard = balanceElement[areaCode == 0 & Year == 0,
        !c("areaCode", "Year"), with = FALSE]
    setkeyv(areaYearWildCard, "itemCode")
    list(specific = specific, 
         yearWildCard = yearWildCard,
         areaYearWildCard = areaYearWildCard)
}
