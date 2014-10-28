getShare = function(countryCode, conn){
    shareQuery =
        paste0("SELECT area, item_parent, item_child, yr, aupus_share
                FROM aupus_item_tree_shares
                WHERE area in (0, ", countryCode, ")")
    share =
        data.table(dbGetQuery(conn = conn, shareQuery))
    setnames(share,
             old = c("AREA", "ITEM_PARENT", "ITEM_CHILD", "YR",
                 "AUPUS_SHARE"),
             new = c("areaCode", "itemCode", "itemChildCode", "Year",
                 "SHARE"))
    specific = share[areaCode != 0 & Year != 0, ]
    setkeyv(specific, c("areaCode", "itemCode", "itemChildCode", "Year"))
    yearWildCard = share[areaCode != 0 & Year == 0,
        !"Year", with = FALSE]
    setkeyv(yearWildCard, c("areaCode", "itemCode", "itemChildCode"))
    areaYearWildCard = share[areaCode == 0 & Year == 0,
        !c("areaCode", "Year"), with = FALSE]
    setkeyv(areaYearWildCard, c("itemCode", "itemChildCode"))
    list(specific = specific, 
         yearWildCard = yearWildCard,
         areaYearWildCard = areaYearWildCard)
}
