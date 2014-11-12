getShare = function(countryCode, database = c("new", "old"), conn){
    database = match.arg(database)
    if(database == "old"){
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
        setkeyv(specific, c("areaCode", "itemCode", "itemChildCode",
                            "Year"))
        yearWildCard = share[areaCode != 0 & Year == 0,
            !"Year", with = FALSE]
        setkeyv(yearWildCard, c("areaCode", "itemCode", "itemChildCode"))
        areaYearWildCard = share[areaCode == 0 & Year == 0,
            !c("areaCode", "Year"), with = FALSE]
        setkeyv(areaYearWildCard, c("itemCode", "itemChildCode"))
        finalShare = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){

        shareDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c(0, testCountryCode))),
                 Dimension(name = "measuredItemParentFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "measuredItemChildFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c(0, testYears))))

        shareDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "aupus_share_fs",
                       dimensions = shareDimension)

        sharePivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemParentFS", ascending = TRUE),
            Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
            Pivoting(code = "timePointYearsSP", ascending = FALSE)
        )

        finalShare =
            GetData(key = shareDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = sharePivot)
        finalShare[, flagShare := NULL]


        setnames(share, new = c("Value", "flagShare"),
                 old = c("share_value", "share_flag"))
        shareKeys = c("geographicAreaFS", "measuredItemParentFS",
            "measuredItemChildFS", "timePointYearsSP")
        finalShare[, `:=`(c(shareKeys),
                          lapply(finalShare[, shareKeys, with = FALSE],
                                 as.numeric))]
        setkeyv(finalShare, shareKeys)
    }
    finalShare
}
