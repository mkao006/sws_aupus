getShareData = function(database = c("new", "old"), param, conn){
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
                           keys = as.character(c("0", param$areaCode))),
                 Dimension(name = "measuredItemParentFS",
                           keys = as.character(param$itemCode)),
                 Dimension(name = "measuredItemChildFS",
                           keys = as.character(param$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", param$year))))

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

        fullShare =
            GetData(key = shareDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = sharePivot)
        fullShare[, flagShare := NULL]
        print(str(fullShare))
        setnames(fullShare,
                 old = c("Value", "measuredItemParentFS"),
                 new = c("share_value", "measuredItemFS"))


        specific = fullShare[geographicAreaFS != 0 &
                             timePointYearsSP != 0, ]
        setkeyv(specific, c("geographicAreaFS", "measuredItemFS",
                            "measuredItemChildFS", "timePointYearsSP"))
        yearWildCard = fullShare[geographicAreaFS != 0 &
                                 timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS",
                                "measuredItemFS",
                                "measuredItemChildFS"))
        areaYearWildCard =
            fullShare[geographicAreaFS == 0 & timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, c("measuredItemFS",
                                    "measuredItemChildFS"))
        finalShare = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    }
    finalShare
}