##' This function extracts the share data from the data base.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 

getShareData = function(database = c("new", "old"), conn, aupusParam){
    database = match.arg(database)
    if(database == "old"){
        if(missing(conn))
            stop("Connection details are required but missing")
        shareQuery =
            paste0("SELECT area, item_parent, item_child, yr, aupus_share
                FROM aupus_item_tree_shares
                WHERE area in (0, ", areaCode, ")")
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
        if(missing(aupusParam))
            stop("Aupus aupusParameters are missing but required")
        shareDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c("0", aupusParam$areaCode))),
                 Dimension(name = "measuredItemParentFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "measuredItemChildFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", aupusParam$year))))

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
        fullShare[, `:=`(c(aupusParam$keyNames$yearName),
                         as.numeric(get(aupusParam$keyNames$yearName)))]
        setnames(fullShare,
                 old = c("Value"),
                 new = c("Value_share"))


        specific = fullShare[geographicAreaFS != "0" &
                             timePointYearsSP != 0, ]
        setkeyv(specific, c("geographicAreaFS", "measuredItemParentFS",
                            "measuredItemChildFS", "timePointYearsSP"))
        yearWildCard = fullShare[geographicAreaFS != "0" &
                                 timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS",
                                "measuredItemParentFS",
                                "measuredItemChildFS"))
        areaYearWildCard =
            fullShare[geographicAreaFS == "0" & timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, c("measuredItemParentFS",
                                    "measuredItemChildFS"))
        finalShare = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    }
    finalShare
}
