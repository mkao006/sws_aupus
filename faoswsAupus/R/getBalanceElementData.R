##' This function extracts the balance element data from the data
##' base.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 



getBalanceElementData = function(database = c("new", "old"), 
    conn, aupusParam){
    database = match.arg(database)
    if(database == "old"){
        if(missing(conn))
            stop("Connection details are required but missing")
        balanceElementQuery =
            paste0("SELECT area, item, ele, yr
                FROM aupus_ratios
                WHERE area in (0, ", areaCode, ")
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
        finalBalanceElement = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){
        if(missing(aupusParam))
            stop("Aupus aupusParameters are missing but required")
        balanceElementDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c("0",
                               aupusParam$areaCode))),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", aupusParam$year))),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(aupusParam$elementCode)))

        balanceElementDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "aupus_ratio_fs",
                       dimensions = balanceElementDimension)

        balanceElementPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYearsSP", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )

        fullBalanceElement  =
            GetData(key = balanceElementDataContext, flags = TRUE,
                    normalized = TRUE,
                    pivoting = balanceElementPivot)
        setnames(fullBalanceElement,
                 old = "measuredElementFS",
                 new = aupusParam$keyNames$balanceElementName)
        balanceElement =
            with(aupusParam$keyNames,
                 fullBalanceElement[flagRatio == "Y",
                                    c(areaName, itemName, yearName,
                                      balanceElementName), with = FALSE])

        balanceElement[, timePointYearsSP := as.numeric(timePointYearsSP)]
        ## tmp = lapply(balanceElement[, colnames(balanceElement),
        ##     with = FALSE], as.numeric)
        ## balanceElement[, `:=`(c(colnames(balanceElement)), tmp)]
        balanceElementKey = c("geographicAreaFS", "measuredItemFS",
            "timePointYearsSP")
        setkeyv(balanceElement, balanceElementKey)

        specific = balanceElement[geographicAreaFS != 0 &
                                  timePointYearsSP != 0, ]
        setkeyv(specific, cols = c("geographicAreaFS", "measuredItemFS",
                            "timePointYearsSP"))
        yearWildCard = balanceElement[geographicAreaFS != 0 &
                                      timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS", "measuredItemFS"))
        areaYearWildCard = balanceElement[geographicAreaFS == 0 &
                                          timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, "measuredItemFS")

        finalBalanceElement = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    }
    finalBalanceElement
}
