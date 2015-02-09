##' This function extracts the ratio data from the data base.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 


getRatioData = function(database = c("new", "old"), conn, aupusParam){
    database = match.arg(database)
    if(database == "old"){
        if(missing(conn))
            stop("Connection details are required but missing")
        ratioQuery =
            paste0("SELECT area, item, ele, yr, ratio
                FROM aupus_ratios
                WHERE area in (0, ", areaCode, ")")
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
        finalRatio = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){
        if(missing(aupusParam))
            stop("Aupus aupusParameters are missing but required")
        ratioDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c("0",
                               aupusParam$areaCode))),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", aupusParam$year))),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(aupusParam$elementCode)))

        ratioDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "aupus_ratio_fs",
                       dimensions = ratioDimension)

        ratioPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYearsSP", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )

        fullRatio =
            GetData(key = ratioDataContext, flags = TRUE,
                    normalized = FALSE,
                    pivoting = ratioPivot)

        ## Remove the symbol since they are the indicator whether it
        ## is the balancing element
        fullRatio[, `:=`(c(grep("flag", colnames(fullRatio))), NULL)]

        sapply(names(which(sapply(fullRatio, typeof) == "logical")),
               FUN = function(x){
                   fullRatio[, eval(parse(text =
                                paste0(x, " := as.numeric(", x, ")")))]
                   invisible(NULL)
               })

        setnames(fullRatio,
                 old = grep("Value", colnames(fullRatio), value = TRUE),
                 new = gsub("Value", "Ratio",
                     grep("Value", colnames(fullRatio), value = TRUE)))
        fullRatio[, timePointYearsSP := as.numeric(timePointYearsSP)]

        specific = fullRatio[geographicAreaFS != "0" &
                             timePointYearsSP != 0, ]
        setkeyv(specific,
                cols = c("geographicAreaFS", "measuredItemFS",
                    "timePointYearsSP"))
        yearWildCard = fullRatio[geographicAreaFS != "0" &
                                 timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS", "measuredItemFS"))
        areaYearWildCard = fullRatio[geographicAreaFS == "0" &
                                     timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, "measuredItemFS")

        finalRatio = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)


    }
    finalRatio
}
        
