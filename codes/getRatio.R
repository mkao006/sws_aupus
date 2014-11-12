getRatio = function(countryCode, database = c("new", "old"), conn){
    database = match.arg(database)
    if(databse == "old"){
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
        finalRatio = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){

        aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 63, 66, 71,
            91, 92, 93, 95, 96, 101, 111, 121, 131, 141, 144, 151, 161,
            171, 174, 181, 191, 261, 264, 271, 274, 281, 284, 541, 546)

        ratioDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c(0, testCountryCode))),
                 Dimension(name = "measuredItemFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c(0, testYears))),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(intersect(aupusElements,
                               elementCodeList$code))))

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

        finalRatio =
            GetData(key = ratioDataContext, flags = TRUE,
                    normalized = FALSE,
                    pivoting = ratioPivot)

        ## Remove the symbol since it is not used
        finalRatio[, `:=`(c(grep("flag", colnames(finalRatio))), NULL)]

        sapply(names(which(sapply(finalRatio, typeof) == "logical")),
               FUN = function(x){
                   finalRatio[, eval(parse(text =
                                paste0(x, " := as.numeric(", x, ")")))]
                   invisible(NULL)
               })


        setnames(finalRatio,
                 old = grep("Value", colnames(finalRatio), value = TRUE),
                 new = gsub("Value", "finalRatio",
                     grep("Value", colnames(finalRatio), value = TRUE)))

        ratioKeys = c("geographicAreaFS", "measuredItemFS",
            "timePointYearsSP")
        finalRatio[, `:=`(c(ratioKeys),
                     lapply(finalRatio[, ratioKeys, with = FALSE],
                            as.numeric))]
        setkeyv(finalRatio, cols = ratioKeys)
    }
    finalRatio
}
        
