getAupusData = function(countryCode, database = c("new", "old"), conn){
    database = match.arg(database)
    if(database == "old"){
        aupusQuery =
            paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE area =", countryCode)
        aupus =
            data.table(dbGetQuery(conn = conn, aupusQuery))
        meltedAupus =
            suppressWarnings(melt(aupus,
                                  id.var = c("AREA", "ITEM", "ELE")))
        meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedAupus[, type := gsub("[0-9|_]", "", variable)]
        meltedAupus[, variable := NULL]
        finalAupus =
            dcast.data.table(meltedAupus, AREA + ITEM + Year ~ type + ELE,
                             value.var = "value")
        valueCol = grep("NUM", colnames(finalAupus), value = TRUE)
        finalAupus[, (valueCol) :=
                       lapply(valueCol, function(x)
                           as.numeric(finalAupus[[x]]))]
        for(i in valueCol){
            remove0M(data = finalAupus, value = i,
                     flag = gsub("NUM", "SYMB", i), naFlag = "M")
        }
        setnames(finalAupus,
                 old = c("AREA", "ITEM"),
                 new = c("areaCode", "itemCode"))
        setkeyv(finalAupus, cols = c("areaCode", "itemCode", "Year"))
    } else if(database == "new"){
        aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 63, 66, 71,
            91, 92, 93, 95, 96, 101, 111, 121, 131, 141, 144, 151,
            161, 171, 174, 181, 191, 261, 264, 271, 274, 281, 284, 541,
            546)
        aupusDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(testCountryCode)),
                 Dimension(name = "measuredItemFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "timePointYears",
                           keys = as.character(testYears)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(intersect(aupusElements,
                               elementCodeList$code))))

        aupusDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "FS1_SUA",
                       dimensions = aupusDimension)

        aupusPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYears", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )

        finalAupus =
            GetData(key = finalAupusDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = finalAupusPivot)

        ## Convert list of NULL to vector of NA
        for(i in colnames(finalAupus)){
            if(typeof(finalAupus[, i, with = FALSE]) == "list"){
                finalAupus[, eval(parse(text =
                                   paste0(i, " := NULLtoNA(", i, ")")))]
            } else if(typeof(finalAupus[, i, with = FALSE]) == "logical"){
                finalAupus[, eval(parse(text =
                                   paste0(x, " := as.numeric(", x, ")")))]
                
            }
        }
        
        setnames(finalAupus, "timePointYears", "timePointYearsSP")
        finalAupusKey = c("geographicAreaFS", "measuredItemFS",
            "timePointYearsSP")
        setkeyv(finalAupus, cols = finalAupusKey)        
    }
    finalAupus
}
