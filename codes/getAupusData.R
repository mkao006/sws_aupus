getAupusData = function(database = c("new", "old"), param, conn){
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
        aupusDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(param$countryCode)),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(param$itemCode)),
                 Dimension(name = "timePointYears",
                           keys = as.character(param$year)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(param$elementCode)))

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
            GetData(key = aupusDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = aupusPivot)

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
        finalAupus[, timePointYearsSP := as.numeric(timePointYearsSP)]
        finalAupusKey = c("geographicAreaFS", "measuredItemFS",
            "timePointYearsSP")
        setkeyv(finalAupus, cols = finalAupusKey)        
    }
    finalAupus
}
