##' This function extracts the extraction rate data from the aupus data
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 


getExtractionRateData = function(database = c("new", "old"), conn, aupusParam){
    database = match.arg(database)
    if(database == "old"){
        if(missing(conn))
            stop("Connection details are required but missing")
        aupusQuery =
            paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE area =", areaCode,
                "AND item = '1'")
        aupus =
            data.table(dbGetQuery(conn = conn, aupusQuery))
        meltedAupus =
            suppressWarnings(melt(aupus,
                                  id.var = c("AREA", "ITEM", "ELE")))
        meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedAupus[, type := gsub("[0-9|_]", "", variable)]
        meltedAupus[, `:=`(c("variable", "ITEM"), NULL)]
        finalExtractionRate =
            dcast.data.table(meltedAupus, AREA + Year ~ type + ELE,
                             value.var = "value")
        valueCol = grep("NUM", colnames(finalExtractionRate), value = TRUE)
        finalExtractionRate[, (valueCol) :=
                       lapply(valueCol, function(x)
                           as.numeric(finalExtractionRate[[x]]))]
        for(i in valueCol){
            remove0M(data = finalExtractionRate, value = i,
                     flag = gsub("NUM", "SYMB", i), naFlag = "M")
        }
        setnames(finalExtractionRate,
                 old = c("AREA"),
                 new = c("areaCode"))
        setkeyv(finalExtractionRate, cols = c("areaCode", "Year"))
    } else if(database == "new"){
        if(missing(aupusParam))
            stop("Aupus aupusParameters are missing but required")
        extractionRateDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(aupusParam$areaCode)),
                 Dimension(name = "measuredItemFS",
                           keys = aupusParam$itemCode),
                 Dimension(name = "timePointYears",
                           keys = as.character(aupusParam$year)),
                 Dimension(name = "measuredElementFS",
                           keys = "41"))

        extractionRateDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "FS1_SUA",
                       dimensions = extractionRateDimension)

        extractionRatePivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYears", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )
        finalExtractionRate =
            GetData(key = extractionRateDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = extractionRatePivot)

        ## Convert list of NULL to vector of NA
        for(i in colnames(finalExtractionRate)){
            if(grepl("Value", i)){
                finalExtractionRate[, eval(parse(text =
                                   paste0(i, " := as.numeric(", i, ")")))]
            } else if(grepl("flag", i)){
                finalExtractionRate[, eval(parse(text =
                                paste0(i, " := as.character(", i, ")")))]
            }
        }

        setnames(finalExtractionRate,
                 old = c("timePointYears",
                     grep(aupusParam$keyNames$elementName,
                          colnames(finalExtractionRate),
                          value = TRUE)),
                 new = c("timePointYearsSP", aupusParam$extractionRateName))
        finalExtractionRate[, measuredItemFS := NULL]
        finalExtractionRate[, timePointYearsSP := as.numeric(timePointYearsSP)]
        finalExtractionRateKey = c("geographicAreaFS", "timePointYearsSP")
        setnames(finalExtractionRate,
                 old = grep("measuredItemFS", colnames(finalExtractionRate),
                     value = TRUE),
                 new = gsub("measuredItemFS", "extractionRate",
                     grep("measuredItemFS", colnames(finalExtractionRate),
                          value = TRUE)))
        setkeyv(finalExtractionRate, finalExtractionRateKey)
    }
    finalExtractionRate
}
        
