getAupusData = function(countryCode, conn){
    aupusQuery =
        paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE area =", countryCode)
    aupus =
        data.table(dbGetQuery(conn = conn, aupusQuery))
    meltedAupus =
        melt(aupus, id.var = c("AREA", "ITEM", "ELE"))
    meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
    meltedAupus[, type := gsub("[0-9|_]", "", variable)]
    meltedAupus[, variable := NULL]
    finalAupus =
        dcast.data.table(meltedAupus, AREA + ITEM + Year ~ type + ELE,
                         value.var = "value")
    valueCol = grep("NUM", colnames(finalAupus), value = TRUE)
    finalAupus[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(finalAupus[[x]]))]
    for(i in valueCol){
        remove0M(data = finalAupus, value = i,
                 flag = gsub("NUM", "SYMB", i), naFlag = "M")
    }    
    setnames(finalAupus,
             old = c("AREA", "ITEM"),
             new = c("areaCode", "itemCode"))
    setkeyv(finalAupus, cols = c("areaCode", "itemCode", "Year"))
    finalAupus
}
