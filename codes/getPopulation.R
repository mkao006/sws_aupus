getPopulation = function(countryCode, conn){
    populationQuery =
        paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE item = 1
                AND area = ", countryCode)
    population =
        data.table(dbGetQuery(conn, populationQuery))
    meltedPopulation =
        melt(population, id.var = c("AREA", "ITEM", "ELE"))
    meltedPopulation[, Year := as.numeric(gsub("[^0-9]", "", variable))]
    meltedPopulation[, type := gsub("[0-9|_]", "", variable)]
    meltedPopulation[, ELE := paste0("POP", ELE)]
    meltedPopulation[, `:=`(c("ITEM", "variable"), NULL)]
    finalPopulation =
        dcast.data.table(meltedPopulation,
                         AREA + Year ~ type + ELE,
                         value.var = "value")
    valueCol = grep("NUM", colnames(finalPopulation), value = TRUE)
    finalPopulation[, (valueCol) :=
               lapply(valueCol, function(x)
                   as.numeric(finalPopulation[[x]]))]
    setnames(finalPopulation, old = "AREA", new = "areaCode")
    setkeyv(finalPopulation, c("areaCode", "Year"))
    finalPopulation
}
