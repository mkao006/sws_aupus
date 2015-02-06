##' This is the function to obtain the population separately.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 


## TODO (Michael): Apply calculation of 11 and 21 here.

getPopulationData = function(database = c("new", "old"), conn, aupusParam){
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
        finalPopulation =
            dcast.data.table(meltedAupus, AREA + Year ~ type + ELE,
                             value.var = "value")
        valueCol = grep("NUM", colnames(finalPopulation), value = TRUE)
        finalPopulation[, (valueCol) :=
                       lapply(valueCol, function(x)
                           as.numeric(finalPopulation[[x]]))]
        for(i in valueCol){
            remove0M(data = finalPopulation, value = i,
                     flag = gsub("NUM", "SYMB", i), naFlag = "M")
        }
        setnames(finalPopulation,
                 old = c("AREA"),
                 new = c("areaCode"))
        setkeyv(finalPopulation, cols = c("areaCode", "Year"))
    } else if(database == "new"){
        if(missing(param))
            stop("Aupus parameters are missing but required")
        populationDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(param$areaCode)),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(1)),
                 Dimension(name = "timePointYears",
                           keys = as.character(param$year)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(c(11, 21))))

        populationDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "FS1_SUA",
                       dimensions = populationDimension)

        populationPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYears", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )
        finalPopulation =
            GetData(key = populationDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = populationPivot)

        ## Convert list of NULL to vector of NA
        for(i in colnames(finalPopulation)){
            if(grepl("Value", i)){
                finalPopulation[, eval(parse(text =
                                   paste0(i, " := as.numeric(", i, ")")))]
            } else if(grepl("flag", i)){
                finalPopulation[, eval(parse(text =
                                paste0(i, " := as.character(", i, ")")))]
            }
        }

        setnames(finalPopulation,
                 old = c("timePointYears",
                     grep(param$keyNames$elementName, colnames(finalPopulation),
                          value = TRUE)),
                 new = c("timePointYearsSP",
                     gsub(param$keyNames$elementName, "population",
                          grep(param$keyNames$elementName,
                               colnames(finalPopulation), value = TRUE))))
        finalPopulation[, measuredItemFS := NULL]
        finalPopulation[, timePointYearsSP := as.numeric(timePointYearsSP)]
        finalPopulationKey = c("geographicAreaFS", "timePointYearsSP")
        setnames(finalPopulation,
                 old = grep("measuredItemFS", colnames(finalPopulation),
                     value = TRUE),
                 new = gsub("measuredItemFS", "population",
                     grep("measuredItemFS", colnames(finalPopulation),
                          value = TRUE)))
        setkeyv(finalPopulation, finalPopulationKey)
    }
    finalPopulation
}
        
