##' This is the function to obtain the population separately.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param param The parameter file from getAupusParam
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 

getPopulationData = function(database = c("new", "old"), param, conn){
    database = match.arg(database)
    if(database == "old"){
        aupusQuery =
            paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE area =", countryCode,
                AND item = '1')
        aupus =
            data.table(dbGetQuery(conn = conn, aupusQuery))
        meltedAupus =
            suppressWarnings(melt(aupus,
                                  id.var = c("AREA", "ITEM", "ELE")))
        meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedAupus[, type := gsub("[0-9|_]", "", variable)]
        meltedAupus[, `:=`(c("variable", "ITEM"), NULL)]
        finalAupus =
            dcast.data.table(meltedAupus, AREA + Year ~ type + ELE,
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
                 old = c("AREA"),
                 new = c("areaCode"))
        setkeyv(finalAupus, cols = c("areaCode", "Year"))
    } else if(database == "new"){
        populationDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(param$countryCode)),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(1)),
                 Dimension(name = "timePointYears",
                           keys = as.character(param$year)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(param$elementCode)))

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
        for(i in colnames(finalAupus)){
            if(typeof(finalAupus[, i, with = FALSE]) == "list"){
                finalAupus[, eval(parse(text =
                                   paste0(i, " := NULLtoNA(", i, ")")))]
            }
            if(grepl("Value", i)){
                finalAupus[, eval(parse(text =
                                   paste0(i, " := as.numeric(", i, ")")))]
            } else if(grepl("flag", i)){
                finalAupus[, eval(parse(text =
                                paste0(i, " := as.character(", i, ")")))]
            }
        }
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
        
