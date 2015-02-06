##' This function extracts the input from processing data from the
##' data base.
##'
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.
##' @export
##' 


getInputFromProcessData = function(database = c("new", "old"), 
    conn, aupusParam){
    database = match.arg(database)
    if(database == "old"){
        if(missing(conn))
            stop("Connection details are required but missing")        
        inputQuery =
            paste0("SELECT *
                FROM input_from_procv
                WHERE area = ", areaCode)
        input =
            data.table(dbGetQuery(conn, inputQuery))
        meltedInput =
            suppressWarnings(melt(input, id.var = c("AREA", "ITEM_PARENT",
                                             "ITEM_CHILD")))
        meltedInput[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedInput[, type := gsub("[0-9|_]", "", variable)]
        meltedInput[, variable := NULL]
        finalInput =
            dcast.data.table(meltedInput, AREA + ITEM_PARENT +
                                 ITEM_CHILD + Year ~ type,
                             value.var = "value")
        finalInput[, NUM := as.numeric(NUM)]
        remove0M(input, value = "NUM", flag = "SYMB", naFlag = "M")
        setnames(finalInput,
                 old = c("AREA", "ITEM_PARENT", "ITEM_CHILD", "NUM",
                     "SYMB"),
                 new = c("areaCode", "itemParentCode", "itemCode",
                     "NUM_INPUT", "SYMB_INPUT"))
        setkeyv(finalInput,
                cols = c("areaCode", "itemParentCode", "itemCode",
                    "Year"))
        finalInput
    } else if(database == "new"){
        if(missing(aupusParam))
            stop("Aupus parameters are missing but required")        
        inputDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(param$areaCode)),
                 Dimension(name = "measuredItemParentFS",
                           keys = as.character(param$itemCode)),
                 Dimension(name = "measuredItemChildFS",
                           keys = as.character(param$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(param$year)))

        inputDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "input_from_proc_fs",
                       dimensions = inputDimension)

        inputPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemParentFS", ascending = TRUE),
            Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
            Pivoting(code = "timePointYearsSP", ascending = FALSE)
        )

        finalInput =
            GetData(key = inputDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = inputPivot)

        setnames(finalInput,
                 old = c("Value", "flagFaostat"),
                 new = c("Value_input", "flagFaostat_input"))

        ## NOTE (Michael): Why does the year has 'SP' suffix?
        inputKey = c("geographicAreaFS", "measuredItemParentFS",
            "measuredItemChildFS", "timePointYearsSP")
        finalInput[, timePointYearsSP := as.numeric(timePointYearsSP)]
        ## finalInput[, `:=`(c(inputKey),
        ##                   lapply(finalInput[, inputKey, with = FALSE],
        ##                          as.numeric))]
        setkeyv(finalInput, cols = inputKey)
    }
    finalInput
}
        
