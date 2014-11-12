getInputFromProcess = function(countryCode, database = c("new", "old"),
    conn){
    database = match.arg(database)
    if(database == "old"){
        inputQuery =
            paste0("SELECT *
                FROM input_from_procv
                WHERE area = ", countryCode)
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
        
        inputDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(testCountryCode)),
                 Dimension(name = "measuredItemParentFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "measuredItemChildFS",
                           keys = itemCodeList[type != 0, code]),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(testYears)))

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
                 new = c("inpute_value", "input_flag"))

        ## NOTE (Michael): Why does the year has 'SP' suffix?
        inputKey = c("geographicAreaFS", "measuredItemParentFS",
            "measuredItemChildFS", "timePointYearsSP")
        finalInput[, `:=`(inputKey,
                          lapply(finalInput[, inputKey, with = FALSE],
                                 as.numeric))]
        setkeyv(finalInput, cols = inputKey)
    }
    finalInput
}
        
