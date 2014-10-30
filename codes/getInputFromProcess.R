getInputFromProcess = function(countryCode, conn){
    inputQuery =
        paste0("SELECT *
                FROM input_from_procv
                WHERE area = ", countryCode)
    input =
        data.table(dbGetQuery(conn, inputQuery))
    meltedInput =
        melt(input, id.var = c("AREA", "ITEM_PARENT", "ITEM_CHILD"))
    meltedInput[, Year := as.numeric(gsub("[^0-9]", "", variable))]
    meltedInput[, type := gsub("[0-9|_]", "", variable)]
    meltedInput[, variable := NULL]
    finalInput =
        dcast.data.table(meltedInput, AREA + ITEM_PARENT + ITEM_CHILD +
                             Year ~ type,
                         value.var = "value")
    finalInput[, NUM := as.numeric(NUM)]
    remove0M(input, value = "NUM", flag = "SYMB", naFlag = "M")
    setnames(finalInput,
             old = c("AREA", "ITEM_PARENT", "ITEM_CHILD", "NUM", "SYMB"),
             new = c("areaCode", "itemParentCode", "itemCode",
                 "NUM_INPUT", "SYMB_INPUT"))
    setkeyv(finalInput,
            cols = c("areaCode", "itemParentCode", "itemCode", "Year"))
    finalInput
}
