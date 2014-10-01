## Load the libraries
## library(faoswsExtra)
## library(data.table)
library(reshape2)
library(RJDBC)
library(data.table)
## library(FAOSTAT)


## Connect to the database
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "~/ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")


inputData =
    dbGetQuery(conn, "SELECT * FROM input_from_procv WHERE area = '9'")

meltedInput = melt(inputData,
    id.var = c("AREA", "ITEM_PARENT", "ITEM_CHILD"))

share =
    dbGetQuery(conn,
               "SELECT * FROM aupus_item_tree_shares WHERE area = '9'")

ratio =
    dbGetQuery(conn,
               "SELECT * FROM aupus_ratios WHERE area = '9'")





## Test of Wheat Germany
testCountryCode = 79
testItemCode = 15
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)

getAupusData = function(countryCode, itemCode, elementCode, conn){
    tmp = dbGetQuery(conn, paste0("SELECT * FROM tsv_ics_work_yr
                                  WHERE area = '", countryCode, "'
                                  AND item = '", itemCode, "'
                                  AND ele in (",
        paste0(elementCode, collapse = ", "), ")")
                     )
    colnames(tmp)[1:3] =
        c("areaCode", "itemCode", "elementCode")
    melted = melt(tmp,
        id.var = c("areaCode", "itemCode", "elementCode"))
    melted$Year =
        as.numeric(gsub("[^0-9]", "", melted$variable))
    melted$type = gsub("[0-9|_]", "", melted$variable)
    melted$variable = NULL
    casted = data.table(dcast(melted, areaCode + itemCode +
                                  Year ~ elementCode + type,
        value.var = "value"))
    valueCol = grep("NUM", colnames(casted), value = TRUE)
    casted[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(casted[[x]]))]    
    casted
}

rawAupus = getAupusData(testCountryCode, testItemCode, aupusElements,
                        conn)

## This is not required for primary commodities
getInputData = function(countryCode, itemCode, conn){
    tmp = dbGetQuery(conn, paste0("SELECT * FROM input_from_procv
                                  WHERE area = '", countryCode, "'
                                  AND item_child = '", itemCode, "'"))
    colnames(tmp)[1:3] =
        c("areaCode", "itemCode", "elementCode")
    melted = melt(tmp,
        id.var = c("areaCode", "itemCode", "elementCode"))
    melted$Year =
        as.numeric(gsub("[^0-9]", "", melted$variable))
    melted$type = gsub("[0-9|_]", "", melted$variable)
    melted$variable = NULL
    casted = data.table(dcast(melted, areaCode + itemCode +
                                  Year ~ elementCode + type,
        value.var = "value"))
    valueCol = grep("NUM", colnames(casted), value = TRUE)
    casted[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(casted[[x]]))]    
    casted
}
input = getInputData(testCountryCode, testItemCode, conn)



getShares = function(countryCode, itemCode, conn){
    tmp = dbGetQuery(conn, paste0("SELECT area, item, ele, yr, ratio
                                   FROM aupus_ratios
                                   WHERE area in (", countryCode, ")
                                   AND item = '", itemCode, "'
                                   AND yr != 0"))
    colnames(tmp) =
        c("areaCode", "itemCode", "elementCode", "Year",
          "ratio")

    ## year wild cared
    wildCardYear = dbGetQuery(conn,
        paste0("SELECT area, item, ele, ratio FROM aupus_ratios
                WHERE area in (", countryCode, ")
                AND yr = 0
                AND item = '", itemCode, "'"))
    colnames(wildCardYear) =
        c("areaCode", "itemCode", "elementCode", "wildCardYearRatio")
    ## global wild card
    wildCardGlobal = dbGetQuery(conn,
        paste0("SELECT item, ele, ratio FROM aupus_ratios
                WHERE area = 0 AND item = '", itemCode, "'"))
    colnames(wildCardGlobal) =
        c("itemCode", "elementCode", "wildCardGlobalRatio")
    Reduce(function(x, y) merge(x, y, all = TRUE), list(tmp, wildCardYear, wildCardGlobal))
    ## tmp2 = data.table(merge(tmp, wildCard, all = TRUE))
    ## tmp2[is.na(tmp2$ratio), ratio := defaultRatio]
    ## tmp2[, defaultRatio := NULL]
    ## tmp2
    ## tmp$elementCode = paste0(tmp$elementCode, "_RATIO")
    ## casted = dcast.data.table(tmp, areaCode + itemCode +
    ##                           Year + balanceIndex ~ elementCode,
    ##     value.var = "ratio"))
    ## casted = casted[Year != 0, ]
    ## casted
}
(shares = getShares(testCountryCode, testItemCode, conn))


## Need to check how the wild card work, we can already observe wild
## card for year but specific country, and also global item wild card.

dbGetQuery(conn, "SELECT * FROM aupus_ratios WHERE area = 0 AND YR = 0")
dbGetQuery(conn, "SELECT * FROM aupus_ratios WHERE area = 79 AND item = 15")


## Should also merge the input, by we will not do this for now for the
## primary commodity.
mergedAupus =
    merge(rawAupus, shares, by = c("areaCode", "itemCode", "Year"),
          all = TRUE)




## Apply the wild card at the end.
