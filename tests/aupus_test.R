## Load the libraries
library(igraph)
library(reshape2)
library(RJDBC)
library(data.table)
library(FAOSTAT)
library(faoswsUtil)
options(java.parameters = "-Xmx3000m")
lapply(dir("../codes/", full.names = TRUE), FUN = source)


## Connect to the database
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "~/ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")

swsItemTable = data.table(dbGetQuery(conn,
    "SELECT item, name_e, item_typ FROM item"))
setnames(swsItemTable,
         old = c("ITEM", "NAME_E", "ITEM_TYP"),
         new = c("itemCode", "itemName", "itemType"))
save(swsItemTable, file = "swsItemTable.RData")

## Write a function for wild key matching in both ratio and share table

## Test of Germany
testCountryCode = 79
testItemCode = swsItemTable[swsItemTable$GRP_IND == "D", "ITEM"]
## testItemCode = 1:1000
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)

check = getAupusData(testCountryCode, conn)

             
rawAupus =
    getAupusData(testCountryCode, testItemCode, aupusElements,
                 conn)
save(rawAupus, file = "aupusData.RData")

getInputFromProcess = function(countryCode, itemCode, conn){
    tmp = dbGetQuery(conn, paste0("SELECT * FROM input_from_procv
                                  WHERE area = '", countryCode, "'
                                  AND item_child in (",
                                  paste0(itemCode, collapse = ", "), ")"))
    colnames(tmp)[1:3] =
        c("areaCode", "itemCode", "itemChildCode")
    melted = melt(tmp,
        id.var = c("areaCode", "itemCode",
            "itemChildCode"))
    melted$Year =
        as.numeric(gsub("[^0-9]", "", melted$variable))
    melted$type = gsub("[0-9|_]", "", melted$variable)
    melted$variable = NULL
    casted = data.table(dcast(melted, areaCode + itemChildCode +
                                  itemCode +
                                  Year ~ type,
        value.var = "value"))
    valueCol = grep("NUM", colnames(casted), value = TRUE)
    casted[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(casted[[x]]))]
    setnames(casted,
             old = c(grep("NUM", colnames(casted), value = TRUE),
                 "SYMB"),
             new = c(gsub("NUM", "INPUT", 
                 grep("NUM", colnames(casted), value = TRUE)),
                 "SYMB_INPUT"))
    casted
}
input = getInputFromProcess(testCountryCode, testItemCode, conn)
save(input, file = "input.RData")


balanceElement = getBalanceElement(testCountryCode, conn)
save(balanceElement, file = "balanceElement.RData")
ratio = getRatio(testCountryCode, conn)
save(ratio, file = "ratio.RData")
share = getShare("79", conn)
save(share, file = "share.RData")


## Should also merge the input, by we will not do this for now for the
## primary commodity.
load("swsItemTable.RData")
load("aupusData.RData")
load("input.RData")
load("ratio.RData")
load("share.RData")


## Need to remove 0M for all the data.
treeData = merge(input, shares,
    by = c("areaCode", "itemCode", "itemChildCode", "Year"),
    all = TRUE, allow.cartesian = TRUE)
remove0M(treeData, value = "INPUT", flag = "SYMB_INPUT")

## TODO (Michael): Code the input from processing with network
##                 approach as well.
mergedAupus =
    Reduce(f = function(x, y){
        merge(x, y, by = intersect(colnames(x), colnames(y)), all = TRUE,
              allow.cartesian = TRUE)
    }, x = list(rawAupus, ratio))
mergedAupus = merge(mergedAupus, swsItemTable, all.x = TRUE,
    by = "itemCode")
    
calculateInput =
    merge(treeData,
          mergedAupus[, list(areaCode, itemCode, Year, NUM_131)])
inputFromProcess =
    calculateInput[, list(INPUT_131= sum(share * NUM_131/100)),
                   by = c("areaCode", "itemChildCode", "Year")]
setnames(inputFromProcess, "itemChildCode", "itemCode")

finalAupus = merge(mergedAupus, inputFromProcess, all.x = TRUE,
                   by = c("areaCode", "itemCode", "Year"))

calculateEle21 = function(element21Num, element11Num, data){
    data[itemCode == 1, element21Num := element11Num]
    ## NOTE (Michael): I think this means that the calculation is done
    ##                 in the next year if seeding rates are
    ##                 applicable.
}






calculateEle111 = function(ratio171Num, ratio111Num, element111Num,
    stotal, data){
    setnames(data,
             old = c(ratio171Num, ratio111Num, element111Num, stotal),
             new = c("ratio171Num", "ratio111Num", "element111Num",
                 "stotal"))
    ## In this case it's the same to calculateEle101
    data[is.na(ratio171Num) & !is.na(ratio111Num),
         element111Num := ratio111Num * stotal/100]
    ## NOTE (Michael): How do you define a 'valid value'?
    if(!missing(t1)){
        tmp = c(ele21t1, ele31t1, ele21t0, ele31t0) * ratio171/1000
        ele111 = tmp[isvalid(tmp)][1]
    }
    setnames(data,
             new = c(ratio171Num, ratio111Num, element111Num, stotal),
             old = c("ratio171Num", "ratio111Num", "element111Num",
                 "stotal"))    
}    



balance = function(){
    if(item in c(04, 15, 16, 20, 21, 25, 32, 33, 37, 49, 50, 55, 56))
        break
    supply = c(51, 58, 61, 66)
    utilization = c(91, 95, 96, 101, 111, 121, 131, 141, 161, 546)
    if(item in 51, 58, 59, 61){
        supply = c(supply, 11)
    } else {
        utilization = c(utilization, 11)
    }

    if(item in WEIS){
        supply = c(supply, 53)
    } else {
        utilization = c(utilization, 53)
    }
    balance = sum(supply) - sum(utilization)

    if(item in 57){
        tmp = (ele161/ele171) * 1000
        if(is.finite(tmp)){
            balance = tmp
        } else {
            balance = 0
        }
    }

    if(balele == 71)
        balance = -balance

    if(balance < 0){
        if(area != 10 & itm != 3904){
            ele181 = balance
            balance = 0
        }
    }

    
    if(!missing(balele)){
        assign(balance)
    } else {
        warning("Value entered for an element that is balance")
        ele181 = ele181 + balance
    }
}

    
## Things to Note:
## Need to write a function to check whether a cell is not null or zero.
##
## Need to check whether zero or na is replaced.
##
## Need to check how the symbols are applied. When we assign a value
## of zero, do we assign the symbol 'M' or 'C'.
##
##
             
