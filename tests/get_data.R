# Load the libraries
library(reshape2)
library(RJDBC)
library(data.table)
options(java.parameters = "-Xmx3000m")
lapply(dir("../codes/", full.names = TRUE), FUN = source)

## Connect to the database
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "~/ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")

## Get item information
swsItemTable = data.table(dbGetQuery(conn,
    "SELECT item, name_e, item_typ FROM item"))
setnames(swsItemTable,
         old = c("ITEM", "NAME_E", "ITEM_TYP"),
         new = c("itemCode", "itemName", "itemType"))
setkeyv(swsItemTable, "itemCode")
save(swsItemTable, file = "swsItemTable.RData")

## Initialization
testCountryCode = 100
testItemCode = swsItemTable[swsItemTable$GRP_IND == "D", "ITEM"]
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)

## Load aupus data
aupus = getAupusData(testCountryCode,  conn)
save(aupus, file = "aupusData.RData")

## Get input data
input = getInputFromProcess(testCountryCode, conn)
save(input, file = "input.RData")

## Get ratio data
ratio = getRatio(testCountryCode, conn)
save(ratio, file = "ratio.RData")

## Get share data
share.lst = getShare(testCountryCode, conn)
share = mergeShare(share.lst, aupus = aupus)
save(share, file = "share.RData")

## Get balancing element
balanceElement = getBalanceElement(testCountryCode, conn)
save(balanceElement, file = "balanceElement.RData")
