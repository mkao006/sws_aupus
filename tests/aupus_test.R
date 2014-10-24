# Load the libraries
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
setkeyv(swsItemTable, "itemCode")
save(swsItemTable, file = "swsItemTable.RData")

## Test of Germany
testCountryCode = 79
testItemCode = swsItemTable[swsItemTable$GRP_IND == "D", "ITEM"]
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)

## Load aupus data
aupus = getAupusData(testCountryCode,  conn)
for(i in grep("NUM", colnames(aupus), value = TRUE)){
    remove0M(data = aupus, value = i, flag = gsub("NUM", "SYMB", i),
             naFlag = "M")
}
save(aupus, file = "aupusData.RData")

population = getPopulation(testCountryCode, conn)
save(population, file = "population.RData")

## Get input data
input = getInputFromProcess(testCountryCode, conn)
remove0M(input, value = "NUM_INPUT", flag = "SYMB_INPUT")
save(input, file = "input.RData")

## Get ratio data
ratio = getRatio(testCountryCode, conn)
save(ratio, file = "ratio.RData")
share = getShare(testCountryCode, conn)
save(share, file = "share.RData")
balanceElement = getBalanceElement(testCountryCode, conn)
save(balanceElement, file = "balanceElement.RData")

## Should also merge the input, by we will not do this for now for the
## primary commodity.
load("swsItemTable.RData")
load("aupusData.RData")
load("population.RData")
load("input.RData")
load("ratio.RData")
load("share.RData")
load("balanceElement.RData")

aupus = merge(aupus, swsItemTable, all.x = TRUE)
setkeyv(aupus, c("areaCode", "itemCode", "Year"))

aupusRatio = appendRatio(aupus, ratio)
updatedInput =
    updateInputFromProcess(aupus = aupus, share = share, input = input,
                           element131Num = "NUM_131")
aggregatedInput = calculateTotalInput(updatedInput)
aupusFinal = merge(aupusRatio, aggregatedInput, all.x = TRUE)
setkeyv(aupusFinal, c("areaCode", "itemCode", "Year"))
## aupusRatioInput = merge(aupusRatio, aggregatedInput, all.x = TRUE)
## aupusFinal = merge(aupusRatioInput, population, all.x = TRUE)
## setkeyv(aupusFinal, c("areaCode", "itemCode", "Year"))

## Element 11
calculateEle11(element11Num = "NUM_11", element11Symb = "SYMB_11",
               element161Num = "NUM_161", data = aupusFinal)

## Element 21
calculateEle21(element21Num = "NUM_21", element21Symb = "SYMB_21",
               element11Num = "NUM_11", element111Num = "NUM_111",
               ratio171Num = "RATIO_171", data = aupusFinal)

## Denormalize population
denormalizePopulation(elementNum11 = "NUM_11", elementNum21 = "NUM_21",
                      data = aupusFinal)


## Element 31
calculateEle31(element31Num = "NUM_31", element31Symb = "SYMB_31",
               inputNum = "NUM_TOTAL_INPUT", data = aupusFinal)

## Element 41
calculateEle41(ratio41Num = "RATIO_41",
               element41Num = "NUM_41", element41Symb = "SYMB_41",
               data = aupusFinal)

## Element 51
calculateEle51(element51Num = "NUM_51", element51Symb = "SYMB_51",
               element58Num = "NUM_58", data = aupusFinal)

## Element 58
calculateEle58(element58Num = "NUM_58", element58Symb = "SYMB_58",
               data = aupusFinal)


## Element 31, 41, 51 balance
calculateEle314151(element31Num = "NUM_31", element31Symb = "SYMB_31",
                   element41Num = "NUM_41", element41Symb = "SYMB_41",
                   element51Num = "NUM_51", element51Symb = "SYMB_51",
                   data = aupusFinal)

## Element 61, 62, 63
calculateEle63(element61Num = "NUM_61",  element62Num = "NUM_62", 
               element63Num = "NUM_63", element63Symb = "SYMB_63",
               data = aupusFinal)

## Element 66

## Element71

calculateEle71(element71Num = "NUM_71", element71Symb = "SYMB_71",
               element51Num = "NUM_51", element61Num = "NUM_61",
               element91Num = "NUM_91", element101Num = "NUM_101",
               element121Num = "NUM_121", element131Num = "NUM_131",
               element141Num = "NUM_141", element151Num = "NUM_151",
               element161Num = "NUM_161", data = aupusFinal)


## Element 91, 92, 93
calculateEle93(element91Num = "NUM_91", element92Num = "NUM_92",
               element93Num = "NUM_93", element93Symb = "SYMB_93",
               data = aupusFinal)


## Calculate total supply
calculateTotalSupply(element11Num = "NUM_11", element51Num = "NUM_51",
                     element58Num = "NUM_58", element61Num = "NUM_61",
                     element66Num = "NUM_66", data = aupusFinal)

## Elemet 101
calculateEle101(element101Num = "NUM_101", element101Symb = "SYMB_101",
                ratio101Num = "RATIO_101", stotal = "TOTAL_SUPPLY",
                data = aupusFinal)

## Element 111
calculateEle111(element111Num = "NUM_111", element111Symb = "SYMB_111",
                element21Num = "NUM_21", element31Num = "NUM_31",
                ratio171Num = "RATIO_171", ratio111Num = "RATIO_111",
                stotal = "TOTAL_SUPPLY", data = aupusFinal)


## Element 121
calculateEle121(element121Num = "NUM_121", element121Symb = "SYMB_121",
                ratio121Num = "RATIO_121", stotal = "TOTAL_SUPPLY",
                data = aupusFinal)

## Element 131
calculateEle131(element131Num = "NUM_131", element131Symb = "SYMB_131",
                ratio131Num = "RATIO_131", stotal = "TOTAL_SUPPLY",
                data = aupusFinal)


## Element 141
calculateEle141(element141Num = "NUM_141", element141Symb = "SYMB_141",
                element11Num = "NUM_11", element51Num = "NUM_51",
                element61Num = "NUM_61", element91Num = "NUM_91",
                element95Num = "NUM_95", element161Num = "NUM_161",
                ratio141Num = "RATIO_141", stotal = "TOTAL_SUPPLY",
                data = aupusFinal)

## Element 144
calculateEle144(element144Num = "NUM_144", element144Symb = "SYMB_144",
                element141Num = "NUM_141", population = "NUM_POP11",
                data = aupusFinal)

## Element 151
calculateEle151(element151Num = "NUM_151", element151Symb = "SYMB_151",
                element131Num = "NUM_131", element51Num = "NUM_51",
                ratio151Num = "RATIO_151", stotal = "TOTAL_SUPPLY",
                data = aupusFinal)

## Element 161
calculateEle161(element161Num = "NUM_161", element161Symb = "SYMB_161",
                element11Num = "NUM_11", element71Num = "NUM_71",
                data = aupusFinal)

## Element 171
calculateEle171(element171Num = "NUM_161", element171Symb = "SYMB_171",
                element101Num = "NUM_101", element121Num = "NUM_121",
                element131Num = "NUM_131", element141Num = "NUM_141",
                element151Num = "NUM_151", data = aupusFinal)


## Element 174
calculateEle174(element174Num = "NUM_174", element174Symb = "SYMB_174",
                element171Num = "NUM_171", population = "NUM_POP11",
                data = aupusFinal)


## Element 261
calculateEle261(element261Num = "NUM_261", element261Symb = "SYMB_261",
                ratio261Num = "RATIO_261", element141Num = "NUM_141",
                data = aupusFinal)

## Element 264
calculateEle264(element264Num = "NUM_264", element264Symb = "SYMB_264",
                element261Num = "NUM_261", population11 = "NUM_POP11",
                population21 = "NUM_POP21", data = aupusFinal)

## Element 271
calculateEle271(element271Num = "NUM_271", element271Symb = "SYMB_271",
                ratio271Num = "RATIO_271", element141Num = "NUM_141",
                data = aupusFinal)

## Element 274
calculateEle274(element274Num = "NUM_274", element274Symb = "SYMB_274",
                element261Num = "NUM_261", population11 = "NUM_POP11",
                population21 = "NUM_POP21", data = aupusFinal)

## Element 281
calculateEle281(element281Num = "NUM_281", element281Symb = "SYMB_281",
                ratio281Num = "RATIO_281", element141Num = "NUM_141",
                data = aupusFinal)

## Element 284
calculateEle284(element284Num = "NUM_284", element284Symb = "SYMB_284",
                element261Num = "NUM_261", population11 = "NUM_POP11",
                population21 = "NUM_POP21", data = aupusFinal)


## Element 541
calculateEle541(element541Num = "NUM_541", element541Symb = "SYMB_541",
                element542Num = "NUM_542", element543Num = "NUM_543",
                element544Num = "NUM_544", element545Num = "NUM_545",
                data = aupusFinal)

## Element 546
calculateEle546(element546Num = "NUM_546", element546Symb = "SYMB_546",
                element541Num = "NUM_541", element151Num = "NUM_151",
                element191Num = "NUM_191", data = aupusFinal)


########################################################################
## Remaining test code to translate
########################################################################

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
             
