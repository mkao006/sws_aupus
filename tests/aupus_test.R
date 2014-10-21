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
load("input.RData")
load("ratio.RData")
load("share.RData")
load("balanceElement.RData")

aupus = merge(aupus, swsItemTable, all.x = TRUE)
setkeyv(aupus, c("areaCode", "itemCode", "Year"))

## Need to remove 0M for all the data.
treeData = merge(input, shares,
    by = c("areaCode", "itemCode", "itemChildCode", "Year"),
    all = TRUE, allow.cartesian = TRUE)


wildCardFill = function(originalData, wildCardData, variable,
                        verbose = FALSE){
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",
            sum(is.na(tmp[, variable, with = FALSE])),        
            "\n")
    evalText = paste0(variable, " := i.", variable)
    index = unique(wildCardData[originalData[is.na(get(variable)),
        key(wildCardData), with = FALSE], ][!is.na(get(variable)), ])
    setkeyv(index, key(wildCardData))
    okey = key(originalData)
    setkeyv(originalData, key(index))
    originalData[index[!is.na(get(variable)), 
                       c(key(index), variable),
                       with = FALSE],
                 eval(parse(text = evalText))]
    setkeyv(originalData, okey)
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",    
            sum(is.na(tmp[, variable, with = FALSE])),
            "\n")
}

appendRatio = function(aupus, ratio){
    base = merge(aupus, ratio[[1]], all.x = TRUE)
    ## Fill in wild card
    for(i in 2:length(ratio)){
        lapply(grep("RATIO", colnames(ratio[[i]]), value = TRUE),
               FUN = function(x) wildCardFill(base, ratio[[i]], x))
    }
    base
}


########################################################################
## Double Check this section.

availableToInput = function(available, share){
    inputShare = Reduce(f = function(x, y){
        merge(x, y, all = TRUE, allow.cartesian = TRUE,
              by = intersect(colnames(x), colnames(y)))
    }, x = share, init = available)
    setnames(inputShare,
             old = c("itemCode", "itemChildCode"),
             new = c("itemParentCode", "itemCode"))
    setkeyv(x = inputShare,
            cols = c("areaCode", "itemParentCode", "itemCode", "Year"))
    inputShare[!is.na(itemCode) & !is.na(Year), ]
}
available =
    availableToInput(aupus[, list(areaCode, itemCode, Year, NUM_131)],
                     share)
setkeyv(input, key(available))

final = merge(available, input, all = TRUE)
remove0M(final, "NUM_INPUT", "SYMB_INPUT")
sum(is.na(final$NUM_INPUT))
final[is.na(NUM_INPUT), NUM_INPUT := NUM_131 * SHARE/100]
sum(is.na(final$NUM_INPUT))
toInput = final[, list(NUM_TOTAL_INPUT = sum(NUM_INPUT)),
    by = c("areaCode", "itemCode", "Year")]

########################################################################

aupusRatio = appendRatio(aupus, ratio)

length(is.na(aupusRatio$NUM_11))
length(is.na(aupusRatio$NUM_161))
calculateEle11(element11Num = "NUM_11", element11Symb = "SYMB_11",
               element161Num = "NUM_161", data = aupusRatio)
length(is.na(aupusRatio$NUM_11))
length(is.na(aupusRatio$NUM_161))

length(is.na(aupusRatio$NUM_31))
calculateEle31(element31Num = "NUM_31", element31Symb = "SYMB_31",
               input131Num = "INPUT_131", data = aupusRatio)
length(is.na(aupusRatio$NUM_31))






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
             
