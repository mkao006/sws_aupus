## Load the libraries
## library(data.table)
library(igraph)
library(reshape2)
library(RJDBC)
library(data.table)
library(FAOSTAT)
library(faoswsUtil)
options(java.parameters = "-Xmx3000m")
sapply(dir("../codes/", full.names = TRUE), FUN = source)


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
## testItemCode = swsItemTable[swsItemTable$GRP_IND == "D", "ITEM"]
testItemCode = 1:1000
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)

getAupusData = function(countryCode, itemCode, elementCode, conn){
    query = paste0("SELECT * FROM tsv_ics_work_yr
                                  WHERE area = '", countryCode, "'
                                  AND item in (",
                                  paste0(itemCode, collapse = ", "), ")
                                  AND ele in (",
                                  paste0(elementCode, collapse = ", "), ")")
    ## print(query)
    tmp = dbGetQuery(conn, query)
    colnames(tmp)[1:3] =
        c("areaCode", "itemCode", "elementCode")
    melted = melt(tmp,
        id.var = c("areaCode", "itemCode", "elementCode"))
    melted$Year =
        as.numeric(gsub("[^0-9]", "", melted$variable))
    melted$type = gsub("[0-9|_]", "", melted$variable)
    melted$variable = NULL
    casted = data.table(dcast(melted, areaCode + itemCode +
                                  Year ~ type + elementCode,
        value.var = "value"))
    valueCol = grep("NUM", colnames(casted), value = TRUE)
    casted[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(casted[[x]]))]    
    casted
}
rawAupus =
    getAupusData(testCountryCode, testItemCode, aupusElements,
                 conn)
save(rawAupus, file = "aupusData.RData")

## This is not required for primary commodities
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


getRatios = function(countryCode, itemCode, conn){
    ## This is a temporary solution
    years = 1961:2015    
    ## country and year specific
    base = dbGetQuery(conn, paste0("SELECT area, item, ele, yr, ratio
                                   FROM aupus_ratios
                                   WHERE area in (", countryCode, ")
                                   AND item in (",
                                   paste0(itemCode, collapse = ", "), ")
                                   AND yr != 0"))
    colnames(base) =
        c("areaCode", "itemCode", "elementCode", "Year", "ratio")
    ## year wild card
    wildCardYear = dbGetQuery(conn,
        paste0("SELECT area, item, ele, ratio FROM aupus_ratios
                WHERE area in (", countryCode, ")
                AND yr = 0
                AND item in (", paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardYear) =
        c("areaCode", "itemCode", "elementCode", "wildCardYearRatio")
    expandedWildCardYear =
        as.data.frame(sapply(wildCardYear,
                             function(x){
                                 rep(x, length(years))
                             }
                             )
                      )
    expandedWildCardYear$Year = rep(years, each = NROW(wildCardYear))
    ## global wild card
    wildCardGlobal = dbGetQuery(conn,
        paste0("SELECT item, ele, ratio FROM aupus_ratios
                WHERE area = 0 AND item in (",
               paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardGlobal) =
        c("itemCode", "elementCode", "wildCardGlobalRatio")
    wildCardGlobal$areaCode = countryCode
    expandedWildCardGlobal =
        as.data.frame(sapply(wildCardGlobal,
                             function(x){
                                 rep(x, length(years))
                                         }
                             )
                      )
    expandedWildCardGlobal$Year = rep(years, each = NROW(wildCardGlobal))
    ## Now merge the whole lot
    final =
        data.table(
            Reduce(function(x, y) merge(x, y, all = TRUE),
                   list(base, expandedWildCardYear,
                        expandedWildCardGlobal))
            )
    final[is.na(ratio), ratio := wildCardYearRatio]
    final[is.na(ratio), ratio := wildCardGlobalRatio]
    final[, `:=`(c("wildCardYearRatio", "wildCardGlobalRatio"), NULL)]
    final[, elementCode := paste0("RATIO_", elementCode)]
    castedFinal =
        dcast(final, areaCode + itemCode + Year ~ elementCode,
              value.var = "ratio")
    castedFinal
}
ratio = getRatios(testCountryCode, testItemCode, conn)
save(ratio, file = "ratio.RData")


getShare = function(countryCode, itemCode, conn){
    ## This is a temporary solution
    years = 1961:2015
    
    ## country and year specific
    base = dbGetQuery(conn, paste0("SELECT *
                                   FROM aupus_item_tree_shares
                                   WHERE area in (", countryCode, ")
                                   AND item_child in (",
                                   paste0(itemCode, collapse = ", "), ")
                                   AND yr != 0"))
    colnames(base) =
        c("areaCode", "itemCode", "itemChildCode", "Year",
          "share", "aupusRequired")
    ## print(str(base))

    ## year wild card
    wildCardYear = dbGetQuery(conn,
        paste0("SELECT area, item_parent, item_child, aupus_share
                FROM aupus_item_tree_shares
                WHERE area in (", countryCode, ")
                AND yr = 0
                AND item_child in (",
               paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardYear) =
        c("areaCode", "itemCode", "itemChildCode",
          "wildCardYearShare")
    expandedWildCardYear =
        as.data.frame(sapply(wildCardYear,
                             function(x){
                                 rep(x, length(years))
                             }
                             )
                      )
    expandedWildCardYear$Year = rep(years, each = NROW(wildCardYear))
    ## print(str(expandedWildCardYear))
    
    ## global wild card
    wildCardGlobal = dbGetQuery(conn,
        paste0("SELECT item_parent, item_child, aupus_share
                FROM aupus_item_tree_shares
                WHERE area = 0
                AND item_child in (",
                paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardGlobal) =
        c("itemCode", "itemChildCode", "wildCardGlobalShare")
    wildCardGlobal$areaCode = countryCode
    expandedWildCardGlobal =
        as.data.frame(sapply(wildCardGlobal,
                             function(x){
                                 rep(x, length(years))
                                         }
                             )
                      )
    expandedWildCardGlobal$Year = rep(years, each = NROW(wildCardGlobal))
    ## print(str(expandedWildCardGlobal))
    ## Now merge the whole lot
    final =
        data.table(
            Reduce(function(x, y) merge(x, y, all = TRUE),
                   list(base, expandedWildCardYear,
                        expandedWildCardGlobal))
            )
    ## print(str(final))
    final[is.na(share), share := wildCardYearShare]
    final[is.na(share), share := wildCardGlobalShare]
    final[, `:=`(c("wildCardYearShare", "wildCardGlobalShare"), NULL)]
    final
}

shares = getShare(testCountryCode, testItemCode, conn)
save(shares, file = "share.RData")


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
plot(wheatStdStarch.graph,
     vertex.label = paste0(V(wheatStdStarch.graph)$name, "\n",
         V(wheatStdStarch.graph)$standardizeElement),
     vertex.label.cex = 0.6,
     edge.label = paste0(E(wheatStdStarch.graph)$share, "\n",
                         round(E(wheatStdStarch.graph)$extractionRate/
                                   1000)),
     edge.label.cex = 0.6)




calculateEle21 = function(){
    data[itemCode == 1, element21Num := element11Num]    
}


## Need to double check whether the name input131Num is appropriate.
calculateEle31 = function(element31Num, element31Symb, input131Num,
    data){
    setnames(data,
             old = c("element31Num", "element31Symb", "input131Num"),
             new = c(element31Num, element31Symb, input131Num))
    ## NOTE (Michael): Need to find how to identify processed
    ##                 commodity.
    if(commodity == processed){
        data[is.calculatedelement31Symb & !is.na(input131Num),
             element31Num := input131Num]
        data[is.calculatedelement31Symb & is.na(input131Num),
             element31Num := 0]
    }
    setnames(data,
             new = c("element31Num", "element31Symb", "input131Num"),
             old = c(element31Num, element31Symb, input131Num))    
}

system.time(
    calculateEle6696(mergedAupus, shares,
                     "NUM_41", "NUM_61", "NUM_66", "NUM_91", "NUM_96")
)



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


calculateEle141 = function(element11Num, element51Num, element61Num,
    element91Num, element95Num, element141Num, element161Num,
    ratio141Num, stotal, data){
    data[!itemCode %in% c(50, 58, 59, 60, 61),
         element141Num := ratio141Num * stotal/100]
    ## For commodity Jute (50)
    ## Define: calcType
    switch(calcTye,
           `1` = {},
           `2` = {},
           `3` = {}
    )
    data[itemCode %in% c(58:61),
         element141Num := element11Num + element51Num + element61Num -
             element91Num - element95Num - element161Num]
}



calculateEle141 = function(){
    if(item != ESCR){
        ele141 = ratio141 * stotal/100
    } else if(item == jute){
        tp = ele61 - ele91
        if(calcType == 1){
            tr = ele61s - ele91s
            if(is.na(tr)){
                if(!is.na(ifp)){
                    tr = ifp
                } else {
                    tr = 0
                }
            }
            ele141 = tr + (tp * 1.07)
        } else if(calcType == 2){
            ele141 = (ele51 * 1.02) + (tp * 1.07)
        } else if(calcType == 3){
            tr = ele61s - ele91s + ele51s
            if(is.na(tr)){
                if(!is.na(ifp)){
                    tr = ifp
                } else {
                    tr = 0
                }
            }            
            ele141 = tr + (tp * 1.07)
        } else if(calcType == 4){
            rc = ele141s + ele145s
            if(is.na(rc)){
                if(!is.na(ifp)){
                    rc = ifp
                } else {
                    rc = 0
                }
            }
            ele141 = rc + (ele71 * 1.07) + (tp * 1.07)
        }
        if(ele141 < 0){
            ele181 = ele141
            ele141 = 0
        } else {
            ele181 = NA
        }        
    } else if(item == ESCR tea){
        ele141 = ele11 + ele51 + ele61 - ele91 - ele95 - ele161
    }
}


calculateEle161 = function(element161Num, element11Num, element71Num){
    data[itemCode == 57, element161Num := element11Num + element71Num]
    ## NOTE (Michael): Need to clarify how traded item are treated
    data[itemType %in% c(2:13, 19:22, 25:30, 39), ]
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
             
