## Load the libraries
## library(faoswsExtra)
## library(data.table)
library(reshape2)
library(RJDBC)
library(data.table)
library(FAOSTAT)
options(java.parameters = "-Xmx3000m")

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



dbGetQuery(conn, "SELECT COUNT(*) FROM item")

swsItemTable = dbGetQuery(conn, "SELECT * FROM item")
save(swsItemTable, file = "swsItemTable.RData")



## Test of Wheat Germany
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
    print(query)
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
                                  Year ~ elementCode + type,
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
        c("areaCode", "itemParentCode", "itemChildCode")
    melted = melt(tmp,
        id.var = c("areaCode", "itemParentCode",
            "itemChildCode"))
    melted$Year =
        as.numeric(gsub("[^0-9]", "", melted$variable))
    melted$type = gsub("[0-9|_]", "", melted$variable)
    melted$variable = NULL
    casted = data.table(dcast(melted, areaCode + itemParentCode +
                                  itemChildCode +
                                  Year ~ type,
        value.var = "value"))
    valueCol = grep("NUM", colnames(casted), value = TRUE)
    casted[, (valueCol) :=
               lapply(valueCol, function(x) as.numeric(casted[[x]]))]
    setnames(casted, old = grep("NUM", colnames(casted), value = TRUE),
             new = gsub("NUM", "INPUT", 
                 grep("NUM", colnames(casted), value = TRUE)))
    casted
}
input = getInputFromProcess(testCountryCode, testItemCode, conn)
save(input, file = "input.RData")

dbGetQuery(conn, "SELECT * FROM input_from_procv WHERE ROWNUM <= 5")


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
    final[, elementCode := paste0(elementCode, "_RATIO")]
    castedFinal =
        dcast.data.table(final, areaCode + itemCode + Year ~ elementCode,
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
        c("areaCode", "itemParentCode", "itemChildCode", "Year",
          "share", "aupusRequired")
    print(str(base))

    ## year wild card
    wildCardYear = dbGetQuery(conn,
        paste0("SELECT area, item_parent, item_child, aupus_share
                FROM aupus_item_tree_shares
                WHERE area in (", countryCode, ")
                AND yr = 0
                AND item_child in (",
               paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardYear) =
        c("areaCode", "itemParentCode", "itemChildCode",
          "wildCardYearShare")
    expandedWildCardYear =
        as.data.frame(sapply(wildCardYear,
                             function(x){
                                 rep(x, length(years))
                             }
                             )
                      )
    expandedWildCardYear$Year = rep(years, each = NROW(wildCardYear))
    print(str(expandedWildCardYear))
    
    ## global wild card
    wildCardGlobal = dbGetQuery(conn,
        paste0("SELECT item_parent, item_child, aupus_share
                FROM aupus_item_tree_shares
                WHERE area = 0
                AND item_child in (",
                paste0(itemCode, collapse = ", "), ")"))
    colnames(wildCardGlobal) =
        c("itemParentCode", "itemChildCode", "wildCardGlobalShare")
    wildCardGlobal$areaCode = countryCode
    expandedWildCardGlobal =
        as.data.frame(sapply(wildCardGlobal,
                             function(x){
                                 rep(x, length(years))
                                         }
                             )
                      )
    expandedWildCardGlobal$Year = rep(years, each = NROW(wildCardGlobal))
    print(str(expandedWildCardGlobal))
    ## Now merge the whole lot
    final =
        data.table(
            Reduce(function(x, y) merge(x, y, all = TRUE),
                   list(base, expandedWildCardYear,
                        expandedWildCardGlobal))
            )
    print(str(final))
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


setnames(input, "itemChildCode", "itemCode")
setnames(shares, "itemChildCode", "itemCode")

## Need to remove 0M for al the data.
treeData = merge(input, shares,
    by = c("areaCode", "itemCode", "itemParentCode", "Year"),
    all = TRUE, allow.cartesian = TRUE)
treeData[INPUT == 0 & SYMB == "M", INPUT := as.numeric(NA)]

mergedAupus =
    Reduce(f = function(x, y){
        merge(x, y, by = c("areaCode", "itemCode", "Year"), all = TRUE,
              allow.cartesian = TRUE)
    },
           x = list(rawAupus, ratio))

library(faoswsExtra)
calculateInputFromProcessing = function(){
    tmp =
        merge(mergedAupus[, list(areaCode, itemCode, Year, `131_NUM`)],
              treeData[, list(areaCode, itemCode, itemParentCode,
                              Year, INPUT, share)],
              all.y = TRUE, allow.cartesian = TRUE)
    tmp2 =
        tmp[, list(input = sumWithNA(`131_NUM` * share/100)),
            by = c("areaCode", "itemCode", "Year")]
    tmp2
}
          





calculateEle31 = function(){
    if(commodity = processed){
        if(missing(ifp)){
            ifp = sum(ele131 * share/100, na.rm = TRUE)
        } else {
            ifp = ifp
        }
    }

    if(!manual(ele31)){
        if(!is.na(ifp)){
            ele31 = ifp
        } else {
            ele31 = 0
        }
    }
}


is.calculated = function(symb){
    symb %in% "C"
}

calculateEle41 = function(ratio41Num, ratio41Symb,
    element41Num, element41Symb, data){
    ## Do the new calculation
    newCalculation = data[, ratio41Num, with = FALSE] * 100

    ## if new calculation is not possible, then set as zero
    newCalculation[is.na(newCalculation)] = 0
    ## Find the index for which the values were previously calculated
    previousCalculation =
        is.calculated(data[, element41Symb, with = FALSE])
    ## Replace data which were previously calculated.
    ## TODO: Remove the hard coded names.
    data[previousCalculation,
         `41_NUM` := newCalculation[previousCalculation]]
    data
}


calculateEle314151 = function(){
    if(length(is.na(elements)) > 1){
        ## give warning
    } else if(length(is.na(elements)) == 1){
        ## calculate the missing element
    } else if(length(is.na(elements)) == 0){
        ## Re-calculate trended value (31, 41, 51)
    }
}


calculateEle63 = function(element61Num, element62Num,
    element63Num, data){
    ## Calculate element 63 from element 61 and 62
    newCalculation = data[, element61Num, with = FALSE] *
        1000/data[, element62Num, with = FALSE]
    ## If any one of them is missing, then the new calculatino would
    ## be missing. Therefore, replace with zero.
    newCalculation[is.na(newCalculation)] = 0
    ## asssign the calculation.
    data[, element63Num := newCalculation]
}




## This is the reverse of the standardization
calculateEle66 = function(){
    if(item != trade)
        break
}


calculateEle71 = function(){
    if(item != ESCR + stimulants such as coffee)
        break
    if(item in ESCR){
        ele71 = ele51 + ele61 - ele91 - ele101 - ele121 -
            ele131 - ele141 - ele151
    } else {
        ele71 = ele161 - ele101
    }
}

calculateEle919293 = function(){
    if(item in c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52))
        break

    if(!is.na(ele91) & !is.na(ele92)){
        ele93 = ele92 * 1000/elel91
    } else {
        ele93 = 0
    }
}

## Same as element 66 and is reverse standardization
calculateEle96 = function(){
    if(item != trade)
        break
}




calculateEle101 = function(){
    ## assumes total is calculated already
    if(!missing(ratio)){
        ele101 = ratio101 * stotal/100
    }
}

calculateEle111 = function(){
    if(missing(ratio171)){
        if(!missing(ratio111){
            calculateEle101()
        }
       } else if(!missing(t1)){
           tmp = c(ele21t1, ele31t1, ele21t0, ele31t0) * ratio171/1000
           ele111 = tmp[isvalid(tmp)][1]
       }
    }
}

calculateEle121 = function(){
    ele121 = ratio121 * stotal/100
}

calculateEle131 = function(){
    ele131 = ratio131 * stotal/100
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

calculateEle144 = function(){
    ## total consumption has been calculated
    if(item in ESCR){
        ele144 = ele141/population * 1000
    } else {
        ele144 = ele141/population
    }
}

calculateEle151 = function(){
    if(item != 1687){
        ele151 = ratio151 * totals/100
    } else {
        ele151 = ele131i1684 - ele51i1687
    }
}

calculateEle161 = function(){
    if(item == sugar57){
        ele161 = ele11 + ele71
    } else if(item in trade){
        ## unclear
    }
}
        
calculateEle171 = function(){
    if(item == 57)
        ele171 = ele101 + ele121 + ele131 + ele141 + ele151
}



calculateEle174 = function(){
    ## Assumes 171 calculated
    if(item == 57)
        ele174 = ele171 * population
}

calculateEle181 = function(){
    ## Only in balance
}

calculateEle261 = function(){
    ele261 = ratio261 * ele141/fd
}


calculateEle264 = function(){
    population = na.omit(21, 11)[1]
    ele264 = ((ele261/365) * 1000)/population
}

calculateEle271 = function(){
    ele271 = ratio271 * ele141/fd
}


calculateEle274 = function(){
    population = na.omit(21, 11)[1]
    ele274 = ((ele261/365) * 1000)/population
}


calculateEle281 = function(){
    ele281 = ratio281 * ele141/fd
}


calculateEle284 = function(){
    population = na.omit(21, 11)[1]
    ele284 = ((ele261/365) * 1000)/population
}

calculateEle541 = function(){
    ele541 = sum(ele542, ele543, ele544, ele545, na.rm = TRUE)
}

calculate546 = function(){
    ele546 = sum(ele541, ele151, ele191, na.rm = TRUE)
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


modified.na.locf = function(num, symb, justOnce, ...){
    validNum = num
    validNum[!symb %in% c("T", "C", "M")] = NA
    if(!justOnce){
        trendedNum = na.locf(validNum, na.rm = FALSE, ...)
        trendedNum[!is.na(num)] = num[!is.na(num)]
    } else {
        trendedNum = c(NA, validNum)
        trendedNum[which(is.na(trendedNum))] =
            trendedNum[which(is.na(trendedNum)) - 1]
        trendedNum = trendNum[-1]
    }
    trendedNum
}
    

calculateTrend = function(element, elementNum, elementSymb, data){
    setnames(x = data, old = c(elementNum, elementSymb),
             new = c("elementNum", "elementSymb"))
    if(!element %in% c(31, 41, 51)){
        data[itemCode %in% c(0:1299, 1455:1700),
             elementNum :=
                 modified.na.locf(elementNum, elementSymb, FALSE),
             by = c("areaCode", "itemCode")]
    } else if(element %in% c(31, 41, 51)){
        data[itemCode %in% c(0:1299, 1455:1700),
             elementNum : =
                 modified.na.locf(elementNum, elementSymb, TRUE),
             by = c("areaCode", "itemCode")]
    } else if(element == 71){
        data[itemCode %in% c(12, 13),
             elementNum :=
                 modified.na.locf(elementNum, elementSymb, FALSE),
             by = c("areaCode", "itemCode")]        
    }
    setnames(x = data, new = c(elementNum, elementSymb),
             old = c("elementNum", "elementSymb"))
}
        
        
        
        



    
## Things to Note:
## Need to write a function to check whether a cell is not null or zero.
##
## Need to check whether zero or na is replaced.
