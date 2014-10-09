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

calculateEle41 = function(ratio41Num, 
    element41Num, element41Symb, data){
    setnames(data,
             old = c(ratio41Num, element41Num, element41Symb),
             new = c("ratio41Num", "element41Num", "element41Symb"))
    ## Do the new calculation
    newCalculation = data[, ratio41Num] * 100

    ## if new calculation is not possible, then set as zero
    newCalculation[is.na(newCalculation)] = 0
    
    ## Find the index for which the values were previously calculated
    previousCalculation =
        is.calculated(data[, element41Symb])
    
    ## Replace data which were previously calculated.
    data[previousCalculation,
         element41Num := newCalculation[previousCalculation]]
    setnames(data,
             new = c(ratio41Num, element41Num, element41Symb),
             old = c("ratio41Num", "element41Num", "element41Symb"))    
}

## Function to find the number of elements which are missing from
## multiple elements.
numberOfMissingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(is.na(x))
                                     }
                   )
            )
}


## Function to find the number of elements which are trended from
## multiple elements.
numberOfTrendingElement = function(...){
    listOfElements = list(...)
    rowSums(sapply(listOfElements,
                   FUN = function(x){
                       as.numeric(x == "T")
                                     }
                   )
            )
}

## Function to balance element 31, 41, 51 after each has been
## calculated/updated.
calculateEle314151 = function(element31Num, element41Num, element51Num,
    element31Symb, element41Symb, element51Symb, data){
    setnames(data,
             old = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             new = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))

    ## Calculate condition statistics
    data[, numberOfMissingElements :=
             numberOfMissingElement(element31Num, element41Num,
                                    element51Num)]
    data[, numberOfTrendingElements :=
             numberOfTrendingElement(element31Symb, element41Symb,
                                     element51Symb)]    

    ## Start the balancing if there is only one missing value
    data[is.na(element31Num) & numeberOfMissingElements == 1,
         element31Num := element51Num/element41Num]
    data[is.na(element41Num) & numeberOfMissingElements == 1,
         element41Num := element51Num/element31Num]
    data[is.na(element51Num) & numeberOfMissingElements == 1,
         element51Num := element31Num * element41Num]

    ## Recalculate the trend if there is only one trending value
    trendOnce = function(Num, numberOfTrendingElemets){
        trendeIndex = which(numberOfTrendingElemetns == 1) + 1
        tmp = c(NA, Num)
        newTrendIndex = intersect(trendIndex, which(is.na(tmp)))
        tmp[newTrendIndex] = tmp[newTrendIndex - 1]
        trendedOnce = tmp[-1]
        trendedOnce
    }
    data[, elemeent31Num :=
             trendOnce(element31Num, numberOfTrendingElemetns),
         by = c("itemCode", "Year")]
    data[, elemeent41Num :=
             trendOnce(element41Num, numberOfTrendingElemetns),
         by = c("itemCode", "Year")]
    data[, elemeent51Num :=
             trendOnce(element51Num, numberOfTrendingElemetns),
         by = c("itemCode", "Year")]
    data[, `:=`(c(numberOfMissingElements, numberOfTrendingElements),
                NULL)]
    
    setnames(data,
             new = c(element31Num, element41Num, element51Num,
                 element31Symb, element41Symb, element51Symb),
             old = c("element31Num", "element41Num", "element51Num",
                 "element31Symb", "element41Symb", "element51Symb"))

    ## NOTE (Michael): For the case which trend sequentially, does the
    ##                 algorithm trend then balance?
}


calculateEle63 = function(element61Num, element62Num,
    element63Num, data){
    setnames(data,
             old = c(element61Num, element62Num, element63Num),
             new = c("element61Num", "element62Num", "element63Num"))
    ## Calculate element 63 from element 61 and 62 if both are
    ## available.
    data[!is.na(element61Num) & !is.na(element62Num),
         element63Num := element61Num * 1000/element62Num]
    ## If any one of them is missing, then the new calculatino would
    ## be missing. Therefore, replace with zero.
    data[is.na(element61Num) | is.na(element62Num),
         element63Num := 0]
    setnames(data,
             new = c(element61Num, element62Num, element63Num),
             old = c("element61Num", "element62Num", "element63Num"))    
}




## This is the reverse of the standardization
calculateEle66 = function(){
    if(item != trade)
        break
}


calculateEle71 = function(element71Num, element51Num, element61Num,
    element91Num, element101Num, element121Num, element131Num,
    element141Num, element151Num, element161Num, data){
    setnames(data,
             old = c(element71Num, element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             new = c("element71Num", "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num")
    data[itemCode == 58, element71Num := element51Num + element61Num -
             element91Num - element101Num - element121Num -
                 element131Num - element141Num - element151Num]
    data[itemCode %in% c(59, 60, 61),
         element71Num := element161Num - element101Num]
    ## NOTE (Michael): What about element 57?
    setnames(data,
             new = c(element71Num, element51Num, element61Num,
                 element91Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num,
                 element161Num),
             old = c("element71Num", "element51Num", "element61Num",
                 "element91Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num",
                 "element161Num")             
}

calculateEle919293 = function(element91Num, element92Num, element93Num,
    data){
    setnames(data,
             old = c(element91Num, element92Num, element93Num),
             new = c("element91Num", "element92Num", "element93Num"))
    data[!itemCode in c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         !is.na(element91Num) & !is.na(element92Num),
         element93Num := element91Num * 1000/element92Num]
    data[!itemCode in c(42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52) &
         is.na(element91Num) | is.na(element92Num),
         element93Num := 0]
    setnames(data,
             new = c(element91Num, element92Num, element93Num),
             old = c("element91Num", "element92Num", "element93Num"))    
}

## Same as element 66 and is reverse standardization
calculateEle96 = function(){
    if(item != trade)
        break
}


calculateEle101 = function(element101Num, ratio101Num, stotal, data){
    ## Assumes total is calculated already.
    ## NOTE (Michael): how to calculat total supply?
    setnames(data,
             old = c(element101Num, ratio101Num, stotal),
             new = c("element101Num", "ratio101Num", "stotal"))
    data[!is.na(ratio101Num),
         element101Num := ratio101Num * stotal/100]
    setnames(data,
             new = c(element101Num, ratio101Num, stotal),
             old = c("element101Num", "ratio101Num", "stotal"))    
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

calculateEle121 = function(element121Num, ratio121Num, stotal, data){
    setnames(data,
             old = c(element121Num, ratio121Num, stotal),
             new = c("element121Num", "ratio121Num", "stotal"))
    data[, element121Num := ratio121Num * stotal/100]
    setnames(data,
             new = c(element121Num, ratio121Num, stotal),
             old = c("element121Num", "ratio121Num", "stotal"))
    
}

calculateEle131 = function(element131Num, ratio131Num, stotal, data){
    setnames(data,
             old = c(element131Num, ratio131Num, stotal),
             new = c("element131Num", "ratio131Num", "stotal"))    
    data[, element131Num := ratio131Num * stotal/100]
    setnames(data,
             new = c(element131Num, ratio131Num, stotal),
             old = c("element131Num", "ratio131Num", "stotal"))        
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


calculateEle144 = function(element144Num, element141Num, population,
    data){
    setnames(data,
             old = c(element144Num, element141Num, population),
             new = c("element144Num", "element141Num", "population"))
    ## Assumes total consumption (element141Num) has already been
    ## calculated.
    data[itemCode %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61),
         element144Num = element141Num/population * 1000]
    data[!itemCode %in% c(46, 47, 48, 51, 52, 58, 59, 60, 61),
         element144Num = element141Num/population]
    setnames(data,
             new = c(element144Num, element141Num, population),
             old = c("element144Num", "element141Num", "population"))    
}


calculateEle151 = function(element151Num, element131Num, element51Num,
    ratio151Num, stotal, data){
    setnames(data,
             old = c(element151Num, element131Num, element51Num,
                 ratio151Num, stotal),
             new = c("element151Num", "element131Num", "element51Num",
                 "ratio151Num", "stotal"))
    data[itemCode != 1697, element151Num := ratio151Num * stotal/100]

    tmp = merge(data[itemCode == 1684,
        list(itemCode, Year, element131Num)],
        data[itemCode == 1687, list(itemCode, Year, element51Num)],
        all = TRUE, by = c("itemCode", "Year"))
    tmp[, element151Calculated := element131Num - element51Num]
    tmp[, `:=`(c(element131Num, element51Num), NULL)]
    tmp[, itemCode:= 1687]
    data = merge(data, tmp, all = TRUE, by = c("itemCode", "Year"))
    data[itemCode == 1687, element151Num := element151Calculated]
    data[, element151Calculated := NULL]
    setnames(data,
             new = c(element151Num, element131Num, element51Num,
                 ratio151Num, stotal),
             old = c("element151Num", "element131Num", "element51Num",
                 "ratio151Num", "stotal"))    
}


calculateEle161 = function(){
    if(item == sugar57){
        ele161 = ele11 + ele71
    } else if(item in trade){
        ## unclear
    }
}

calculateEle171 = function(element171Num, element101Num, element121Num,
    element131Num, element141Num, element151Num, data){
    setnames(data,
             old = c(element171Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num),
             new = c("element171Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num"))
    data[itemCode == 57, element171Num := element101Num +
             element121Num + element131Num + element141Num +
                 element151Num]
    setnames(data,
             old = c(element171Num, element101Num, element121Num,
                 element131Num, element141Num, element151Num),
             new = c("element171Num", "element101Num", "element121Num",
                 "element131Num", "element141Num", "element151Num"))    
}





calculateEle174 = function(element174Num, elemenet171Num, population,
    data){
    setnames(data,
             old = c(element174Num, elemenet171Num, population),
             new = c("element174Num", "elemenet171Num", "population"))
    ## Assumes 171 calculated
    data[itemCode == 57, element174Num := element171Num * population]
    setnames(data,
             new = c(element174Num, elemenet171Num, population),
             old = c("element174Num", "elemenet171Num", "population"))
}


calculateEle261 = function(element261Num, ratio261Num, element141Num,
    data){
    setnames(data,
             old = c(element261Num, ratio261Num, element141Num),
             new = c("element261Num", "ratio261Num", "element141Num"))
    data[, element261Num := ration261Num * element141Num/100]
    setnames(data,
             new = c(element261Num, ratio261Num, element141Num),
             old = c("element261Num", "ratio261Num", "element141Num"))
}


calculateEle264 = function(element21Num, element11Num, element264Num,
    element261Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element264Num,
                 element261Num),
             new = c("element21Num", "element11Num", "element264Num",
                 "element261Num"))
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element264Num := element261Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element264Num,
                 element261Num),
             old = c("element21Num", "element11Num", "element264Num",
                 "element261Num"))
}


calculateEle271 = function(element271Num, ratio271Num, element141Num,
    data){
    setnames(data,
             old = c(element271Num, ratio271Num, element141Num),
             new = c("element271Num", "ratio271Num", "element141Num"))
    data[, element271Num := ration271Num * element141Num/1000]
    setnames(data,
             new = c(element271Num, ratio271Num, element141Num),
             old = c("element271Num", "ratio271Num", "element141Num"))
}



calculateEle274 = function(element21Num, element11Num, element274Num,
    element271Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element274Num,
                 element271Num),
             new = c("element21Num", "element11Num", "element274Num",
                 "element271Num"))
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element274Num := element271Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element274Num,
                 element271Num),
             nold = c("element21Num", "element11Num", "element274Num",
                 "element271Num"))    
}

calculateEle281 = function(element281Num, ratio281Num, element141Num,
    data){
    setnames(data,
             old = c(element281Num, ratio281Num, element141Num),
             new = c("element281Num", "ratio281Num", "element141Num")
    data[, element281Num := ration281Num * element141Num/1000]
    setnames(data,
             new = c(element281Num, ratio281Num, element141Num),
             old = c("element281Num", "ratio281Num", "element141Num")
}


calculateEle284 = function(element21Num, element11Num, element284Num,
    element281Num, data){
    setnames(data,
             old = c(element21Num, element11Num, element284Num,
                 element281Num),
             new = c("element21Num", "element11Num", "element284Num",
                 "element281Num"))       
    data[, validPopulation := element21Num]
    data[is.na(validPopulation), validPopulation := element11Num]
    data[, element284Num := element281Num/365 * 1000/validPopulation]
    setnames(data,
             new = c(element21Num, element11Num, element284Num,
                 element281Num),
             old = c("element21Num", "element11Num", "element284Num",
                 "element281Num"))    
}

calculateEle541 = function(element542Num, element543Num, element544Num,
    element545Num, data){
    setnames(data,
             old = c(element542Num, element543Num, element544Num,
                 element545Num),
             new = c("element542Num", "element543Num", "element544Num",
                 "element545Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element542Num, element543Num,
                                    element544Num, element545Num)]
    data[, element541Num := element542Num + element543Num +
             element544Num + element545Num]
    data[numberOfMissingElements == 0, element541Num := 0]
    data[, numberOfMissingElements := NULL]
    setnames(data,
             new = c(element542Num, element543Num, element544Num,
                 element545Num),
             old = c("element542Num", "element543Num", "element544Num",
                 "element545Num"))    
}

calculateEle546 = function(element541Num, element151Num, element191Num,
    data){
    setnames(data,
             old = c(element541Num, element151Num, element191Num),
             new = c("element541Num", "element151Num", "element191Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element541Num, element151Num,
                                    element191Num)]
    data[, element541Num := element541Num + element151Num +
             element191Num]
    data[numberOfMissingElements == 0, element541Num := 0]
    data[, numberOfMissingElements := NULL]
    setnames(data,
             new = c(element541Num, element151Num, element191Num),
             old = c("element541Num", "element151Num", "element191Num"))
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
             elementNum :=
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
