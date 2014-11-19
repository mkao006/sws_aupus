## Load libraries
## testCountryCode = 100
library(faoswsAupus)
library(faoswsUtil)
library(data.table)
library(igraph)
    
source("test_get_api_data.R")
missingColumns =
    c(paste0("Value_measuredElementFS_", c(541, 546)),
      paste0("flagFaostat_measuredElementFS_", c(541, 546)))

aupusData[, `:=`(c(missingColumns),
                 list(as.numeric(NA), as.numeric(NA), as.character(NA),
                      as.character(NA)))]




aupusFinal =
    mergeAll(aupusData = aupusData, itemInfoData = itemInfoData,
             balanceElementData = balanceElementData,
             shareData = shareData,
             inputData = inputData, param = param,
             inputNum = "Value_input",
             balanceElementNum = "balanceElement",
             shares = "Value_share",
             element131Num = "Value_measuredElementFS_131")

## ## Replace Num col with value equivalent to its column
## lapply(grep("NUM|RATIO", colnames(aupusSim), value = TRUE),
##        FUN = function(x){
##            num = as.numeric(gsub("[^0-9]", "", x))
##            if(is.na(num))
##                num = 100
##            aupusSim[, `:=`(c(x), num)]
##        }
## )

## ## resample symbol to replace missing flag
## resampleSymb = function(symbs){
##     print(str(symbs))
##     if(!all(is.na(symbs))){
##         missIndex = which(is.na(symbs))
##         symbs[is.na(symbs)] =
##             sample(unique(na.omit(symbs)), length(missIndex),
##                    replace = TRUE)
##     } else {
##         symbs = rep("C", length(symbs))
##     }
##     symbs
## }

## lapply(grep("SYMB", colnames(aupusSim), value = TRUE),
##        FUN = function(x){
##            aupusSim[, `:=`(c(x),
##                            resampleSymb(unlist(aupusSim[, x, with = FALSE])))]
##        })

## aupusSim[is.na(balanceElement),
##          balanceElement := unique(na.omit(aupusSim$balanceElement))]

## aupusCopy = copy(aupusSim)

## compareFunction = function(replaceIndex, elementNum){
##     tmp =
##         cbind(aupusFinal[replaceIndex,
##                        c("areaCode", "itemCode", "itemType", "Year",
##                          paste0(c("NUM_", "SYMB_"), elementNum)),
##                        with = FALSE],
##               aupusCopy[replaceIndex,
##                        paste0(c("NUM_", "SYMB_"), elementNum),
##                        with = FALSE])
##     print(tmp, nrow = NROW(tmp))
## }

## compareFunction = function(replaceIndex, elementNum){
##     print(paste0("PASS", elementNum))
## }







Aupus(aupusFinalData = aupusFinal, shareData = shareData,
      itemTypeCol = "measuredItemTypeFS", shareNum = "Value_share",
      inputNum = "Value_input", balanceElementNum = "balanceElement")




## Testing of the network standardization module
load(".RData")
load("share.RData")
setnames(share, colnames(share), colnames(shareData))
share[, `:=`(c("measuredItemParentFS", "measuredItemChildFS",
               "geographicAreaFS"),
             list(as.character(measuredItemParentFS),
                  as.character(measuredItemChildFS),
                  as.character(geographicAreaFS)))]
shareDataOld = copy(share)
setkeyv(shareDataOld, c("geographicAreaFS", "measuredItemParentFS",
                        "measuredItemChildFS", "timePointYearsSP"))

test.graph =
    constructGraph(shareData = shareDataOld[timePointYearsSP == 2005, ],
                   aupus = aupusData[timePointYearsSP == 2005, ],
                   param = param, 
                   shares = "Value_share",
                   extractionRate = "Value_measuredElementFS_41",
                   standardizeElement = "Value_measuredElementFS_61",
                   plot = TRUE)



## The calculation looks fine, but write it on paper and also ask nick
## to provide the control files.
connected.graph = findConnectedGraph(test.graph, c("15"))
connected.graph = connected.graph - V(connected.graph)["24"] - V(connected.graph)["17"] 
plot(connected.graph, vertex.size = 8, edge.arrow.size = 0.5,
     vertex.label = paste0(V(connected.graph)$name, "\n(",
         V(connected.graph)$standardizeElement, ")"),
     edge.label =
         paste0(round(10000/E(connected.graph)$extractionRate, 2), "\n",
                round(100/E(connected.graph)$shares, 2)))
standardizeStep1 = standardizeNode(connected.graph,
    names(which(degree(connected.graph, mode = "in") == 0 &
                    degree(connected.graph, mode = "out") > 0)))
with(standardizeStep1, 
plot(standardizedGraph, vertex.size = 8, edge.arrow.size = 0.5,
     vertex.label = paste0(V(standardizedGraph)$name, "\n(",
         V(standardizedGraph)$standardizeElement, ")"),
     edge.label = round(1000000/(E(standardizedGraph)$extractionRate * 
                         E(standardizedGraph)$shares), 2)))
standardizeStep2 = standardizeNode(standardizeStep1$standardizedGraph,
    names(which(degree(standardizeStep1$standardizedGraph, mode = "in") == 0 &
                    degree(standardizeStep1$standardizedGraph, mode = "out") > 0)))
V(standardizeStep2$standardizedGraph)$standardizeElement



element131.dt = aupusData[, c(key(aupusData), 
    "Value_measuredElementFS_131"), with = FALSE]
setnames(element131.dt, "measuredItemFS", "measuredItemParentFS")
setkeyv(element131.dt, c("geographicAreaFS", "measuredItemParentFS",
                         "timePointYearsSP"))

inputWith131.dt = merge(inputData, element131.dt,
    by = key(element131.dt), all.x = TRUE)
setkeyv(inputWith131.dt, key(inputData))

load("share.RData")
setnames(share, colnames(share), colnames(shareData))
share[, `:=`(c("measuredItemParentFS", "measuredItemChildFS",
               "geographicAreaFS"),
             list(as.character(measuredItemParentFS),
                  as.character(measuredItemChildFS),
                  as.character(geographicAreaFS)))]
shareDataOld = copy(share)
setkeyv(shareDataOld, c("geographicAreaFS", "measuredItemParentFS",
                        "measuredItemChildFS", "timePointYearsSP"))

inputFinal = merge(inputWith131.dt, shareDataOld,
    by = key(inputWith131.dt), all = TRUE)
inputCheck[, list(Check_input = Value_share/100 *
                      Value_measuredElementFS_131)]

## Take year 2005
e = inputFinal[timePointYearsSP == 2005,
    list(measuredItemParentFS, measureItemChildFS, Value_share)]

## Element 131 belongs to parent and value_input belongs to child
##
## NOTE (Michael): There would be a problem if the child has value
##                 much greater than parent.
vp = inputFinal[timePointYearsSP == 2005,
    list(measuredItemParentFS, Value_measuredElementFS_131)]
vc = inputFinal[timePointYearsSP == 2005,
    list(measuredItemChildFS, Value_input)]





## Check the flags
sapply(grep("flagFaostat", colnames(aupusData), value = TRUE),
       FUN = function(x) table(aupusData[, x, with = FALSE]))



## This function should be done by year.
checkShareUnity = function(shareData, param){
    checkShare =
        shareData[, list(sum_check = sum(Value_share)),
                  by = c(param$keyNames$countryName,
                      param$keyNames$itemChildName,
                      param$keyNames$yearName)]
    checkShare[sum_check != 100, ]
}

checkShareUnity(shareDataOld, param)

aupus2005 = aupusData[timePointYearsSP == 2005, ]
share2005 = shareDataOld[timePointYearsSP == 2005, ]
input2005 = inputData[timePointYearsSP == 2005, ]
aupusEdges =
    buildEdges(aupusData = aupus2005,
               extractionRate = "Value_measuredElementFS_41",
               shareData = share2005,
               inputData = input2005,
               param = param)

aupusNodes =
    buildNodes(aupusData = aupus2005, ratioData = ratioData,
               balanceElementData = balanceElementData,
               itemInfoData = itemInfoData,
               balanceElementNum = "balanceElement")


processDownLevel =
    findProcessingLevel(aupusEdges,
                        from = "measuredItemParentFS",
                        to = "measuredItemChildFS",
                        plot = TRUE)

matchProcessingLevel = function(processingLevel, item){
    matchedLevel = processingLevel[match(item, names(processingLevel))]
    ## If it is not in the edge, then it is an item which has no
    ## relationship with others and can be processed immediately.
    matchedLevel[is.na(matchedLevel)] = 0
    matchedLevel
}


aupusNodes[, processingLevel :=
               matchProcessingLevel(processDownLevel, measuredItemFS)]



## These 3 elements need to be propagated at every step.
##
## The share data in the Aupus is only for element 66 and 96, thus we
## can take it out with element66 and 96 as standalone module.
##
## The input is only used by element 31 and updating input from processing.
##



             
    
             

for(i in range(aupusNodes$processingLevel)){
    ## And also the population

    ## NOTE (Michael): Maybe work on the whole aupus node, but the
    ##                 calculation of all other element should be done
    ##                 at processing level. So the updateEdges and
    ##                 calculation of element 66 and 96 should be part
    ##                 of the Aupus.
    
    ## Step (1): Run the aupus module at the primary level on the nodes
    Aupus(aupusFinalData = aupusNodes[processingLevel == i, ],
          shareData = share2005,
          itemTypeCol = "measuredItemTypeFS",
          balanceElementNum = "balanceElement")
    
    ## Step (2): Update the edges (extraction rate and input from processing)
    updateEdges(nodes = aupusNodes[processingLevel == i, ],
                edges = aupusEdges,
                element41Num = "Value_measuredElementFS_41",
                element131Num = "Value_measuredElementFS_131",
                param = param)

    ## Step (3): Propagate input from processing to the node
    updateInputFromProcessing(nodes = aupusNodes,
                              edges = aupusEdges,
                              param = param,
                              element31Num = "Value_measuredElementFS_31")    
}




    
## NOTE (Michael): Looks like the tree is coded in the shares table,
##                 and the collapseShare or getShare function is
##                 losing some of the information.
##
## TODO (Michael): Need to get items which AUPUS applies
##
## NOTE (Michael): To make everything clean and understandable, we
##                 only replicate those that are essential and well
##                 understood.
##
## NOTE (Michael): Do not make modification to the get data related
##                 functions unless they can be tested.
##
## NOTE (Michael): Maybe get population and process separately as
##                 before.
##
## NOTE (Michael): Remove population data from getAupusData
##
## NOTE (Michael): The propagation of element 66 and 96 should be dont
##                 within Aupus after the update of element 41 and
##                 before the calculation of total supply.
