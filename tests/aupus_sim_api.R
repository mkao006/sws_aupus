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


test = by(aupusData, aupusData$timePointYearsSP, buildEdges, )



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



## This is a temporary solution for converting the population data
## assuming we have used getPopulation
library(faoswsAupus)
library(faoswsUtil)
library(data.table)
library(igraph)

load(".RData")
populationData =
    aupusData[measuredItemFS == '1', c(key(aupusData),
                  "Value_measuredElementFS_11", "Value_measuredElementFS_21"),
              with = FALSE]
setkeyv(populationData, c("geographicAreaFS", "timePointYearsSP"))
## NOTE (Michael): Looks like the key eliminates the duplicates
populationData[, measuredItemFS := NULL]
aupusData = aupusData[measuredItemFS != '1', ]


okey = key(aupusData)
setkeyv(aupusData, key(populationData))
aupusData[populationData, `:=`(c("Value_population_11", "Value_population_21"),
                               list(i.Value_measuredElementFS_11,
                                    i.Value_measuredElementFS_21))]
setkeyv(aupusData, okey)

## This is the temporary solution before we update the share data
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



extractionRateData =
    aupusData[, c(key(aupusData), "Value_measuredElementFS_41"), with = FALSE]
setnames(x = extractionRateData,
         old = c("measuredItemFS", "Value_measuredElementFS_41"),
         new = c("measuredItemChildFS", "Value_extraction"))
setkeyv(extractionRateData,
        cols = c("geographicAreaFS", "measuredItemChildFS", "timePointYearsSP"))


## These are temporary hacks for names before updated the data
param$keyNames = list(areaName = "geographicAreaFS",
             itemName = "measuredItemFS",
             itemParentName = "measuredItemParentFS",
             itemChildName = "measuredItemChildFS",
             itemTypeName = "measuredItemTypeFS",
             elementName = "measuredElementFS",
             extractionRateName = "Value_extractionRate",
             balanceElementName = "Value_balanceElement",
             inputName = "Value_input",
             shareName = "Value_share",
             yearName = "timePointYearsSP",
             valuePrefix = "Value_",
             flagPrefix = "flagFaostat_",
             ratioPrefix = "Ratio_")

lapply(balanceElementData, function(x)
    setnames(x, old = "balanceElement",
             new = param$keyNames$balanceElementName))





foo = function(nodes, edges, ...){


    ## Step (1): Run the aupus module at the primary level on the nodes
    Aupus(aupusFinalData = nodes,
          itemTypeCol = param$keyNames$itemTypeName,
          balanceElementNum = param$keyNames$balanceElementName)

    ## Step (2): Update the edges (extraction rate and input from processing)
    updateEdges(nodes = nodes, 
                edges = edges,
                element41Num = with(param$keyNames,
                    paste0(valuePrefix, elementName, "_41")),
                element131Num = with(param$keyNames,
                    paste0(valuePrefix, elementName, "_131")))

    ## Step (3): Propagate input from processing to the node
    updateInputFromProcessing(nodes = nodes,
                              edges = edges,
                              element31Num = "Value_measuredElementFS_31")
}




foo2 = function(extractionRateData, shareData, inputData,
    ratioData, balanceElementData, itemInfoData, from, to, ...){

    edges =
        buildEdges(extractionRateData = extractionRateData,
                   shareData = shareDataOld,
                   inputData = inputData)

    nodes =
        buildNodes(aupusData = aupusData, ratioData = ratioData,
                   balanceElementData = balanceElementData,
                   itemInfoData = itemInfoData,
                   balanceElementNum = param$keyNames$balanceElementName)

    ## CHECK (Michael): Add in nodes that are contained in the edges
    ##                  list but not found in the node frame. Check
    ##                  why they are missing.

    uniqueEdgeNodes =
        unique(unlist(edges[, c(param$keyNames$itemParentName,
                                param$keyNames$itemChildName), with = FALSE]))

    missingNodes = uniqueEdgeNodes[!uniqueEdgeNodes %in%
        nodes[[param$keyNames$itemName]]]

    missingKeyTable =
        data.table(expand.grid(unlist(unique(nodes[, c(param$keyNames$areaName),
                                                   with = FALSE])),
                               unlist(unique(nodes[, c(param$keyNames$yearName),
                                                   with = FALSE])),
                               missingNodes, stringsAsFactors = FALSE))
    with(param$keyNames,
         setnames(missingKeyTable, c(areaName, yearName, itemName)))
    nodes = rbind(nodes, missingKeyTable, fill = TRUE)
    setkeyv(nodes, key(aupusData))
    
    
    processingLevelData =
        edges[, findProcessingLevel(.SD, from = from, to = to),
                   by = c(param$keyNames$areaName, param$keyNames$yearName)]
    setkeyv(processingLevelData, key(nodes))

    nodes[processingLevelData, processingLevel := i.processingLevel]
    nodes[is.na(processingLevel), processingLevel := as.numeric(0)]
    list(nodes = nodes, edges = edges)
}


check = foo2(extractionRateData = extractionRateData,
    shareData = shareDataOld,
    inputData = inputData,
    ratioData = ratioData,
    balanceElementData = balanceElementData,
    itemInfoData = itemInfoData,
    from = param$keyNames$itemParentName,
    to = param$keyNames$itemChildName)



system.time(
    {
        for(i in range(check$nodes$processingLevel)){
            with(check, foo(nodes = nodes[processingLevel == i, ], edges = edges))
        }
    })
    

constructStandardizationGraph = function(nodes, edges,
    standardizeElement = c("Value_measuredElementFS_61",
        "Value_measuredElementFS_91"), ...){

    nodeCopy = copy(nodes)
    edgeCopy = copy(edges)
    setnames(edgeCopy, "measuredItemParentFS", "measuredItemFS")
    
    uniqueYears = unique(unique(nodeCopy$timePointYearsSP),
        unique(edgeCopy$timePointYearsSP))
    edgeCopy[, `:=`("geographicAreaFS", NULL)]
    edgeCopy[, `:=`("timePointYearsSP", NULL)]
    nodes.lst = split(nodeCopy, uniqueYears)
    edges.lst = split(edgeCopy, uniqueYears)

    print(str(nodes.lst[[1]]))
    print(str(edges.lst[[1]]))

    
    ## edges.lst =
    ##     lapply(edges.lst,
    ##            FUN = function(x){
    ##                print(str(x))
    ##                setnames(x, "measuredItemParentFS", "measuredItemFS")
    ##            }
    ##            )    
    ## currentEdge = edges.lst[[1]]
    ## currentEdge[, geographicAreaFS := NULL]
    ## currentEdge[, timePointYearsSP := NULL]

    nodes.lst =
        lapply(nodes.lst,
               FUN = function(x){
                   x[, c("measuredItemFS", standardizeElement), with = FALSE]
               })
    
    ## currentNode =
    ##     nodes.lst[[1]][, c("measuredItemFS", standardizeElement), with = FALSE]
    
    ## stnd.graph = graph.data.frame(d = currentEdge, vertices = currentNode)
    ## stnd.graph
    graph.lst = mapply(graph.data.frame, d = edges.lst, vertices = nodes.lst,
        SIMPLIFY = FALSE)
    graph.lst
}


allGraph = with(check, constructStandardizationGraph(nodes = nodes, edges = edges))

## Note (Michael): We will just standardize everything anyway.

standardizeNode = function (graph, node, standardizeElement)
{
    outEdges = E(graph)[from(V(graph)[node])]
    shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_share")
    rateMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_extraction")
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    
    valueMatrix =
        matrix(unlist(lapply(X = standardizeElement,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[colnames(shareMatrix)])
                             }
                             )),
               nc = length(standardizeElement))
    
    standardized = reverseMatrix %*% valueMatrix

    targetValueMatrix =
        matrix(unlist(lapply(X = standardizeElement,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[rownames(standardized)])
                             }
                             )),
               nc = length(standardizeElement))
    
    ## Need to convert the na to zeros for addition
    standardizedValues = targetValueMatrix  + standardized

    for(i in 1:NCOL(standardizedValues)){
        set.vertex.attribute(graph = graph, name = standardizeElement[i],
                             index = V(graph)[rownames(standardized)],
                             value = standardizedValues[, i])

        intermediateValuesMatrix =
            matrix(unlist(lapply(X = standardizeElement,
                                 FUN = function(x){
                                     get.vertex.attribute(graph = graph, name = x,
                                                          index = V(graph)[node])
                                 }
                                 )),
                   nc = length(standardizeElement))
        rownames(intermediateValuesMatrix) = node
    }
    graph = graph - vertices(node)
    list(standardizedGraph = graph, intermediateValues = intermediateValuesMatrix)
    
}

stnd.graph = allGraph[[1]]
workingNode =
    names(which(degree(stnd.graph, mode = "in") == 
                    0 & degree(stnd.graph, mode = "out") > 0))
standardize = standardizeNode(graph = stnd.graph, 
    node = workingNode,
    standardizeElement = c("Value_measuredElementFS_61",
        "Value_measuredElementFS_91"))




## NOTE (Michael): This only allows you to standardize one element,
##                 think about how to standardize multiple elements
system.time(
    {
        intermediateStandardization = c()
        while (length(E(stnd.graph)) > 0) {
            workingNode =
                names(which(degree(stnd.graph, mode = "in") == 
                                0 & degree(stnd.graph, mode = "out") > 0))
            standardize = standardizeNode(graph = stnd.graph, 
                node = workingNode,
                standardizeElement = c("Value_measuredElementFS_61",
                    "Value_measuredElementFS_91"))
            stnd.graph = standardize$standardizedGraph
            if (FALSE)
                plot(stnd.graph, vertex.size = 3, edge.arrow.size = 0.5,
                     vertex.label.cex = 0.5)
            ## vertex.label = paste0(V(stnd.graph)$name, "\n(", 
            ##     V(stnd.graph)$standardizeElement, ")"))
            intermediateStandardization = rbind(intermediateStandardization, 
                standardize$intermediateValues)
        }
        terminalValueMatrix =
            matrix(unlist(lapply(X = c("Value_measuredElementFS_61",
                                     "Value_measuredElementFS_91"),
                                 FUN = function(x){
                                     get.vertex.attribute(graph = stnd.graph,
                                                          name = x)
                                 }
                                 )),
                   nc = 2)
        ## terminalValue = get.vertex.attribute(graph = stnd.graph,
        ##     name = "Value_measuredElementFS_91")
        rownames(terminalValueMatrix) = V(stnd.graph)$name
        fullStandardization =
            rbind(terminalValueMatrix,
                  intermediateStandardization)
    })

str(check$nodes)


test.nodes = data.table(name = letters[1:5], values = 1:5, value2 = rnorm(5))
test.edges = data.table(from = letters[c(1, 3, 5)], to = letters[1], value = 1:3)

test.graph = graph.data.frame(d = test.edges, vertices = test.nodes)

matrix(unlist(lapply(list.vertex.attributes(test.graph), function(x) get.vertex.attribute(test.graph, x))[2:3]), nc = 2)

Reduce(function(x, y) cbind(data.frame(x), data.frame(y)), lapply(list.vertex.attributes(test.graph), FUN = function(x) get.vertex.attribute(test.graph, x)))



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
## NOTE (Michael): The propagation of element 66 and 96 should be done
##                 within Aupus after the update of element 41 and
##                 before the calculation of total supply.
##
## NOTE (Michael): The current implementation does not perform element
##                 66 and 96 after the element 41 has been updated,
##                 thus we can run element 69 and 99 before the Aupus
##                 module.
##
## NOTE (Michael): Should the functions for node and edge building be
##                 specific or generic?
##
## NOTE (Michael): Doo not try to wrap the aupus and standardization
##                 module together. Simply just use the foo2 function
##                 to build the edge and node data structure.
