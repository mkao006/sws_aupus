## Load libraries
## testCountryCode = 100
## source("test_get_api_data.R")

library(faoswsAupus)
library(faoswsUtil)
library(data.table)
library(igraph)

FBSelements = c("Value_measuredElementFS_61", "Value_measuredElementFS_91",
    "Value_measuredElementFS_111",
    "Value_measuredElementFS_121", "Value_measuredElementFS_141")

## Get the parameter
param = getAupusParameter(areaCode = "100", assignGlobal = FALSE)

## Get the data sets
getAupusDataset()

## This is a hack to fill in the missing columns
missingColumns =
    c(paste0("Value_measuredElementFS_", c(541, 546)),
      paste0("flagFaostat_measuredElementFS_", c(541, 546)))

aupusData[, `:=`(c(missingColumns),
                 list(as.numeric(NA), as.numeric(NA), as.character(NA),
                      as.character(NA)))]
   
## Construct the aupus network representation
aupusNetwork =
    suaToNetworkRepresentation(extractionRateData = extractionRateData,
                               shareData = shareData,
                               inputData = inputData,
                               ratioData = ratioData,
                               balanceElementData = balanceElementData,
                               itemInfoData = itemInfoData,
                               populationData = populationData)


## Run the aupus to update the data
updatedAupusNetwork =
    with(aupusNetwork,
         Aupus(nodes = nodes, edges = edges,
               from = param$keyNames$itemParentName,
               to = param$keyNames$itemChildName))


## Construct the network for standardization
standardizationGraph = 
    with(updatedAupusNetwork,
         constructStandardizationGraph(nodes = nodes, edges = edges,
                                       standardizeElement = FBSelements,
                                       from = param$keyNames$itemChildName,
                                       to = param$keyNames$itemParentName))

## Standardize the data
fbs =
    fbsStandardization(graph = standardizationGraph,
                       standardizeElement = FBSelements,
                       plot = FALSE)








## test.nodes = data.table(name = letters[1:5], values = 1:5, value2 = rnorm(5))
## test.edges = data.table(from = letters[c(1, 3, 5)], to = letters[1], value = 1:3)

## test.graph = graph.data.frame(d = test.edges, vertices = test.nodes)

## matrix(unlist(lapply(list.vertex.attributes(test.graph), function(x) get.vertex.attribute(test.graph, x))[2:3]), nc = 2)

## Reduce(function(x, y) cbind(data.frame(x), data.frame(y)), lapply(list.vertex.attributes(test.graph), FUN = function(x) get.vertex.attribute(test.graph, x)))



## NOTE (Michael): Looks like the tree is coded in the shares table,
##                 and the collapseShare or getShare function is
##                 losing some of the information.
##
## NOTE (Michael): To make everything clean and understandable, we
##                 only replicate those that are essential and well
##                 understood.
##
## NOTE (Michael): Do not make modification to the get data related
##                 functions unless they can be tested.
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
##
