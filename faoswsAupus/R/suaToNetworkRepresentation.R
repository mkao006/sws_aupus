##' This is the function to convert sua data to network specification
##' which is more natural for processing.
##'
##'
##' @param extractionRateData The extraction rate data
##' @param shareData The share data from getShare function
##' @param inputData The input data from the function getInputFromProcessing.
##' @param ratioData The ratio data from getRatio
##' @param balanceElementData The balance element data from the
##' function getBalanceElementData.
##' @param itemInfoData
##' @export
##' 



suaToNetworkRepresentation = function(extractionRateData, shareData, inputData,
    ratioData, balanceElementData, itemInfoData, populationData, aupusParam){
    
    edges =
        buildEdges(extractionRateData = extractionRateData,
                   shareData = shareData,
                   inputData = inputData)

    nodes =
        buildNodes(aupusData = aupusData, ratioData = ratioData,
                   balanceElementData = balanceElementData,
                   itemInfoData = itemInfoData,
                   populationData = populationData,
                   balanceElementNum = aupusParam$keyNames$balanceElementName)

    ## CHECK (Michael): Add in nodes that are contained in the edges
    ##                  list but not found in the node frame. Check
    ##                  why they are missing.

    uniqueEdgeNodes =
        unique(unlist(edges[, c(aupusParam$keyNames$itemParentName,
                                aupusParam$keyNames$itemChildName), with = FALSE]))

    missingNodes = uniqueEdgeNodes[!uniqueEdgeNodes %in%
        nodes[[aupusParam$keyNames$itemName]]]

    missingKeyTable =
        data.table(expand.grid(unlist(unique(nodes[, c(aupusParam$keyNames$areaName),
                                                   with = FALSE])),
                               unlist(unique(nodes[, c(aupusParam$keyNames$yearName),
                                                   with = FALSE])),
                               missingNodes, stringsAsFactors = FALSE))
    with(aupusParam$keyNames,
         setnames(missingKeyTable, c(areaName, yearName, itemName)))
    nodes = rbind(nodes, missingKeyTable, fill = TRUE)
    setkeyv(nodes, key(aupusData))
    
    

    list(nodes = nodes, edges = edges)
}
