##' The function performs the whole aupus procedure
##'
##' @param nodes The nodes returned in the list from the function
##' suaToNetworkRepresentation.
##' @param edges The edge returned in the list from the function
##' suaToNetworkRepresentation.
##' @param from The column name corresponding to the starting nodes.
##' @param to The column corresponding to the target nodes.
##' @export
##' 


Aupus = function(nodes, edges, from, to){

    processingLevelData =
        edges[, findProcessingLevel(.SD, from = from, to = to),
                   by = c(aupusParam$keyNames$areaName, aupusParam$keyNames$yearName)]
    setkeyv(processingLevelData, key(nodes))

    nodes[processingLevelData, processingLevel := i.processingLevel]
    nodes[is.na(processingLevel), processingLevel := as.numeric(0)]


    for(currentLevel in range(nodes$processingLevel)){

        ## Step (1): Run the aupus module at the primary level on the nodes
        calculateAupusElements(aupusFinalData =
                                   nodes[processingLevel == currentLevel, ],
                               itemTypeCol = aupusParam$keyNames$itemTypeName,
                               balanceElementNum =
                                   aupusParam$keyNames$balanceElementName)

        ## Step (2): Update the edges (extraction rate and input from processing)
        updateEdges(nodes = nodes[processingLevel == currentLevel, ], 
                    edges = edges,
                    element41Num = with(aupusParam$keyNames,
                        paste0(valuePrefix, elementName, "_41")),
                    element131Num = with(aupusParam$keyNames,
                        paste0(valuePrefix, elementName, "_131")))

        ## Step (3): Propagate input from processing to the node
        updateInputFromProcessing(nodes = nodes[processingLevel == currentLevel, ],
                                  edges = edges,
                                  element31Num = "Value_measuredElementFS_31")
    }

    nodes[,processingLevel := NULL]
    list(nodes = nodes, edges = edges)
}
