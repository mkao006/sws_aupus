##' Function to update the extraction rates and input from processing
##' after each iteration of Aupus.
##'
##' @param nodes The nodes data returned by the function buildNodes
##' @param edges The edge data returned by the function buildEdges
##' @param element41Num The column correspoinding to element 41, or
##' extraction rates.
##' @param element131Num The column corresponding to element 131.
##'
##' @export

updateEdges = function(nodes, edges, element41Num, element131Num){
    ## Update input
    ##
    ## Element 131 is for parent, while input value is child
    newInputs = na.omit(nodes[, c(key(nodes), element131Num), with = FALSE])
    setnames(newInputs,
             old = c(aupusParam$keyNames$itemName, element131Num),
             new = c(aupusParam$keyNames$itemParentName, "inputFromParent"))
    newInputKeys = key(nodes)
    newInputKeys[newInputKeys == aupusParam$keyNames$itemName] =
        aupusParam$keyNames$itemParentName
    setkeyv(newInputs, newInputKeys)

    ## Update extraction rates
    ##
    ## Extraction rate is for children
    newExtraction = na.omit(nodes[, c(key(nodes), element41Num), with = FALSE])
    setnames(newExtraction,
             old = c(aupusParam$keyNames$itemName, element41Num),
             new = c(aupusParam$keyNames$itemChildName, "newExtractionRates"))
    newExtractionKeys = key(nodes)
    newExtractionKeys[newExtractionKeys == aupusParam$keyNames$itemName] =
        aupusParam$keyNames$itemChildName
    setkeyv(newExtraction, newExtractionKeys)

    okey = key(edges)
    setkeyv(edges, key(newInputs))
    edges[newInputs, Value_input := inputFromParent * Value_share]
    setkeyv(edges, key(newExtraction))
    edges[newExtraction, Value_extraction := newExtractionRates]
    setkeyv(edges, okey)
}
