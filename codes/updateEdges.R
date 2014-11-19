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

updateEdges = function(nodes, edges, element41Num, element131Num, param){
    ## Update input
    ##
    ## Element 131 is for parent, while input value is child
    newInputs = na.omit(nodes[, c(key(nodes), element131Num), with = FALSE])
    setnames(newInputs,
             old = c(param$keyNames$itemName, element131Num),
             new = c(param$keyNames$itemParentName, "inputFromParent"))
    newInputKeys = key(nodes)
    newInputKeys[newInputKeys == param$keyNames$itemName] =
        param$keyNames$itemParentName
    setkeyv(newInputs, newInputKeys)

    ## Update extraction rates
    ##
    ## Extraction rate is for children
    newExtraction = na.omit(nodes[, c(key(nodes), element41Num), with = FALSE])
    setnames(newExtraction,
             old = c(param$keyNames$itemName, element41Num),
             new = c(param$keyNames$itemChildName, "newExtractionRates"))
    newExtractionKeys = key(nodes)
    newExtractionKeys[newExtractionKeys == param$keyNames$itemName] =
        param$keyNames$itemChildName
    setkeyv(newExtraction, newExtractionKeys)

    okey = key(edges)
    setkeyv(edges, key(newInputs))
    edges[newInputs, Value_input := inputFromParent * Value_share]
    setkeyv(edges, key(newExtraction))
    edges[newExtraction, Value_extraction := newExtractionRates]
    setkeyv(edges, okey)
}
