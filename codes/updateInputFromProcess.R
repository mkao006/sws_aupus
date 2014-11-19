##' The function re-calculates the input from processing
##'
##' @param nodes The nodes data returned by the function buildNodes
##' @param edges The edge data returned by the function buildEdges
##' @param param The parameters from the function getAupusParameter
##' @param element131Num The column corresponding to element 131.
##' @export
##' 

updateInputFromProcessing = function(nodes, edges, param, element31Num){
    setnames(nodes, old = element31Num, new = "element31Num")
    aggregateKey = key(edges)
    aggregateKey = aggregateKey[aggregateKey != param$keyNames$itemParentName]
    aggregatedInput = edges[, list(Aggregated_input = sum(Value_input)),
        by = aggregateKey]
    setnames(aggregatedInput,
             old = param$keyNames$itemChildName,
             new = param$keyNames$itemName)
    newInputKeys = aggregateKey
    newInputKeys[newInputKeys == param$keyNames$itemChildName] =
        param$keyNames$itemName

    okey = key(nodes)
    setkeyv(nodes, newInputKeys)
    nodes[aggregatedInput, element31Num := Aggregated_input]
    setkeyv(nodes, okey)
    setnames(nodes, new = element31Num, old = "element31Num")

}
