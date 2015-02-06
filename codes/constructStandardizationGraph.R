##' This function constructs the graph/network required for standardization
##'
##' @param nodes The nodes of the network representation
##' @param edges The edges of the network representation
##' @param standardizeElement The names of the elements to be standardized.
##' @param from The name corresponding to the from node in the edge.
##' @param to The name corresponding to the target node in the edge.
##' @export
##' 


constructStandardizationGraph = function(nodes, edges,
    standardizeElement, from, to){

    nodeCopy = copy(nodes)
    edgeCopy = copy(edges)

    ## NOTE (Michael): Can use set column order here! (setcolorder)
    edgeCopy = edgeCopy[, c(from, to, colnames(edgeCopy)[!colnames(edgeCopy) %in%
        c(from, to)]), with = FALSE]
    setnames(edgeCopy, to, aupusParam$keyNames$itemName)


    uniqueYears = unique(unique(nodeCopy$timePointYearsSP),
        unique(edgeCopy$timePointYearsSP))
    nodes.lst = split(nodeCopy, uniqueYears)
    edges.lst = split(edgeCopy, uniqueYears)

    nodes.lst =
        lapply(nodes.lst,
               FUN = function(x){
                   x[, c("measuredItemFS", standardizeElement), with = FALSE]
               })
    
    graph.lst = mapply(graph.data.frame, d = edges.lst, vertices = nodes.lst,
        SIMPLIFY = FALSE)
    graph.lst
}


