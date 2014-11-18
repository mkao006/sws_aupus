##' The function finds the processing level of the item in relation
##' with the rest of the commodity items.
##'
##' @param edgeData the edge data from the function buildEdgeRelation
##' @param from The column corresponding to the from node.
##' @param to The column corresponding to target node
##' @param plot Whether the graph should be plotted.
##'
##' @export

findProcessingLevel = function(edgeData, from, to, plot = FALSE){
    e = edgeData[, c(from, to), with = FALSE]
    v = unique(unlist(aupusEdges[, c(from, to), with = FALSE]))
    processingGraph = graph.data.frame(d = e, vertices = v, directed = TRUE)
    
    root = names(which(degree(processingGraph, mode = "in") == 0 &
                           degree(processingGraph, mode = "out") > 0))

    processingLevel =
        shortest.paths(processingGraph, v = V(processingGraph),
                       to = V(processingGraph)[c(root)], mode = "in")
    if(plot == TRUE)
        plot(processingGraph)

    ## Take the finite maximum level fro processing level
    finalLevels = apply(processingLevel, 1,
        FUN = function(x) max(x[is.finite(x)], na.rm = TRUE))

}