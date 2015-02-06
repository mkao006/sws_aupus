##' This function performs standardization
##'
##' Standardization is simply a weighted aggregation, but with
##' multiple hierachical levels.
##'
##' @param graph The graph object
##' @param standardizeElement The node attribute in the graph to be
##' standardized.
##' @param plot Whether the network should be plotted.
##' @export
##' 

standardization = function(graph, standardizeElement, plot){
    intermediateStandardization = c()
    while (length(E(graph)) > 0) {
        workingNode =
            names(which(degree(graph, mode = "in") == 
                            0 & degree(graph, mode = "out") > 0))
        standardize = standardizeNode(graph = graph, 
            workingNode = workingNode,
            standardizeAttributes = standardizeElement)
        graph = standardize$standardizedGraph
        if (plot)
            plot(graph, vertex.size = 3, edge.arrow.size = 0.5,
                 vertex.label.cex = 0.5)
        intermediateStandardization = rbind(intermediateStandardization, 
            standardize$intermediateValues)
    }
    terminalValueMatrix =
        matrix(unlist(lapply(X = standardizeElement,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph,
                                                      name = x)
                             }
                             )),
               nc = length(standardizeElement))
    ## terminalValue = get.vertex.attribute(graph = graph,
    ##     name = "Value_measuredElementFS_91")
    rownames(terminalValueMatrix) = V(graph)$name
    fullStandardization =
        rbind(terminalValueMatrix,
              intermediateStandardization)
    fullStandardization = data.table(cbind(rownames(fullStandardization),
        fullStandardization))
    setnames(fullStandardization, old = colnames(fullStandardization),
             new = c(aupusParam$keyNames$itemName, standardizeElement))
    fullStandardization

}
