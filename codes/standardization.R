##' This function performs standardization
##'
##' Standardization is simply a weighted aggregation, but with
##' multiple hierachical levels.
##'
##' @param graph The graph object
##' @param standardizeElement The node attribute in the graph to be
##' standardized.
##' @export
##' 

standardization = function(graph, standardizeElement){
    intermediateStandardization = c()
    while (length(E(graph)) > 0) {
        workingNode =
            names(which(degree(graph, mode = "in") == 
                            0 & degree(graph, mode = "out") > 0))
        standardize = standardizeNode(graph = graph, 
            node = workingNode,
            standardizeElement = standardizeElement)
        graph = standardize$standardizedGraph
        if (FALSE)
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
               nc = 2)
    ## terminalValue = get.vertex.attribute(graph = graph,
    ##     name = "Value_measuredElementFS_91")
    rownames(terminalValueMatrix) = V(graph)$name
    fullStandardization =
        rbind(terminalValueMatrix,
              intermediateStandardization)
    colnames(fullStandardization) = standardizeElement
    data.table(fullStandardization)
}
