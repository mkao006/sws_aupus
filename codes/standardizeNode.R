##' Function to standardize all the nodes in the node list of a graph
##'
##' The formula is hard coded
##'
##' @param graph The graph object created by the function
##' constructGraph.
##' @param node The nodes to be standardized
##' @export
##' 

standardizeNode = function(graph, node){
    outEdges = E(graph)[from(V(graph)[node])]

    ## TODO (Michael): Remove the hard coded scaling of share and
    ##                 extraction rates.
    ##
    ## NOTE (Michael): No need for shares in standardization back
    ## shareMatrix =
    ##     100/get.adjacency(subgraph.edges(graph, outEdges),
    ##                       sparse = FALSE, attr = "shares")
    shareMatrix[TRUE] = 1
    ## if(!all(colSums(1/shareMatrix) %in% c(0, 1))){
    ##     print(shareMatrix)
    ##     stop("Shares does not sum up to one")
    ## }
    rateMatrix =
        10000/get.adjacency(subgraph.edges(graph, outEdges),
                            sparse = FALSE, attr = "extractionRate")
    ## print(t(shareMatrix))
    ## print(t(rateMatrix))
    values = V(graph)[colnames(shareMatrix)]$standardizeElement
    ## print(values)
    reverseMatrix = t(shareMatrix) * t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    print(reverseMatrix)
    print(matrix(values, nc = 1))
    standardized = reverseMatrix %*% matrix(values, nc = 1)
    print(standardized)
    V(graph)[rownames(standardized)]$standardizeElement =
        V(graph)[rownames(standardized)]$standardizeElement +
            standardized
    intermediateValues = V(graph)[node]$standardizeElement
    names(intermediateValues) = node
    graph = graph - vertices(node)
    list(standardizedGraph = graph,
         intermediateValues = intermediateValues)
}
