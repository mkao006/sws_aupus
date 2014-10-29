standardizeNode = function(graph, leave){
    outEdges = E(graph)[from(V(graph)[leave])]
    shareMatrix =
        get.adjacency(subgraph.edges(graph, outEdges), sparse = FALSE,
                      attr = "SHARE")
    rateMatrix =
        get.adjacency(subgraph.edges(graph, outEdges), sparse = FALSE,
                      attr = "extractionRate")
    ## print(t(shareMatrix))
    ## print(t(rateMatrix))
    values = V(graph)[colnames(shareMatrix)]$standardizeElement
    ## print(values)
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    standardized = reverseMatrix %*% matrix(values, nc = 1)
    V(graph)[rownames(standardized)]$standardizeElement =
        V(graph)[rownames(standardized)]$standardizeElement +
            standardized
    intermediateValues = V(graph)[leave]$standardizeElement
    names(intermediateValues) = leave
    graph = graph - vertices(leave)
    list(standardizedGraph = graph,
         intermediateValues = intermediateValues)
}
