standardizeNode = function(graph, leave){
    outEdges = E(graph)[from(V(graph)[leave])]
    ## print(outEdges)
    ## print(V(graph)[get.edges(graph, outEdges)[, 2]]$NUM_61)
    standardized =
        outEdges$share *
            V(graph)[leave]$standardizeElement/outEdges$extractionRate
    if(any(is.na(standardized)))
        standardized[is.na(standardized)] = 0
    if(any(!is.finite(standardized)))
        standardized[!is.finite(standardized)] = 0
    V(graph)[get.edges(graph, outEdges)[, 2]]$standardizeElement =
        V(graph)[get.edges(graph, outEdges)[, 2]]$standardizeElement +
            standardized            
    graph = graph - vertices(leave)
    graph
}
