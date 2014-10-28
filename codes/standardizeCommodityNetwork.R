standardizeCommodityNetwork = function(shares, aupus, extractionRate,
    standardizeElement, commodity){
    print(commodity)
    graph = constructGraph(shares = shares, aupus = aupus,
        extractionRate = extractionRate,
        standardizeElement = standardizeElement)
    sub.graph = findConnectedGraph(graph, commodity = commodity)

    ## plot(sub.graph, vertex.size = 8, edge.arrow.size = 0.5,
    ##      vertex.label = paste0(V(sub.graph)$name, "\n(",
    ##          V(sub.graph)$standardizeElement, ")"))
    ## print(sub.graph)
    while(length(E(sub.graph)) > 0){
        workingNode =
            names(which(degree(sub.graph, mode = "in") == 0))
        sub.graph = standardizeNode(sub.graph, workingNode)
    }
    gc()
    V(sub.graph)$standardizeElement
}
