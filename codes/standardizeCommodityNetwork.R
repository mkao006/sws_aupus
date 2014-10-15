standardizeCommodityNetwork = function(shares, aupus, extractionRate,
    standardizeElement, commodity){
    graph = constructGraph(shares = shares, aupus = aupus,
        extractionRate = extractionRate,
        standardizeElement = standardizeElement)
    sub.graph = findConnectedGraph(graph, commodity = commodity)

    ## print(str(sub.graph))
    ## plot(sub.graph)
    while(length(E(sub.graph)) > 0){
        workingNode =
            names(which(degree(sub.graph, mode = "in") == 0))[1]
        ## print(workingNode)
        sub.graph = standardizeNode(sub.graph, workingNode)
        ## print(V(sub.graph)$standardizeElement)
    }
    V(sub.graph)$standardizeElement
}
