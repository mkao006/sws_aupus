standardizeCommodityNetwork = function(shares, aupus, extractionRate,
    standardizeElement, commodity, plot = FALSE){
    ## print(commodity)
    ## print(str(shares))
    graph = constructGraph(shares = shares, aupus = aupus,
        extractionRate = extractionRate,
        standardizeElement = standardizeElement)
    sub.graph = findConnectedGraph(graph, commodity = commodity)
    if(length(E(sub.graph)) > 0){
        if(plot)
            plot(sub.graph, vertex.size = 8, edge.arrow.size = 0.5,
                 vertex.label = paste0(V(sub.graph)$name, "\n(",
                     V(sub.graph)$standardizeElement, ")"))
        
        intermediateStandardization = c()
        while(length(E(sub.graph)) > 0){
            workingNode =
                names(which(degree(sub.graph, mode = "in") == 0 &
                                degree(sub.graph, mode = "out") > 0))
            standardize = standardizeNode(sub.graph, workingNode)
            sub.graph =
                standardize$standardizedGraph
            if(plot)
                plot(sub.graph, vertex.size = 8, edge.arrow.size = 0.5,
                     vertex.label = paste0(V(sub.graph)$name, "\n(",
                         V(sub.graph)$standardizeElement, ")"))
            intermediateStandardization =
                c(intermediateStandardization,
                  standardize$intermediateValues)
        }
        gc()
        terminalValue = V(sub.graph)$standardizeElement
        names(terminalValue) = V(sub.graph)$name
        fullStandardization =
            c(terminalValue, intermediateStandardization)
        standardizedValue = as.numeric(rep(NA, length(commodity)))
        names(standardizedValue) = commodity
        standardizedItem =
            intersect(names(standardizedValue),
                      names(fullStandardization))
        standardizedValue[standardizedItem] =
            fullStandardization[standardizedItem]
    } else {
        ## TODO (Michael): Need to make this the same as the commodity
        ##                 value rather than NA.
        standardizedValue = as.numeric(rep(NA, length(commodity)))
        names(standardizedValue) = commodity
    }
    standardizedValue
}
