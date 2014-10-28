constructGraph = function(shares, aupus, extractionRate,
    standardizeElement, plot = FALSE){
    ## print(str(aupus))
    ## print(str(shares))
    setnames(aupus,
             old = c(extractionRate, standardizeElement),
             new = c("extractionRate", "standardizeElement"))
    aupusExtract = aupus[, list(itemCode, extractionRate)]
    ## setnames(aupusExtract, "itemCode", "itemChildCode")    
    shareAupus = merge(shares, aupusExtract, by = "itemCode",
        all.x = TRUE, allow.cartesian = TRUE)
    
    e = shareAupus[!is.na(extractionRate) | extractionRate != 0,
        list(itemChildCode, itemCode, SHARE, extractionRate)]
    uniqueEdgeItem =
        sort(unique(c(unique(e$itemCode), unique(e$itemChildCode))))
    v = aupus[itemCode %in% uniqueEdgeItem,
        list(itemCode, standardizeElement)]
    ## print(str(e))
    ## print(str(v))
    ## print(uniqueEdgeItem)
    inEdgeNotInNode = setdiff(uniqueEdgeItem, v$itemCode)
    if(length(inEdgeNotInNode) > 0){
        missingNodes = 
            data.table(itemCode = inEdgeNotInNode,
                       standardizeElement = 0)
        v = rbind(v, missingNodes)
    }
    graph =
        graph.data.frame(d = e, vertices = v)
    setnames(aupus,
             new = c(extractionRate, standardizeElement),
             old = c("extractionRate", "standardizeElement"))
    if(plot)
        plot(graph, vertex.size = 8, edge.arrow.size = 0.5,
             vertex.label = paste0(V(graph)$name, "\n(",
                 V(graph)$standardizeElement, ")"))
    graph
}
