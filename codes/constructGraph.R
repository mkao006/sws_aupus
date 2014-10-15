constructGraph = function(shares, aupus, extractionRate,
    standardizeElement){
    ## print(str(aupus))
    ## print(str(shares))
    setnames(aupus,
             old = c(extractionRate, standardizeElement),
             new = c("extractionRate", "standardizeElement"))
    aupusExtract = aupus[, list(itemCode, extractionRate)]
    setnames(aupusExtract, "itemCode", "itemChildCode")
    shareAupus = merge(shares, aupusExtract, by = "itemChildCode")
    ## print(str(shareAupus))
    e = shareAupus[, list(itemChildCode, itemCode, share,
        extractionRate)]
    v = aupus[, list(itemCode, standardizeElement)]
    missingNodes = 
        data.table(itemCode =
                       setdiff(unique(e$itemCode, e$itemChildCode),
                               v$itemCode),
                   standardizeElement = 0)
    v = rbind(v, missingNodes)
    graph =
        graph.data.frame(d = e, vertices = v)
    setnames(aupus,
             new = c(extractionRate, standardizeElement),
             old = c("extractionRate", "standardizeElement"))
    ## plot(graph)
    graph
}
