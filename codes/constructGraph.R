##' This function construct the graph/network necessary for
##' standardization.
##'
##' @param shareData The shares data extracted from getShare.
##' @param aupus The aupus data
##' @param shares The column correspoind to the shares value.
##' @param extractionRate The column corresponds to extraction rate.
##' @param standarizeElemnt The element name to be standardized.
##' @param plot Whether the network should be plotted.
##' @export
##' 

constructGraph = function(shareData, aupus, shares, extractionRate,
    standardizeElement, plot = FALSE){
    ## print(str(aupus))
    ## print(str(shares))
    setnames(aupus,
             old = c(extractionRate, standardizeElement),
             new = c("extractionRate", "standardizeElement"))
    setnames(shareData,
             old = shares,
             new = "shares")

    ## Construct edge from share and extraction aret
    aupusExtract = aupus[, list(itemCode, extractionRate)]
    shareAupus = merge(shareData, aupusExtract, by = c(key(aupus)[2]),
        all.x = TRUE, allow.cartesian = TRUE)
    
    e = shareAupus[!is.na(extractionRate) | extractionRate != 0,
        list(itemChildCode, itemCode, shares, extractionRate)]
    uniqueEdgeItem =
        sort(unique(c(unique(e$itemCode), unique(e$itemChildCode))))
    v = aupus[, list(itemCode, standardizeElement)]
    ## print(str(e))
    ## print(str(v))
    ## print(uniqueEdgeItem)
    inEdgeNotInNode = setdiff(uniqueEdgeItem, unique(v$itemCode))
    if(length(inEdgeNotInNode) > 0){
        missingNodes = 
            data.table(itemCode = inEdgeNotInNode,
                       standardizeElement = 0)
        v = rbind(v, missingNodes)
    }
    graph =
        graph.data.frame(d = e, vertices = v)
    ## setnames(aupus,
    ##          new = c(extractionRate, standardizeElement),
    ##          old = c("extractionRate", "standardizeElement"))
    if(plot)
        plot(graph, vertex.size = 8, edge.arrow.size = 0.5,
             vertex.label = paste0(V(graph)$name, "\n(",
                 V(graph)$standardizeElement, ")"))
    setnames(aupus,
             new = c(extractionRate, standardizeElement),
             old = c("extractionRate", "standardizeElement"))
    setnames(shareData,
             new = shares,
             old = "shares")
    graph
}
