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

constructGraph = function(shareData, aupus, param, shares,
    extractionRate, standardizeElement, plot = FALSE){
    ## print(str(aupus))
    ## print(str(shares))
    aupusEdge = aupus[, c(key(aupus)[2], extractionRate), with = FALSE]
    setnames(aupusEdge,
             old = c(key(aupus)[2], extractionRate),
             new = c(param$keyNames$itemChildName, "extractionRate"))
    aupusNode = aupus[, c(key(aupus)[2], standardizeElement),
        with = FALSE]
    setnames(aupusNode,
             old = standardizeElement,
             new = "standardizeElement")
    ## setnames(aupus,
    ##          old = c(extractionRate, standardizeElement),
    ##          new = c("extractionRate", "standardizeElement"))
    setnames(shareData,
             old = shares,
             new = "shares")

    ## Construct edge from share and extraction aret
    ## aupusExtract = aupus[, c(key(aupus)[2], "extractionRate"),
    ##     with = FALSE]
    shareAupus = merge(shareData, aupusEdge,
        by = param$keyNames$itemChildName,
        all.x = TRUE, allow.cartesian = TRUE)
    

    ## NOTE (Michael): This specification is standadradizing up, need
    ##                 to change if rolling down.
    e = shareAupus[!is.na(extractionRate) | extractionRate != 0,
        c(param$keyNames$itemChildName, param$keyNames$itemParentName,
          "shares", "extractionRate"), with = FALSE]
    uniqueEdgeItem =
        sort(unique(c(unique(e[[param$keyNames$itemChildName]]),
                      unique(e[[param$keyNames$itemParentName]]))))

    ## Construct the nodes
    ## v = aupus[, list(itemCode, standardizeElement)]
    v =  aupusNode
    ## print(str(e))
    ## print(str(v))
    ## print(uniqueEdgeItem)

    ## If certain nodes are in the edge but not in the nodes, then add
    ## the node to the node data but with the element set to zero.
    inEdgeNotInNode = setdiff(uniqueEdgeItem, unique(v[[key(aupus)[2]]]))
    print(inEdgeNotInNode)
    if(length(inEdgeNotInNode) > 0){
        missingNodes = 
            data.table(itemCode = inEdgeNotInNode,
                       standardizeElement = 0)
        v = rbind(v, missingNodes)
    }
    print(str(e))
    print(str(v))
    ## Construct the graph
    graph =
        graph.data.frame(d = e, vertices = v)
    ## setnames(aupus,
    ##          new = c(extractionRate, standardizeElement),
    ##          old = c("extractionRate", "standardizeElement"))
    if(plot)
        plot(graph, vertex.size = 8, edge.arrow.size = 0.5,
             vertex.label = paste0(V(graph)$name, "\n(",
                 V(graph)$standardizeElement, ")"))
    setnames(shareData,
             new = shares,
             old = "shares")
    graph
}
