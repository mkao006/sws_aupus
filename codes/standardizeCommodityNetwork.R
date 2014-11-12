##' This function performs the standardization of a whole commodity
##' tree.
##'
##' The commodity list does not necessarily have to include the root,
##' for example standardizing lower level items to wheat flour without
##' wheat is possible.
##'
##' @param shareData The shares data obtained from the function getShare.
##' @param aupus The aupus data obtained from the function
##' getAupusData.
##' @param shares The column correspoind to the shares value.
##' @param extractionRate The column name which corresponds to the
##' extraction rate, usually element 41.
##' @param standardizeElement The column name of the element to be
##' standardized.
##' @param commodity The commodity list to be standardized
##' @param plot Whether the standardization graph should be plotted.
##' @export
##' 

standardizeCommodityNetwork = function(shareData, aupus, shares,
    extractionRate, standardizeElement, commodity, plot = FALSE){
    ## print(commodity)
    ## print(str(shares))
    graph = constructGraph(shareData = shareData, aupus = aupus,
        shares = shares, extractionRate = extractionRate,
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
            standardize =
                standardizeNode(graph = sub.graph, node = workingNode)
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
