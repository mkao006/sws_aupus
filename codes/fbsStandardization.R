##' This function takes the graph and perform the standardization
##'
##' @param graph The standardization graph from the function
##' constructStandardizationGraph.
##' @param standardizeElement The node sttributes to be standardized.
##' @param plot Whether the graph/network should be plotted
##' @export

fbsStandardization = function(graph, standardizeElement, plot){
    standardized =
        lapply(graph, FUN = function(x){
            standardization(graph = x,
                            standardizeElement = standardizeElement,
                            plot = plot)
        })
    
    
    standardizationFinal =
        Reduce(rbind,
               lapply(names(standardized),
                      FUN = function(x){
                          standardized[[x]][, `:=`(c("timePointYearsSP"),
                                                   as.numeric(x))]
                      })
               )
    standardizationFinal
}
