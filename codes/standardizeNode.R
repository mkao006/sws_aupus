##' Function to standardize all the nodes in the node list of a graph
##'
##' The formula is hard coded
##'
##' @param graph The graph object created by the function
##' constructGraph.
##' @param workingNodes The nodes to be standardized
##' @param standardizeAttributes The attribute of the nodes to be
##' standardized.
##' @export
##' 

standardizeNode = function (graph, workingNode, standardizeAttributes)
{
    outEdges = E(graph)[from(V(graph)[node])]
    shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_share")
    rateMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_extraction")
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0
    
    valueMatrix =
        matrix(unlist(lapply(X = standardizeAttributes,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[colnames(shareMatrix)])
                             }
                             )),
               nc = length(standardizeAttributes))
    
    standardized = reverseMatrix %*% valueMatrix

    targetValueMatrix =
        matrix(unlist(lapply(X = standardizeAttributes,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[rownames(standardized)])
                             }
                             )),
               nc = length(standardizeAttributes))
    
    ## Need to convert the na to zeros for addition
    standardizedValues = targetValueMatrix  + standardized

    for(i in 1:NCOL(standardizedValues)){
        set.vertex.attribute(graph = graph, name = standardizeAttributes[i],
                             index = V(graph)[rownames(standardized)],
                             value = standardizedValues[, i])

        intermediateValuesMatrix =
            matrix(unlist(lapply(X = standardizeAttributes,
                                 FUN = function(x){
                                     get.vertex.attribute(graph = graph, name = x,
                                                          index = V(graph)[node])
                                 }
                                 )),
                   nc = length(standardizeAttributes))
        rownames(intermediateValuesMatrix) = node
    }
    graph = graph - vertices(node)
    list(standardizedGraph = graph, intermediateValues = intermediateValuesMatrix)   
}
