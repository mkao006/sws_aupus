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

standardizeNode = function (graph, workingNode, standardizeAttributes){

    ## Get the edges and the construct the reverse matrix
    outEdges = E(graph)[from(V(graph)[node])]
    shareMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_share")
    rateMatrix = get.adjacency(subgraph.edges(graph, outEdges), 
        sparse = FALSE, attr = "Value_extraction")
    reverseMatrix = t(shareMatrix)/t(rateMatrix)
    reverseMatrix[is.na(reverseMatrix) | !is.finite(reverseMatrix)] = 0

    ## Get the value which is to be standardized
    valueMatrix =
        matrix(unlist(lapply(X = standardizeAttributes,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[colnames(shareMatrix)])
                             }
                             )),
               nc = length(standardizeAttributes))

    ## Standardization
    standardized = reverseMatrix %*% valueMatrix

    ## Get the target value of the standardization
    targetValueMatrix =
        matrix(unlist(lapply(X = standardizeAttributes,
                             FUN = function(x){
                                 get.vertex.attribute(graph = graph, name = x,
                                                      index = V(graph)[rownames(standardized)])
                             }
                             )),
               nc = length(standardizeAttributes))

    ## Add the standardized value to the target value
    ##
    ## TODO (Michael): Need to convert the na to zeros for addition
    standardizedValues = targetValueMatrix  + standardized

    ## Save the target value back to the graph, and the intermediave
    ## to the intermediate value matrix
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

    ## Delete the standardized nodes
    graph = graph - vertices(node)

    ## Return the objects
    list(standardizedGraph = graph, intermediateValues = intermediateValuesMatrix)   
}
