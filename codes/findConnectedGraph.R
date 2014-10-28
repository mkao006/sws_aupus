findConnectedGraph = function(graph, commodity){
    ## print(commodity)
    ## print(V(graph))
    ## print(E(graph))
    ## print(commodity)
    if(commodity %in% V(graph)$name){
        dist = shortest.paths(graph = graph,
            v = V(graph)[commodity], mode = "in")
        ## print(dist)
        ## print(V(graph)[colnames(dist)[is.finite(dist)]])
        sub.graph = induced.subgraph(graph,
            V(graph)[colnames(dist)[is.finite(dist)]])
    } else {
        artificialEdge =
            data.frame(commodity = commodity, artifical = "000")
        artificialNodes =
            data.frame(commodity = c(commodity, "000"),
                       standardizeElement = 0)
        sub.graph = graph.data.frame(d = artificialEdge,
                                     vertices = artificialNodes)
        sub.graph = sub.graph - vertices("000")
    }
    sub.graph
}
