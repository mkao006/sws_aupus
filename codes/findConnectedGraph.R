findConnectedGraph = function(graph, commodity){
    dist = shortest.paths(graph = graph, v = V(graph)[commodity],
        mode = "in")
    if(any(!commodity %in% V(graph)$name))
       stop("Commodity not found in graph")
    connectedNodes =
        unique(unlist(sapply(apply(X = dist, MARGIN = 1,
                                   FUN = function(x){
                                       which(is.finite(x))
                                   }), names)))
    if(length(connectedNodes) > 1){
        connectedGraph =
            induced.subgraph(graph, vids = V(graph)[connectedNodes])
    } else {
        connectedGraph =
            graph.data.frame(d = data.frame(from = "000",
                                 to = commodity))
        connectedGraph = connectedGraph - vertices("000")
    }
    connectedGraph
}
