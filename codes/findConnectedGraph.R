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
    induced.subgraph(graph, vids = V(graph)[connectedNodes])
}
