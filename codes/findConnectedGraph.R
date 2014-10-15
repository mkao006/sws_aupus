findConnectedGraph = function(graph, commodity){
    ## print(commodity)
    ## print(graph)
    ## plot(graph)
    ## print(commodity)
    dist = shortest.paths(graph,
        v = V(graph)[commodity], mode = "in")
    sub.graph = induced.subgraph(graph,
        V(graph)[colnames(dist)[is.finite(dist)]])
    ## print(sub.graph)
    sub.graph
}
