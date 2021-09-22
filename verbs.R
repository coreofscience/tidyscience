add_clusters_to_nodes <- function(graph) {
  
  clusters <- 
    graph |> 
    as.undirected(mode = "each") |> 
    cluster_lovain()
  
  graph_clusters <- 
    graph |> 
    set_vertex_attr(name = "cluster",
                    value = membership(clusters))
  
  return(graph_clusters)
  
}