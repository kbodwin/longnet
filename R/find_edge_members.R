find_edge_members <- function(graph_info,
                           node_var,
                           edge_var,
                           edge_labels = edge_var,
                           node_1, node_2) {


  node_1_sub <- graph_info[graph_info[[node_var]] == node_1,][[edge_var]]
  node_2_sub <- graph_info[graph_info[[node_var]] == node_2,][[edge_var]]

  edge_mems <- intersect(node_1_sub, node_2_sub)

  relevant <- graph_info[[node_var]] == node_1 & graph_info[[edge_var]] %in% edge_mems

  edge_mems_names <- graph_info[relevant, ][[edge_labels]]

  return(paste(edge_mems_names, collapse = ", "))

}
