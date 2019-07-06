layout_from_previous <- function(new_graph, prev_layout){

  prev_df <- cbind(prev_layout$x, prev_layout$y, prev_layout$name) %>%
    as.data.frame()

  names(prev_df) = c("x", "y", "name")

  new_node_names = data.frame(
    name = unique(c(new_graph$from, new_graph$to))
  )

  match_layout <-  prev_df %>%
    right_join(new_node_names)


  nas <- is.na(match_layout$x)

  match_layout$x[nas] = runif(sum(nas), min(prev_layout$x), max(prev_layout$y))
  match_layout$y[nas] = runif(sum(nas), min(prev_layout$y), max(prev_layout$y))

  return(as.matrix(match_layout[, c("x", "y")]))

}
