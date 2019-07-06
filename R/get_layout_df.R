get_layout_df <- function(graph_df,
                          node_meta,
                          node_var,
                          prev_layout = NULL,
                          algorithm = "kk") {


  if (!is.null(prev_layout)) {

    my_layout <- create_layout(graph_from_data_frame(graph_df),
                    layout = "igraph",
                    algorithm = "kk",
                    coords = layout_from_previous(graph_df, prev_layout)
                    ) %>%
      left_join(node_meta, by = c("name" = node_var))

  } else {

    my_layout <- create_layout(graph_from_data_frame(graph_df),
                    layout = "igraph",
                    algorithm = "kk") %>%
      left_join(node_meta, by = c("name" = node_var))

  }

  return(my_layout)
}
