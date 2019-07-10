make_network_plot <- function(edge_info, node_layout, date_string,
                              weighted_edges = TRUE,
                              node_label_var,
                              node_cols = rep("#619CFF", nrow(node_layout)),
                              edge_cols = rep("black", nrow(edge_info)),
                              edge_transparency = NULL, node_size = 10){


  #### Nodes

  my_network <- plot_ly(x = ~node_layout$x,
                        y = ~node_layout$y,
                        type = "scatter",
                        mode = "markers",
                        marker = list(color = node_cols, size = node_size),
                        text = node_layout[[node_label_var]],
                        hoverinfo = "text",
                        width = 700, height = 600)


  #### Edges ####

  if (!weighted_edges) {

    edge_info$weights = 1

  }

  edge_shapes <- list()


  for(i in 1:nrow(edge_info)) {

    v_from_idx <- which(node_layout$name == edge_info$from[i])
    v_to_idx <- which(node_layout$name == edge_info$to[i])

    edge_shape = list(
      type = "line",
      line = list(color = edge_cols[i], width = edge_info$width),
      opacity = edge_transparency,
      layer = 'below',
      x0 = node_layout$x[v_from_idx],
      y0 = node_layout$y[v_from_idx],
      x1 = node_layout$x[v_to_idx],
      y1 = node_layout$y[v_to_idx]
    )

    edge_shapes[[i]] <- edge_shape
  }

  #### prepare to erase axes ####
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )

  #### layout ####

  layout(
    my_network,
    shapes = edge_shapes,
    title = date_string,
    xaxis = ax,
    yaxis = ax
  )

}


make_node_cols <- function(node_layout,
                           node_var,
                           group_var = node_var,
                           grouping_var = node_var,
                           color_all_groups = FALSE,
                           highlight_groups = NULL,
                           highlight_nodes = NULL) {

  # highlight_indivs and highlight_groups are possibly vectors
  # color_all_groups overrides highlight_groups

  #### Deal with NAs ####

  nas <- is.na(node_layout[[grouping_var]])

  node_layout[nas, grouping_var] <- "None"


  if (is.null(grouping_var)) {

    grouping_var = node_var

  }

  #### Group Coloring ####

  if (color_all_groups) {

    node_col_set <- data.frame(
      cats <- unique(node_layout[[grouping_var]]),
      cols <- ggcolors(length(unique(node_layout[[grouping_var]])))
      )

    node_cols <- node_col_set$cols[as.integer(as.factor(node_layout[[grouping_var]]))]

  } else {

    node_cols <- rep("#619CFF", nrow(node_layout))

  }

  #### Individual Group Highlighting ####

  if (!is.null(highlight_groups)) {

    where_hl <- node_layout[[group_var]] %in% highlight_groups
    node_cols[where_hl] <- "#FFD700"

  }

  #### Individual Node Highlighting ####

  if (!is.null(highlight_nodes)) {

    where_hl <- node_layout$name %in% highlight_nodes
    node_cols[where_hl] <- "#FF6A6A"

  }

  return(node_cols)

}


make_edge_cols <- function(edge_info,
                           highlight_nodes = NULL) {

  # highlight_indivs is possibly vector

  edge_cols <- rep("black", nrow(edge_info))

  #### Individual Node Highlighting ####

  if (!is.null(highlight_nodes)) {

    where_hl <- edge_info$from %in% highlight_nodes | edge_info$to %in% highlight_nodes
    edge_cols[where_hl] <- "indianred1"

  }

  return(edge_cols)

}
