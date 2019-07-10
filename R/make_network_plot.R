## Testing
# edge_info = bob
# node_layout = prev_layout
# date_string = "1970-01-01"
# node_label_var = "IA.Name"
# node_var = "IA.ID"
# node_cols = rep("#619CFF", nrow(node_layout))
# edge_cols = rep("black", nrow(edge_info))
# edge_transparency = NULL
# node_size = 10


make_network_plot <- function(edge_info, node_layout, date_string,
                              node_var, node_label_var,
                              weighted_edges = TRUE,
                              node_cols = rep("#619CFF", nrow(node_layout)),
                              edge_cols = rep("black", nrow(edge_info)),
                              edge_transparency = NULL, node_size = 10){


  #### Nodes ####

  my_network <- plot_ly(x = ~node_layout$x,
                        y = ~node_layout$y,
                        type = "scatter",
                        mode = "markers",
                        marker = list(color = node_cols, size = node_size),
                        text = node_layout[[node_label_var]],
                        hoverinfo = "text",
                        width = 700, height = 600)


  #### Edges ####

  # Find endpoints and midpoints of lines based on node info
  edge_info <- edge_info %>% mutate(
    node_idx_from = map_int(from, ~which(node_layout[[node_var]] == .x)[1]),
    node_idx_to = map_int(to, ~which(node_layout[[node_var]] == .x)[1]),
    start_x = map_dbl(node_idx_from, ~node_layout$x[.x]),
    start_y = map_dbl(node_idx_from, ~node_layout$y[.x]),
    end_x = map_dbl(node_idx_to, ~node_layout$x[.x]),
    end_y = map_dbl(node_idx_to, ~node_layout$y[.x]),
    midpoint_x = (start_x + end_x)/2,
    midpoint_y = (start_y + end_y)/2,
    hover_size = sqrt( (start_x - end_x)^2 + (start_y - end_y)^2 )/2
  )


  if (!weighted_edges) {

    edge_info$weights = 1

  }

  edge_shapes <- list()


  for(i in 1:nrow(edge_info)) {

    edge_shape = list(
      type = "line",
      line = list(color = edge_cols[i], width = edge_info$weights),
      opacity = edge_transparency,
      layer = 'below',
      x0 = edge_info$start_x[i],
      y0 = edge_info$start_y[i],
      x1 = edge_info$end_x[i],
      y1 = edge_info$end_y[i]
    )

    edge_shapes[[i]] <- edge_shape
  }


  #### Edge hovers ####

  edge_info <- edge_info %>% filter(from != to)

  my_network <- my_network %>%
    add_trace(
      x = ~ edge_info$midpoint_x,
      y = ~ edge_info$midpoint_y,
      type = "scatter",
      mode = "markers",
      marker = list(size = edge_info$hover_size, color = edge_cols, opacity = 0),
      text = edge_info$edge_members,
      hoverinfo = "text",
      showlegend = FALSE
    )


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
