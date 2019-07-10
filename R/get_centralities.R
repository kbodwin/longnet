library(lubridate)
library(purrr)

#
# # testing only
# library(readr)
#
# data <- read_csv("./data/Full_Data.csv")
# node_meta <- read_csv("./data/IA_Meta.csv")
#
# node_var = "IA.ID"
# edge_var = "Member.ID"
# start_date_var = "Start.Date"
# end_date_var = "End.Date"
# start_date = "1970-01-01"
# end_date = "1970-12-01"
# date_orders = "ymd"
# node_labels = "IA.Name"
# edge_labels = "Full.Name"
# # node_remove = NULL
# edge_remove = NULL
# date_today = ymd("1970-01-01")
# which_nodes = unique(data$IA.ID)


get_centralities <- function(data,
                             node_var,
                             edge_var,
                             start_date_var,
                             end_date_var = NULL,
                             by = "month",
                             date_orders = "ymd",
                             remove_node = NULL) {

  #### Setup ####

  # Make sure dates are lubridates
  # Make sure dates are dates

  if (!assertthat::is.date(data[[start_date_var]])) {

    data[[start_date_var]] <-
      lubridate::parse_date_time(data[[start_date_var]], date_orders)

  }

  if (!is.null(end_date_var) &&
      !assertthat::is.date(data[[end_date_var]])) {

    data[[end_date_var]] <-
      lubridate::parse_date_time(data[[end_date_var]], date_orders)

  }


  # If only start date supplied, end date is same
  if (is.null(end_date_var)) {

    end_date_var <- "end_date"
    data[[end_date_var]] <- data[[start_date_var]]

  }

  # Find first and last date in the dataset

  earliest_date <- min(data[[start_date_var]])
  latest_date <- max(data[[end_date_var]])

  # Make a vector of month start dates that covers the full time period.

  dates_vec <- seq(from=floor_date(earliest_date, "months"), to=ceiling_date(latest_date, "months"), by='month')

  #### Centrality measures ####

  # For each day, get info for all the nodes
  my_centralities <- map_df(dates_vec,
      ~ get_centralities_today(data,
                           node_var, edge_var,
                           start_date_var, end_date_var,
                           start_date  = ymd(.x),
                           end_date = ymd(.x) + months(1)
      )
    )

  return(my_centralities)

}


get_centralities_today <- function(data,
                                   node_var, edge_var,
                                   start_date_var, end_date_var,
                                   start_date, end_date = start_date) {


  my_graph_df <- make_graph_df(data,
                            node_var, edge_var,
                            start_date_var, end_date_var,
                            start_date, end_date)

  if (nrow(my_graph_df) == 0) {

    return(NULL)

  }

  my_graph <- graph_from_data_frame(my_graph_df)

  my_degree <- degree(my_graph, loops = FALSE) %>%
    bind_rows() %>%
    gather(key = nodes, value = Degree)


  my_betweenness <- betweenness(my_graph, directed = FALSE) %>%
    bind_rows() %>%
    gather(key = nodes, value = Betweenness)

  my_membership <- map_dbl(my_degree$nodes,
                           ~get_membership_count(my_graph_df, .x)
                    )

  if (ncol(my_betweenness) == 0) {

    my_return_df <- my_degree %>%
      mutate(
        Betweenness = 0,
        Self_Degree = my_membership,
        Date = start_date
      )

  } else {

    my_return_df <- full_join(my_degree, my_betweenness) %>%
      mutate(
        Self_Degree = my_membership,
        Date = start_date
      )

  }


  names(my_return_df) <- c(node_var, "Degree", "Betweenness", "Self_Degree", "Date")


  return(my_return_df)


}


get_membership_count <- function(graph, node){

  # Edges
  where_node <- graph$from == node &  graph$to == node

  if (sum(where_node) == 0) {

    return(0)

  } else {

    return(graph$weight[where_node][1])

  }

}


