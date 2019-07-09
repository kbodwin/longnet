library(lubridate)
library(purrr)


# testing only
library(readr)

data <- read_csv("./data/Full_Data.csv")
node_meta <- read_csv("./data/IA_Meta.csv")

node_var = "IA.ID"
edge_var = "Member.ID"
start_date_var = "Start.Date"
end_date_var = "End.Date"
first_date = "1970-01-01"
last_date = NA
date_orders = "ymd"
node_labels = "IA.Name"
edge_labels = "Full.Name"
# node_remove = NULL
edge_remove = NULL
#date_today = ymd("1970-01-01")
#which_nodes = unique(data$IA.ID)


get_centralities <- function(data,
                             node_var,
                             edge_var,
                             start_date_var,
                             end_date_var,
                             by = "day",
                             date_orders = "ymd",
                             remove_node = NULL) {

  #### Setup ####

  # Make sure dates are lubridates
  if (!is.null(start_date_var)) {

    data[[start_date_var]] <- lubridate::parse_date_time(data[[start_date_var]], date_orders)

    if (!is.null(end_date_var)) {

      data[[end_date_var]] <- lubridate::parse_date_time(data[[end_date_var]], date_orders)

    } else {

      end_date_var <- "end_date"
      data[[end_date_var]] <- data[[start_date_var]]

    }

  }

  # Find first and last date in the dataset

  earliest_date <- min(data[[start_date_var]])
  latest_date <- max(data[[end_date_var]])

  # Make a vector of month start dates that covers the full time period.
  num_days <- lubridate::interval(latest_date, earliest_date) %/% days(1)

  dates_vec <- earliest_date + days(1:num_days)

  #### Centrality measures ####

  # For each day, get info for all the nodes
  my_centralities <- map_df(dates_vec,
      ~ get_centralities_today(data,
                           node_var, edge_var, start_date_var, end_date_var,
                           date_today  = .x
      )
    )

  return(my_centralities)

}


get_centralities_today <- function(data,
                                   node_var, edge_var,
                                   start_date_var, end_date_var,
                                   date_today) {


  my_graph_df <- make_graph_df(data,
                            node_var, edge_var,
                            start_date_var, end_date_var,
                            first_date = date_today, last_date = date_today)


  my_graph <- graph_from_data_frame(my_graph_df)

  my_betweenness <- estimate_betweenness(my_graph,
                                         cutoff = 0)

  my_degree <- degree(my_graph)



  my_membership <- map_dbl(names(my_betweenness),
                           ~get_membership_count(my_graph_df, .x)
                    )


  my_return_df <- data.frame(cbind(names(my_betweenness), my_betweenness, my_degree, my_degree))

  names(my_return_df) <- c(node_var, "Betweenness", "Degree", "Self_Degree")

  my_return_df$Date = rep(date_today, nrow(my_return_df))


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


