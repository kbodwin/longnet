library(dplyr)
library(igraph)
library(lubridate)
library(rlang)
library(purrr)

# # testing only
# library(readr)
#
# data <- read_csv("./Data/Full_Data.csv")
#
# node_var = "IA.ID"
# edge_var = "Member.ID"
# start_date_var = "Start.Date"
# end_date_var = "End.Date"
# first_date = "1970-01-01"
# last_date = NA
# date_orders = "ymd"
# node_labels = "IA.Name"
# edge_labels = "Full.Name"
# # node_remove = NULL
# edge_remove = NULL

#
#
make_graph_df <- function(data,
                       node_var, edge_var,
                       start_date_var = NULL, end_date_var = NULL,
                       first_date = NA, last_date = NA,
                       date_orders = "ymd",
                       edge_labels = edge_var,
                       node_remove = NULL, edge_remove = NULL){

  # start_date_var and end_date_var are column names

  # first_date and last_date should not both be NA.
  ## If they are, give a warning and use the whole dataset.

  # node_var and edge_var specify the variable names in the dataset to use
  ## eventually I should tidyeval this

  # node_labels and edge_labels optionally specify columns with better names
  ## REMOVED just attach metas

  # node_remove and edge_remove are lists with
  ## [[1]] name of dataset column to reference,
  ## [[2]] vector of values to omit


  #### Filter dataset to relevant date range, and optional omissions ####

  drop <- rep(FALSE, nrow(data))

  if (!is.null(node_remove)) {

    drop <- drop | data[[ node_remove[[1]] ]] %in% node_remove[[2]]

  }

  if (!is.null(edge_remove)) {

    drop <- drop | data[[ edge_remove[[1]] ]] %in% edge_remove[[2]]

  }

  ## need to throw some errors here if weird stuff is supplied.
  ## being lazy for now because it's my data I won't be stupid (hopefully)
  ## start date var is required; end date will be considered start date if not supplied

  if (!is.null(start_date_var)) {

    data[[start_date_var]] <- lubridate::parse_date_time(data[[start_date_var]], date_orders)

    if (!is.null(end_date_var)) {

      data[[end_date_var]] <- lubridate::parse_date_time(data[[end_date_var]], date_orders)

    } else {

      end_date_var <- "end_date"
      data[[end_date_var]] <- data[[start_date_var]]

    }

    if (is.na(first_date)) {

      first_date = min(data[[start_date_var]])

    } else {

      first_date = lubridate::parse_date_time(first_date, orders = date_orders)

    }

    if (is.na(last_date)) {

      last_date = max(data[[end_date_var]])

    } else {

      last_date = lubridate::parse_date_time(last_date, orders = date_orders)

    }

    drop <- drop | data[[end_date_var]] < first_date | data[[start_date_var]] > last_date

  }

  # Aaand, drop 'em
  data <- data[!drop, ]

  #### Time to make a graph! ####


  # Make graph from counting co-membership
  my_graph <- data[,c(node_var, edge_var)] %>%
    dplyr::group_by(.data[[node_var]]) %>%
    table() %>%
    as.matrix %>%
    tcrossprod() %>%
    graph_from_adjacency_matrix(weighted = TRUE) %>%
    get.data.frame()


  #### Add edge info ####

  data <- data %>% distinct(!!sym(node_var), !!sym(edge_var), !!sym(edge_labels))

  my_graph <- my_graph %>%
    mutate(
      edge_members = map2_chr(to, from, ~find_edge_members(data, node_var, edge_var, edge_labels, .x, .y))
      )

  # Attach names and meta-info to data frame
    return(my_graph)
  }
