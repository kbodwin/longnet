library(shiny)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)
library(tidyr)
library(shinyWidgets)
#library(graphlayouts)

# Link to shiny app:

# For whatever reason, this is what allows Polish characters to display
Sys.setlocale("LC_ALL", "Polish")


# Some prep

dat <- read_csv("/Users/kelly/Dropbox/longnet/data/Full_Data.csv")

## only keep the first instance of each ID, in case of unmatches
IA_info <- read_csv("/Users/kelly/Dropbox/longnet/data/IA_Meta.csv") %>%
  distinct(IA.ID, .keep_all = TRUE)
Mem_info <- read_csv("/Users/kelly/Dropbox/longnet/data/Member_Meta.csv") %>%
  distinct(Member.ID, .keep_all = TRUE)

node_choices <- c(IA_info$IA.ID)
names(node_choices) <- c(IA_info$IA.Name)

group_choices <- c(unique(as.character(IA_info$Type)))
grouping_var <- "Type"

network_type <- "kk"

node_var <- "IA.ID"
node_labels <- "IA.Name"
edge_var <- "Member.ID"
edge_labels <- "Full.Name"

# would be inputs
Year <- 1979
Month <- 01
Day <- 01

node_remove = NULL
edge_remove = NULL

prev_layout = NULL

color_by_group = "None"
group_highlight = NULL
node_highlight = NULL

  #### Get Selected Dates ####
  first_date <- get_date(Year, Month, Day)

  #### Make Graph ####
  my_graph <- make_graph_df(dat,
               "IA.ID", "Member.ID",
               "Start.Date", "End.Date",
               first_date, first_date,
               date_orders = "ymd",
               node_labels = node_labels,
               edge_labels = edge_labels,
               node_remove = node_remove,
               edge_remove = edge_remove)

  #### Calculate layout ####

  my_layout <- get_layout_df(my_graph,
                  node_meta = IA_info,
                  node_var = node_var,
                  prev_layout = prev_layout,
                  algorithm = network_type)

  prev_layout <- my_layout

  #### Set node and edge details ####

  node_cols <- make_node_cols(my_layout,
                   node_var,
                   grouping_var = color_by_group,
                   color_all_groups = color_by_group != "None",
                   highlight_groups = group_highlight,
                   highlight_nodes = node_highlight)


  edge_cols <-  make_edge_cols(my_graph,
                   highlight_nodes = node_highlight)

  #### Plot it ####

    make_network_plot(my_graph,
                      my_layout,
                      first_date,
                      node_label_var = node_labels,
                      node_cols = node_cols,
                      edge_cols = edge_cols,
                      weighted_edges = TRUE,
                      edge_transparency = 1)


