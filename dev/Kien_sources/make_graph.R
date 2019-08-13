library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(tidyr)
library(netrankr)
library(zoo)
library(gridExtra)

make_graph <- function(date, node_remove = NA){
  # Determine which nodes to narrow down to
  #keep_nodes <- IA_info %>% select(IA.ID) %>% unlist()
  if(is.na(node_remove)){
    keep_nodes <- IA_info %>% select(IA.ID) %>% unlist()
  }else{
    keep_nodes <- IA_info %>%
      filter(IA.ID != node_remove) %>%
      select(IA.ID) %>% unlist()
  }
  
  # Make graph from counting co-membership
  my_graph <- dat %>% 
    filter(IA.ID %in% keep_nodes) %>%
    filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
    select(IA.ID, Member.ID) %>%
    group_by(IA.ID) %>%
    table() %>%
    as.matrix %>%
    tcrossprod() %>%
    graph.adjacency(weighted = TRUE)
}

getIA_Name <- function(graph){
  M <- create_layout(graph, layout = "kk")
  M$name <- as.character(M$name)
  M <- M %>% left_join(IA_info, by = c("name" = "IA.ID"))
  M
}