library(tidyverse)
library(lubridate)
### one function
get_centrality_by_month <- function(IA_id = NULL, IA_group = NULL, IA_type = NULL, node_remove = NA){
  monthVector <- seq(as.Date("1948-01-01"), as.Date("1989-09-01"), by="month")
  
  ### gets it for all dates
  ### one column called "date" that is a datetime object
  ### Betweenness and Degree as two columns
  
  if (!is.null(IA_group)){
    IA_id <- (IA_info %>% filter(IA.Group.1 == IA_group) %>% select(IA.ID)) %>% unlist()
  } else if (!is.null(IA_type)){
    IA_id <- IA_info %>% filter(Type == IA_type) %>% select(IA.ID) %>% unlist()
  }
  
  df <- tibble()
  
  for (i in as.character(monthVector)){
    g <- make_graph(i, node_remove)
    l <- getIA_Name(g)
    
    # Get the centrality
    degree <- degree(g) #
    between <- betweenness(g, directed = F)
    
    ctlt_degree <- numeric(length(IA_id))
    ctlt_between <- numeric(length(IA_id))
    
    for (j in 1:length(IA_id)){
      #print(IA_id[j])
      
      if (IA_id[j] %in% names(degree))
        ctlt_degree[j] <- unname(degree[names(degree) == IA_id[j]])
      else
        ctlt_degree[j] <- 0
      
      if (IA_id[j] %in% names(between))
        ctlt_between[j] <- unname(between[names(between) == IA_id[j]])
      else
        ctlt_between[j] <- 0
      
      # Get info of specific IAs given the id of the most centrality IAs of the year.
      info <- IA_info[IA_info$IA.ID == IA_id[j], c("IA.ID", "IA.Name", "IA.Group.1", "Type")]
      
      temp <- cbind(info,
                    Degree_Centrality = ctlt_degree[j],
                    Betweeness_Centrality = ctlt_between[j],
                    Year = year(i),
                    Month = month.name[month(i)])
      
      #print("haha")
      df <- rbind(df, temp)
    }
    
    # if (IA_id %in% names(degree))
    #   ctlt_degree <- unname(degree[names(degree) == IA_id])
    # else
    #   ctlt_degree <- 0
    # 
    # if (IA_id %in% names(between))
    #   ctlt_between <- unname(between[names(between) == IA_id])
    # else
    #   ctlt_between <- 0
    
    # Get info of specific IAs given the id of the most centrality IAs of the year.
    # info <- IA_info[IA_info$IA.ID == IA_id, c("IA.ID", "IA.Name", "IA.Group.1", "Type")]
    # 
    # # Combine all infomation into 1 tibble
    # temp <- cbind(info,
    #               Degree_Centrality = ctlt_degree,
    #               Betweeness_Centrality = ctlt_between,
    #               Year = year(i),
    #               Month = month.name[month(i)])
    # 
    # df <- rbind(df, temp)
  }

  # Reorder columns
  df <- df[c("Year", "Month", "Degree_Centrality", "Betweeness_Centrality", "IA.ID", "IA.Name", "IA.Group.1", "Type")]
  
  # Rename columns
  names(df)[c(7, 8)]<- c("IA.Group", "Type")
  
  df <- df %>% mutate(
    Date = make_date(year = Year, month = Month)
  )
    
  return(as_tibble(df))
}



# get_centrality_by_month_degree <- function(IA_id, date, node_remove = NA){
#   
#   df <- tibble()
#   
#   for (i in as.character(date)){
#     g <- make_graph(i, node_remove)
#     l <- getIA_Name(g)
#     
#     # Get the centrality
#     degree <- degree(g) #
#     
#     if (IA_id %in% names(degree))
#       ctlt <- unname(degree[names(degree) == IA_id])
#     else
#       ctlt <- 0
#     
#     # Get info of specific IAs given the id of the most centrality IAs of the year.
#     info <- IA_info[IA_info$IA.ID == IA_id, c("IA.ID", "IA.Name", "IA.Group.1", "Type")]
#     
#     # Combine all infomation into 1 tibble
#     temp <- cbind(info,
#                   Centrality = ctlt,
#                   Year = year(i),
#                   Month = month.name[month(i)])
#     
#     df <- rbind(df, temp)
#   }
#   # Reorder columns
#   df <- df[c("Year", "Month", "Centrality", "IA.ID", "IA.Name", "IA.Group.1", "Type")]
#   
#   # Rename columns
#   names(df)[c(6,7)]<- c("IA.Group", "Type")
#   
#   return(as_tibble(df))
# }
# 
# 
# 
# get_centrality_by_month_between <- function(IA_id, date, node_remove = NA){
#   df <- tibble()
#   
#   for (i in as.character(date)){
#     g <- make_graph(i, node_remove)
#     l <- getIA_Name(g)
#     
#     # Get the centrality
#     degree <- betweenness(g, directed = F) #
#     
#     if (IA_id %in% names(degree))
#       ctlt <- unname(degree[names(degree) == IA_id])
#     else
#       ctlt <- 0
#     
#     #print("haha")
#     # Get info of specific IAs given the id of the most centrality IAs of the year.
#     info <- IA_info[IA_info$IA.ID %in% IA_id, c("IA.ID", "IA.Name", "IA.Group.1", "Type")]
#     
#     # Combine all infomation into 1 tibble
#     temp <- cbind(info,
#                   Centrality = ctlt,
#                   Year = year(i),
#                   Month = month.name[month(i)])
#     
#     #print("aaa")
#     df <- rbind(df, temp)
#   }
#   # Reorder columns
#   df <- df[c("Year", "Month", "Centrality", "IA.ID", "IA.Name", "IA.Group.1", "Type")]
#   
#   # Rename columns
#   names(df)[c(6,7)]<- c("IA.Group", "Type")
#   
#   return(as_tibble(df))
# }
# 
