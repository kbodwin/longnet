library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(tidyr)
library(netrankr)

# For whatever reason, this is what allows Polish characters to display
Sys.setlocale("LC_ALL", "Polish")

dat <- read_csv("https://www.dropbox.com/s/gl0twbp7ek7vw38/Network_Dates.csv?dl=1") %>% na.omit()
IA_info <- read_csv("https://www.dropbox.com/s/t81y5j8gpg5rw2s/IA_Meta.csv?dl=1") %>% drop_na(IA.Name)
yearVector <- seq(as.Date("1948-01-01"), as.Date("1989-01-01"), by="year")

make_graph <- function(date){
  # Determine which nodes to narrow down to
  keep_nodes <- IA_info %>% select(IA.ID) %>% unlist()
  
  # Make graph from counting co-membership
  my_graph <- dat %>% 
    filter(IA.ID %in% keep_nodes) %>%
    filter(ymd(Start.Date) <= ymd(date), ymd(End.Date) >= ymd(date)) %>%
    select(IA.ID, Member.ID) %>%
    group_by(IA.ID) %>%
    table() %>%
    as.matrix %>%
    tcrossprod() %>%
    graph.adjacency(weighted = TRUE)
}

# date = as.Date("1948-01-01")
# Create a layout object that merge with IA_info so that it's easy to refer to IA information given edges
getIA_Name <- function(graph){
  M <- create_layout(graph, layout = "kk")
  M$name <- as.character(M$name)
  M <- M %>% left_join(IA_info, by = c("name" = "IA.ID"))
  M
}

########################################################
### Test for a single year
g1 <- make_graph(yearVector[1])
L <- getIA_Name(g1)
degree1 <- degree(g1)
pos_max1 <- which(degree1$res == max(degree1$res))

L[pos_max]

########################################################

### Test for multiple years
centralName <- c()

for (i in as.character(yearVector)){
  #print(i)
  g <- make_graph(i)
  l <- getIA_Name(g)
  degree <- centr_degree(g, mode = "all")
  pos_max <- which(degree$res == max(degree$res))

  centralName <- append(centralName, l[pos_max])
}

table(centralName)

########################################################

### Test for year 1989 (1989-09-01 is the last End.Date of data)
yearVector1989 <- seq(as.Date("1988-01-01"), as.Date("1988-12-01"), by="month")
centralName1989 <- c()

for (i in as.character(yearVector1989)){
  #print(i)
  g89 <- make_graph(i)
  l89 <- getIA_Name(g89)
  degree89 <- centr_degree(g89, mode = "all")
  pos_max89 <- which(degree89$res == max(degree89$res))

  centralName1989 <- append(centralName1989, l89[pos_max89])
}

table(centralName1989)

########################################################
# Centrality for PZPR Member
# IA01113: PZPR Member

name <- "PZPR member"
centralPZPR <- c()

# Test for one year
g1 <- make_graph(yearVector[1])
L <- getIA_Name(g1)
degree1 <- centr_degree(g1, mode = "all")
pos_max1 <- which(degree1$res == max(degree1$res))


degree1$res[which(L == "PZPR member")]

for (i in as.character(yearVector)){
  #print(i)
  g <- make_graph(i)
  l <- getIA_Name(g)
  degree <- centr_degree(g, mode = "all")
  pos <- degree$res
  
  if (name %in% l)
    centralPZPR <- append(centralPZPR, pos[which(l == name)])
}

ind <- 1:35
plot(centralPZPR, pch = 16, cex = 1, col = "blue")
abline(lm(centralPZPR ~ ind))

########################################################
# Centrality for NSZZ Solidarnosc Member
# IA00873: NSZZ Solidarnosc Member

name <- "NSZZ Solidarnosc Member"
centralNSZZ <- c()

# Test for one year
g1 <- make_graph(yearVector[1])
L <- getIA_Name(g1)
degree1 <- centr_degree(g1, mode = "all")
pos_max1 <- which(degree1$res == max(degree1$res))


degree1$res[which(L == name)]

for (i in as.character(yearVector)){
  #print(i)
  g <- make_graph(i)
  l <- getIA_Name(g)
  degree <- centr_degree(g, mode = "all")
  pos <- degree$res
  
  if (name %in% l)
    centralNSZZ <- append(centralNSZZ, pos[which(l == name)])
}

ind <- 1:9
plot(centralNSZZ, pch = 16, cex = 1, col = "blue")
abline(lm(centralNSZZ ~ ind))

########################################################
#3.	What is the relationship between individual XXXX and individual YYYY?
# Test for one year
g1 <- make_graph(yearVector[25])
L <- getIA_Name(g1)
degree1 <- centr_degree(g1, mode = "all")
pos_max1 <- which(degree1$res == max(degree1$res))

all_simple_paths(g1, 3, 5)

########################################################
# New test for get most central
degree <- degree(g1)
pos_max <- which(degree == max(degree))


centralIA <- tibble()
for (i in as.character(yearVector[1])){
  #print(i)
  g <- make_graph(i)
  l <- getIA_Name(g)
  degree <- degree(g)
  
  if ("IA00956" %in% names(degree))
    ctlt <- unname(degree[names(degree) == "IA00956"])
  else
    ctlt <- 0
  
  # Get info of specific IAs given the id of the most centrality IAs of the year.
  info <- IA_info[IA_info$IA.ID == "IA00956", c("IA.ID", "IA.Name", "IA.Group.1", "Type")]
  
  # Combine all infomation into 1 tibble
  temp <-
    cbind(info,
          Centrality = ctlt,
          Year = year(i),
          Month = month.name[month(i)])

  centralIA <- rbind(centralIA, temp)

}

centralIA



