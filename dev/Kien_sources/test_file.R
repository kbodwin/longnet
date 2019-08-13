# dat <- ia %>%
#   filter(IA.Name == "") %>%
#   select(IA.Name, Type, Full.Name, IA.Group.1, IA.Group.2)

# First problem
d1 = as.Date("1935-01-01")
d2 = as.Date("1979-01-01")
date = d1

ggcolors(4)

n = 8
hues = seq(15, 375, length = n+2)
hcl(h = hues, l = 65, c = 100)[2:(n+1)]

dat2 <- read_csv("https://www.dropbox.com/s/gl0twbp7ek7vw38/Network_Dates.csv?dl=1") %>% na.omit()
IA_info <- read_csv("https://www.dropbox.com/s/t81y5j8gpg5rw2s/IA_Meta.csv?dl=1") %>% drop_na(IA.Name)
keep_nodes <- IA_info %>% select(IA.ID) %>% unlist()

my_graph <- dat2 %>% 
  filter(IA.ID %in% keep_nodes2) %>%
  filter(ymd(Start.Date) <= ymd(date), ymd(End.Date) >= ymd(date)) %>%
  select(IA.ID, Member.ID) %>%
  group_by(IA.ID) %>%
  table() # %>%
  # as.matrix %>%
  # tcrossprod() %>%
  # graph.adjacency(weighted = TRUE)

#es <- as.data.frame(get.edgelist(my_graph))

L <- create_layout(my_graph, layout = "kk")
L$name = as.character(L$name)

# Attach relevant node info
M <- L %>% left_join(IA_info, by = c("name" = "IA.ID"))
L <- L %>% left_join(IA_info, by = c("name" = "IA.ID"))

type = "Postsecondary School"
M %>% filter(Type == type)

name = M$name
IA_info %>% filter(IA.ID %in% name)

ncolor <- ggcolors(5)
df <- as.numeric(factor(M$Type))
r = rep("black", 6)

r[M$Type == type] = ncolor[df][M$Type == type]

# Second problem (IA Group)
ida = M$name[M$IA.Group.1 == "LU"]
edge_cols <- rep("black", 31)

# Third problem (Individual nodes)
nodehl = c("Lódz University Faculty of Humanities Sociology Institute", "Lódz University Faculty of Pedagogy",
           "Lódz University Faculty of Law and Economics", "PZPR Central Committee Alternate Member")

# Fourth problem, lower priority, hover option over edges
diff_id_index = which(es$V1 != es$V2)
id1 = es$V1[diff_id_index]
id2 = es$V2[diff_id_index]

M$x[M$name == id1[1]]
M$x[M$name == id2[1]]

find_loc <- function(id, x = TRUE){
  if (x)
    M$x[M$name == id]
  else 
    M$y[M$name == id]
}

find_ave <- function(id_1, id_2, x = TRUE){
  loc = c()
  if (x){
    for (i in 1:length(id_1)){
      loc[i] =  ave(c(M$x[M$name == id_1[i]], M$x[M$name == id_2[i]]))[1]
    }
  } else
    for (i in 1:length(id_1)){
      loc[i] =  ave(c(M$y[M$name == id_1[i]], M$y[M$name == id_2[i]]))[1]
    }
  return(loc)
}

x_loc = find_ave(id1, id2)
y_loc = find_ave(id1, id2, x = FALSE)

for (i in 1:length(id1)){
  print(getMember(id1[i], id2[i]))
}


# 5th problem, find centrality
#erdos.gr <- sample_gnm(n=10, m=25) 
degree.cent <- centr_degree(my_graph, mode = "all")
#degree.eigen <- centr_eigen(my_graph, directed = T)

########################################################
# Make graph from counting co-membership
d1 = as.Date("1957-01-01")
date = d1
keep_nodes <- dat %>% select(Member.ID) %>% unlist()

my_graph <- dat %>% 
  filter(Member.ID %in% keep_nodes) %>%
  filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
  select(Member.ID, IA.ID) %>%
  group_by(Member.ID) %>%
  table() %>%
  as.matrix %>%
  tcrossprod() %>%
  graph.adjacency(weighted = TRUE)

L <- create_layout(my_graph, layout = layout_type)
#draw_circle(use = "focus",max.circle = 3)

L$name <- as.integer(L$name)

# Attach relevant node info
L <- L %>% left_join(Member_info, by = c("name" = "Member.ID"))

# Raw locations of nodes
n_x <- L$x
n_y <- L$y

# Edge information
es <- as.data.frame(get.edgelist(my_graph))

# Size of network
n_node <- nrow(L)
n_edge <- nrow(es)

# Default edge cols
edge_cols <- rep("black", n_edge)

# Set default node color (blue)
node_cols <- rep("#619CFF", n_node)


dat %>% filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
  select(Member.ID)

########################################################
get_centrality_by_month_between("IA00873", nszzTime)


my_graph <- dat %>% 
  filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
  select(IA.ID, Member.ID)

info <- IA_info[IA_info$IA.ID %in% kikID, c("IA.ID", "IA.Name", "IA.Group.1", "Type")]

if (any(kikID %in% names(degree))){
  print("yes")
  ctlt <- unname(degree[names(degree) %in% kikID])
} else{
  ctlt <- 0
}














