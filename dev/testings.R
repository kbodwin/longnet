library(ggraph)

dat <- read_csv("./Data/Full_Data.csv")

bob <- make_graph_df(dat,
                  node_var = "IA.ID",
                  edge_var = "Member.ID",
                  start_date_var = "Start.Date",
                  end_date_var = "End.Date",
                  first_date = "1970-01-01",
                  last_date = "1970-01-01",
                  edge_labels = "Full.Name",
                  node_labels = "IA.Name")


prev_layout <- create_layout(graph_from_data_frame(bob), layout = "igraph", algorithm = "kk")

start <- proc.time()

ggraph(prev_layout) +
  geom_edge_link(aes(edge_width = bob$weight)) +
  geom_node_text(aes(label = name)) +
  theme_graph()

proc.time() - start


plot_ly(x = ~prev_layout$x,
        y = ~prev_layout$y,
        type = "scatter",
        mode = "markers",
        marker = list(size = 20),
        text = prev_layout$name,
        hoverinfo = "text",
        width = 700, height = 600)

proc.time() - start


new_graph <- make_graph_df(dat,
                        node_var = "IA.ID",
                        edge_var = "Member.ID",
                        start_date_var = "Start.Date",
                        end_date_var = "End.Date",
                        first_date = "1982-01-01",
                        last_date = "1982-01-01")

match_layout <- layout_from_previous(new_graph, prev_layout)

## replace NAs with 0s or random or whatevs.

new_graph <- graph_from_data_frame(new_graph)



start = proc.time()

new_layout <- create_layout(new_graph, layout = "igraph", algorithm = "kk", coords = match_layout)

ggraph(new_layout) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

t1 = proc.time()

ggraph(new_graph, "kk") +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

t2 = proc.time()

ggplot(new_layout) +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

t3 = proc.time()

plot_ly(x = ~new_layout$x,
        y = ~new_layout$y,
        type = "scatter",
        mode = "markers",
        marker = list(size = 20),
        text = new_layout$name,
        hoverinfo = "text",
        width = 700, height = 600)

t4 = proc.time()

t1 - start
t2 - start
t3 - start
