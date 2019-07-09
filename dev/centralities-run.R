dat <- read_csv("./data/Full_Data.csv")
IA_meta <- read_csv("./data/IA_Meta.csv")

my_cents <- get_centralities(dat,
                             "IA.ID",
                             "Member.ID",
                             "Start.Date",
                             "End.Date",
                             by = "month",
                             date_orders = "ymd",
                             remove_node = NULL)


my_cents_2 <- my_cents %>% left_join(IA_meta)

write_csv(my_cents_2, "./data/IA_Centralities.csv")
