library(shiny)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)
library(tidyr)
library(shinyWidgets)
library(graphlayouts)

# Link to shiny app: https://hayate.shinyapps.io/IA_Network/

# For whatever reason, this is what allows Polish characters to display
#Sys.setlocale('LC_ALL','C')
Sys.setlocale("LC_ALL", "Polish")

#dat <- read_csv("https://www.dropbox.com/s/gl0twbp7ek7vw38/Network_Dates.csv?dl=1") %>% na.omit()
#IA_info <- read_csv("https://www.dropbox.com/s/t81y5j8gpg5rw2s/IA_Meta.csv?dl=1") %>% drop_na(IA.Name)

dat <- read_csv("https://www.dropbox.com/s/rqb1416klep2evs/Network_Member.csv?dl=1") %>% na.omit()
Member_info <- read_csv("https://www.dropbox.com/s/hxrqamlpx5epwhe/Member_Meta.csv?dl=1") %>% drop_na(Full.Name) %>% unique()

# Function to generate ggplot colors (Original hcl: hues, 65 ,100)
ggcolors <- function(n){
  hues = seq(15, 375, length = n+2)
  hcl(h = hues, l = 65, c = 100)[2:(n+1)]
}

# Function to get a Date value
getDate <- function(year, month, day){
  as.Date(paste(year, month, day, sep = "-"))
}

# Function to filter out IA.ID given the date
getNewData <- function(date){
  dat %>% filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
    select(Member.ID)
}

# Function to get shared member ID of both institutions
getMember <- function(member1, member2, date){
  dat1 <- dat %>% filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
    filter(Member.ID %in% member1) %>% select(IA.Name)
  dat2 <- dat %>% filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
    filter(Member.ID %in% member2) %>% select(IA.Name)
  #print(dat1)
  #print(dat2)
  intersect(dat1$IA.Name, dat2$IA.Name)
}

# Function to get current selection from input
getEvent <- function(input, current_selection){
  #print(input)
  if (is.null(input))
    input <- "None"
  
  observeEvent(input, {
    current_selection(input)
  })
}


make_indiv_network <- function(date, 
                               node_highlight = NA, 
                               gender_highlight = NA,
                               prof_highlight = NA,
                               edge_transparency = 1,
                               node_remove = NA,
                               group_sub = NA, 
                               layout_type = "kk"){
  # print(gender_highlight)
  
  # Determine which nodes to narrow down to
  if(is.na(group_sub)){
    keep_nodes <- Member_info %>%
      filter(Full.Name != node_remove) %>%
      select(Member.ID) %>% unlist()
  } else{
    keep_nodes <- Member_info %>%
      filter(Full.Name != node_remove) %>%
      filter(Profession == group_sub) %>%
      select(Member.ID) %>% unlist()
  }
  
  # Make graph from counting co-membership
  my_graph <- dat %>% 
    filter(Member.ID %in% keep_nodes) %>%
    filter(mdy(Start.Date) <= ymd(date), mdy(End.Date) >= ymd(date)) %>%
    select(Member.ID, IA.ID) %>%
    group_by(Member.ID) %>%
    table() %>%
    as.matrix %>%
    tcrossprod() %>%
    graph.adjacency(weighted = TRUE)
  
  
  # Find layout info
  L <- create_layout(my_graph, layout = layout_type)
  #draw_circle(use = "focus",max.circle = 3)
  
  L$name <- as.integer(as.character(L$name))
  
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
  
  # Vector of Institution Type as number
  # Shiny app treats NA as "", that's why we need to make NA from data becomes empty string
  #L$Type[is.na(L$Type)] <- ""
  gender_as_num <- as.numeric(factor(L$Gender))

  # Get vector of color representing each gender
  color <- ggcolors(2)[gender_as_num]

  # Different node color for the type chosen.
  # Avoid having the same #619CFF color
  #print(gender_highlight)
  color[color == "#619CFF"] <- "red"
  node_cols[L$Gender %in% gender_highlight] <- color[L$Gender %in% gender_highlight]

  #print(paste("Color:", color))
  #print(node_cols)

  # Highlight member's profession
  node_cols[L$Profession == prof_highlight] <- "red"

  # Highlight edges from chosen nodes
  id_highlight <- L$name[L$Profession == prof_highlight]

  edge_cols[which(es$V1 %in% id_highlight)] <- "red"
  edge_cols[which(es$V2 %in% id_highlight)] <- "red"

  #print(paste("Node hl before: ", node_highlight))

  # Highlight individual nodes and edges
  node_cols[match(node_highlight, L$Full.Name)] <- "green"
  id_node_hl <- L$name[match(node_highlight, L$Full.Name)]

  #print(paste("id: ", id_node_hl))
  #print(which(es$V2 %in% id_node_hl))
  edge_cols[which(es$V1 %in% id_node_hl)] <- "green"
  edge_cols[which(es$V2 %in% id_node_hl)] <- "green"
  #print(edge_cols)
  
  ######################
  # Display IAs on edge that are shared by both members
  ######################
  diff_id_index <- which(es$V1 != es$V2)
  id_1 <- es$V1[diff_id_index]
  id_2 <- es$V2[diff_id_index]

  # If there is any edges, we will calculate and display shared IAs on edges
  if (length(diff_id_index) != 0){
    # Vectors to keep xy-coordinates of points between connected institutions
    x_ave <- c()
    y_ave <- c()
    text_edge <- list()

    for (i in 1:length(id_1)){
      # Calculate the x-coordinate between 2 connected points.
      x_ave[i] =  ave(c(L$x[L$name == id_1[i]], L$x[L$name == id_2[i]]))[1]
    }

    for (i in 1:length(id_1)){
      # Calculate the y-coordinate between 2 connected points.
      y_ave[i] =  ave(c(L$y[L$name == id_1[i]], L$y[L$name == id_2[i]]))[1]
    }

    for (i in 1:length(id_1)){
      # Get shared members
      text_edge[[i]] <- getMember(id_1[i], id_2[i], date)

      #print(text_edge[[i]])
      # If there are more than 1 shared members, we need to parse the members into 1 string
      # Because add_trace method can't display a vector in 1 text
      if (length(text_edge[[i]]) > 1){
        text_edge[[i]] <- paste(text_edge[[i]], collapse = '\n')
      }

      #text_edge[[6]] <- paste(text_edge[[6]], collapse = ', ')
    }
  }
  
  ###########################
  network <- plot_ly(x = ~n_x, 
                     y = ~n_y, 
                     type = "scatter",
                     mode = "markers", 
                     marker = list(color = node_cols, size = 10),
                     text = L$Full.Name, 
                     hoverinfo = "text",
                     width = 700, height = 600) %>%
    # This add_annotations is not exactly the feature we want, but the best I can do
    add_annotations(x = ~n_x,
                    y = ~n_y,
                    text = L$Full.Name,
                    visible = FALSE,
                    showarrow = TRUE,
                    clicktoshow = "onoff",
                    font = list(size = "11"))
  
  if (length(diff_id_index) != 0){
    network <- network %>%
      add_trace(x = ~x_ave, y = ~y_ave,
                type = "scatter",
                mode = "markers",
                marker = list(color = "white", size = 0, opacity = 0),
                text = paste(text_edge),
                hoverinfo = "text",
                showlegend = FALSE)
  }
  # 
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  edge_shapes <- list()
  
  
  for(i in 1:length(es$V1)) {
    v0_idx <- which(L$name == es[i,]$V1)
    v1_idx <- which(L$name == es[i,]$V2)
    
    edge_shape = list(
      type = "line",
      line = list(color = edge_cols[i], width = 1),
      opacity = edge_transparency,
      x0 = n_x[v0_idx],
      y0 = n_y[v0_idx],
      x1 = n_x[v1_idx],
      y1 = n_y[v1_idx]
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  
  layout(
    network,
    title = date,
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis
  )
  
}

# Define UI for application
ui <- pageWithSidebar(
  
  headerPanel("Connections between Polish individuals"),
  
  sidebarPanel(
    radioButtons('Day', 'Day',
                 choices = c(
                   "Start of Month" = 01,
                   "Mid Month" = 15)
    ),
    sliderInput('Month', 'Month', 1,
                min = 1, max = 12),
    sliderInput('Year', 'Year', 1957,
                min = 1948, max = 1989),
    
    # selectInput('node_highlight', 'Highlight an institution',
    #             choices = c("None", unique(as.character(IA_info$Full.Name)))
    # ),
    
    # Choosing between highlighting type or group
    radioButtons('gender_prof', label = 'Highlight',
                 choices = list("Gender" = 1, "Profession" = 2), 
                 selected = 1),
    
    # selectInput appears for inputing type highlight
    conditionalPanel(
      condition = "input.gender_prof == 1",
      # selectInput('gender_highlight', 'Highlight gender of member',
      #             choices = c("None", unique(as.character(dat$Gender[!is.na(dat$Gender)])))
      # )
      
      radioButtons('gender_highlight', label = 'Highlight gender of members',
                   choices = list("None" = 1, "Male" = "M", "Female" = "F"), 
                   selected = 1)
      # pickerInput(inputId = "gender_highlight",
      #             label = "Highlight gender of member",
      #             choices = unique(as.character(IA_info$Type[!is.na(IA_info$Type)])),
      #             options = list(`actions-box` = TRUE), multiple = T)
    ),
    
    # selectInput appears for inputing group highlight
    conditionalPanel(
      condition = "input.gender_prof == 2",
      selectInput('prof_highlight', 'Highlight a profession of members', 
                  choices = c("None", sort(unique(Member_info$Profession))))
    ),
    
    # Highlight multiple individuals node
    pickerInput(inputId = "node_highlight",
                label = "Highlight individual member",
                choices = sort(Member_info$Full.Name),
                options = list(`actions-box` = TRUE), multiple = T),
    
    # Selecting edge transparency
    sliderInput('edge', 'Edge transparency', value = 1,
                min = 0, max = 1),
    
    # Removing a node
    selectInput('node_remove', 'Ignoring a member',
                choices = c("None", sort(Member_info$Full.Name)))
    
  ),
  
  mainPanel(
    plotlyOutput('plot1')
    #textOutput("text")
  )
  
)


server <- function(input, output, session) {
  # create reactiveVal to retain previously selected values
  # cur_gender <- reactiveVal(NULL)
  # cur_prof <- reactiveVal(NULL)
  # cur_highlight <- reactiveVal(NULL)
  # 
  # # Change values for input$type
  # observe({
  #   date <- getDate(input$Year, input$Month, input$Day)
  #   newDat <- getNewData(date)
  #   #print(newDat)
  # 
  #   newInfo <- Member_info %>% filter(Member.ID %in% newDat$Member.ID)
  # 
  #   getEvent(input$gender_highlight, cur_gender)
  #   getEvent(input$prof_highlight, cur_prof)
  #   getEvent(input$node_highlight, cur_highlight)
  # 
  #   if (input$gender_prof == 1 ){
  #     #updateRadioButtons(session, 'gender_highlight', choices = unique(as.character(newInfo$Type)), selected = cur_gender())
  #     #updateSelectInput(session, "gender_highlight", choices = c("None", unique(as.character(newInfo$Type))))
  #     updateSelectInput(session, "prof_highlight", choices = "None" )
  #   }
  #   else{
  #     updateRadioButtons(session, "gender_highlight", choices = "None", selected = "None")
  #     updateSelectInput(session, "prof_highlight", choices = c("None", unique(as.character(newInfo$Profession))),
  #                       selected = cur_prof())
  #   }
  # 
  #   updatePickerInput(session, "node_highlight", choices = newInfo$Full.Name, selected = cur_highlight())
  #   updateSelectInput(session, "node_remove", choices = c("None", newInfo$Full.Name))
  # })

  # output$text <- renderText({
  #   paste0("Check: .", input$node_highlight)
  # })
  #
  
  # Plotly
  output$plot1 <- renderPlotly({
    make_indiv_network(
      date = paste(input$Year, input$Month, input$Day, sep = "-"),
      node_highlight = input$node_highlight,
      gender_highlight = input$gender_highlight,
      prof_highlight = input$prof_highlight,
      edge_transparency = input$edge,
      node_remove = input$node_remove
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



