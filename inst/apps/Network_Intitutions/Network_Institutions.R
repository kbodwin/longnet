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


# Function to get current selection from input
getEvent <- function(input, current_selection){
  #print(input)
  if (is.null(input))
    input <- "None"

  observeEvent(input, {
    current_selection(input)
  })
}



# Some prep

dat <- read_csv("/Users/kelly/Dropbox/longnet/data/Full_Data.csv")
IA_info <- read_csv("/Users/kelly/Dropbox/longnet/data/IA_Meta.csv")
Mem_info <- read_csv("/Users/kelly/Dropbox/longnet/data/Member_Meta.csv")

node_choices <- c(IA_info$IA.ID)
names(node_choices) <- c(IA_info$IA.Name)

group_choices <- c(unique(as.character(IA_info$Type)))
grouping_var <- "Type"

network_type <- "kk"

node_var <- "IA.ID"
node_labels <- "IA.Name"
edge_var <- "Member.ID"
edge_labels <- "Full.Name"

#   %>%
#     # This add_annotations is not exactly the feature we want, but the best I can do
#     add_annotations(x = ~n_x,
#                     y = ~n_y,
#                     text = L$IA.Name,
#                     visible = FALSE,
#                     showarrow = TRUE,
#                     clicktoshow = "onoff",
#                     font = list(size = "11"))
#
#   if (length(diff_id_index) != 0){
#     network <- network %>%
#       add_trace(x = ~x_ave, y = ~y_ave,
#                 type = "scatter",
#                 mode = "markers",
#                 marker = list(color = "white", size = 0, opacity = 0),
#                 text = paste(text_edge),
#                 hoverinfo = "text",
#                 showlegend = FALSE)
#   }
#
#   ####################### Lower priority
#
#   # Hover option on edges
#   ## Hover text shows members shared by those institutions
#
#   #######################
#
#
#   axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
#
#   edge_shapes <- list()
#
#
#   for(i in 1:length(es$V1)) {
#     v0_idx <- which(L$name == es[i,]$V1)
#     v1_idx <- which(L$name == es[i,]$V2)
#
#     edge_shape = list(
#       type = "line",
#       line = list(color = edge_cols[i], width = 1),
#       opacity = edge_transparency,
#       x0 = n_x[v0_idx],
#       y0 = n_y[v0_idx],
#       x1 = n_x[v1_idx],
#       y1 = n_y[v1_idx]
#     )
#
#     edge_shapes[[i]] <- edge_shape
#   }
#
#
#   layout(
#     network,
#     title = date,
#     shapes = edge_shapes,
#     xaxis = axis,
#     yaxis = axis
#   )
#
#
# }

# Define UI for application
ui <- pageWithSidebar(

  headerPanel("Connections between Polish institutions"),

  sidebarPanel(
    radioButtons('Day',
                 'Day',
                 choices = c(
                   "Start of Month" = 01,
                   "Mid Month" = 15
                   )
    ),

    sliderInput('Month',
                'Month',
                value = 1,
                min = 1, max = 12),

    sliderInput('Year',
                'Year',
                value = 1979,
                min = 1945, max = 1989,
                sep = ""),

    # Highlight a node
    pickerInput('node_highlight',
                'Highlight Institution(s)',
                choices = node_choices,
                options = list(`actions-box` = TRUE),
                multiple = T
    ),

    # Highlight groups
    pickerInput('group_highlight',
                'Highlight Type of Institution',
                choices = group_choices,
                options = list(`actions-box` = TRUE),
                multiple = T
    ),

    # Color groups
    radioButtons('color_by_group',
                 'Color nodes by:',
                 choices = c(
                   "None" = "None",
                   "Meta-Institution" = "IA.Group.1",
                   "Type" = "Type",
                   "Voivodship" = "Voivodship",
                   "City" = "City")
    ),


    # Selecting edge transparency
    sliderInput('edge_transparency',
                'Edge transparency',
                value = 1,
                min = 0, max = 1),

    # Removing a node
    pickerInput('node_remove',
                'Remove Node(s)',
                choices = node_choices,
                options = list(`actions-box` = TRUE),
                multiple = T
    ),

    # Removing a member
    pickerInput('edge_remove',
                'Omit an Individual from Edge Calculation',
                choices = c(Mem_info$Full.Name),
                options = list(`actions-box` = TRUE),
                multiple = T
    )

  ),

  mainPanel(
    #textOutput('checking_vars'),
    plotlyOutput('my_network')
    #textOutput("text")
  )

)


server <- function(input, output, session) {

  # Day, Month, Year
  # node_highlight, group_highlight, color_by_group
  # edge_transparency, node_remove, edge_remove

  # output$checking_vars <- renderText({
  #   print(input$node_highlight)
  #   print(input$group_highlight)
  #   print(input$color_by_group)
  # })

  # Create reactiveVal to retain previously selected values
  cur_type <- reactiveVal(NULL)
  cur_group <- reactiveVal(NULL)
  cur_highlight <- reactiveVal(NULL)

  # Create reactive to retain previous graph for faster plotting
  #prev_layout <- reactiveValues(x = 0, y = 0, name = "")

  #### Get Selected Dates ####
  first_date <- reactive({
    get_date(input$Year, input$Month, input$Day)
  })

  #### Make Graph ####
  my_graph <- reactive({
    make_graph_df(dat,
               "IA.ID", "Member.ID",
               "Start.Date", "End.Date",
               first_date(), first_date(),
               date_orders = "ymd",
               node_labels = node_labels,
               edge_labels = edge_labels,
               node_remove = input$node_remove,
               edge_remove = input$edge_remove)
  })

  #### Calculate layout ####
  prev_layout <- NULL

  my_layout <- reactive({
    get_layout_df(my_graph(),
                  node_meta = IA_info,
                  node_var = node_var,
                  prev_layout = prev_layout,
                  algorithm = network_type)
  })

  observeEvent(my_layout(),
               {
                 prev_layout <- isolate(my_layout())
               })

  #### Set node and edge details ####

  node_cols <- reactive({
    make_node_cols(my_layout(),
                   node_var,
                   grouping_var = input$color_by_group,
                   color_all_groups = input$color_by_group != "None",
                   highlight_groups = input$group_highlight,
                   highlight_nodes = input$node_highlight)
  })


  edge_cols <- reactive({
    make_edge_cols(my_graph(),
                   highlight_nodes = input$node_highlight)
  })

  #### Plot it ####

  output$my_network <- renderPlotly({

    make_network_plot(my_graph(),
                      my_layout(),
                      first_date(),
                      node_label_var = node_labels,
                      node_cols = node_cols(),
                      edge_cols = edge_cols(),
                      weighted_edges = TRUE,
                      edge_transparency = input$edge_transparency)

  })


  #### Update input options ####

  # observe({
  #
  #   getEvent(input$group_highlight, cur_group)
  #   getEvent(input$node_highlight, cur_highlight)
  #
  #   if (input$type_group == 1 ) {
  #
  #
  #     updateSelectInput(session,
  #                       "group_highlight",
  #                       choices = "None")
  #   } else {
  #
  #     updatePickerInput(session,
  #                       "type_highlight",
  #                       choices = "None")
  #
  #     updateSelectInput(session,
  #                       "group_highlight",
  #                       choices = group,
  #                       selected = cur_group())
  #   }
  #
  #   updatePickerInput(session,
  #                     "node_highlight",
  #                     choices = newInfo$IA.Name,
  #                     selected = cur_highlight())
  #
  #   updateSelectInput(session,
  #                     "node_remove",
  #                     choices = c("None", newInfo$IA.Name))
  # })

}

# Run the application
shinyApp(ui = ui, server = server)



