library(shiny)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)
library(tidyr)
library(shinyWidgets)
library(devtools)
devtools::install_github("kbodwin/longnet")
#library(longnet)
#library(graphlayouts)

# Link to shiny app:

# For whatever reason, this is what allows Polish characters to display
Sys.setlocale("LC_ALL", "Polish")


# # Function to get current selection from input
# getEvent <- function(input, current_selection){
#   #print(input)
#   if (is.null(input))
#     input <- "None"
#
#   observeEvent(input, {
#     current_selection(input)
#   })
# }



# Some prep

dat <- read_csv("https://www.dropbox.com/s/nvh1mi91djp53me/Full_Data.csv?dl=1")
IA_info <- read_csv("https://www.dropbox.com/s/gl461tqg6li1awb/IA_Meta.csv?dl=1")
Mem_info <- read_csv("https://www.dropbox.com/s/dyjby6p55mrikrv/Member_Meta.csv?dl=1")

# dat <- read_csv("/Users/kelly/Dropbox/longnet/data/Full_Data.csv")
# IA_info <- read_csv("/Users/kelly/Dropbox/longnet/data/IA_Meta.csv")
# Mem_info <- read_csv("/Users/kelly/Dropbox/longnet/data/Member_Meta.csv")

node_choices <- c(IA_info$IA.ID)
names(node_choices) <- c(IA_info$IA.Name)

group_choices <- c(unique(as.character(IA_info$Type)))
group_choices <- group_choices[!is.na(group_choices)]
grouping_var <- "Type"

network_type <- "kk"

node_var <- "IA.ID"
node_labels <- "IA.Name"
edge_var <- "Member.ID"
edge_labels <- "Full.Name"



# Define UI for application
ui <- pageWithSidebar(

  headerPanel("Connections between Polish institutions"),

  sidebarPanel(
    h3("Choose a Date"),
    div(style="display: inline-block;vertical-align:top; width: 150px;",
        selectInput('month1',
                'Month',
                choices = 1:12,
                selected = 1
    )),
    div(style="display: inline-block;vertical-align:top; width: 150px;",
        selectInput('day1',
                'Day',
                choices = 1:31,
                selected = 1
    )),
    sliderInput('year1',
                'Year',
                value = 1979,
                min = 1945, max = 1989,
                sep = ""),
    # h3("End of Date Range"),
    # selectInput('month2',
    #             'Month',
    #             choices = 1:12
    # ),
    # selectInput('day2',
    #             'Day',
    #             choices = 1:31
    # ),
    # sliderInput('year2',
    #             'Year',
    #             value = 1979,
    #             min = 1945, max = 1989,
    #             sep = ""),

    h3("Color Highlighting"),

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

    h3("Aesthetics"),

    # Selecting edge transparency
    sliderInput('edge_transparency',
                'Edge Transparency',
                value = 1,
                min = 0, max = 1),

    sliderInput('node_size',
                "Node Size",
                value = 10,
                min = 0, max = 30),

    h3("Omit Nodes or Edges"),

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

  output$text <- renderText(toString(input$group_highlight))

  # Create reactiveVal to retain previously selected values
  cur_type <- reactiveVal(NULL)
  cur_group <- reactiveVal(NULL)
  cur_highlight <- reactiveVal(NULL)

  # Create reactive to retain previous graph for faster plotting
  #prev_layout <- reactiveValues(x = 0, y = 0, name = "")

  #### Get Selected Dates ####
  first_date <- reactive({
    get_date(input$year1, input$month1, input$day1)
  })

  last_date <- reactive({
    get_date(input$year1, input$month1, input$day1)
  })


  #### Make Graph ####
  my_graph <- reactive({
    make_graph_df(dat,
               "IA.ID", "Member.ID",
               "Start.Date", "End.Date",
               first_date(), last_date(),
               date_orders = "ymd",
               edge_labels = edge_labels,
               node_remove = input$node_remove,
               edge_remove = input$edge_remove,
               get_edge_names = TRUE)
  })

  #### Calculate layout ####
  prev_layout <- NULL

  my_layout <- reactive({
    get_layout_df(my_graph(),
                  node_meta = IA_info,
                  node_var = node_var,
                  prev_layout = prev_layout,
                  algorithm = network_type) %>%
      left_join(IA_info, keep = TRUE)
  })

  observeEvent(my_layout(),
               {
                 prev_layout <- isolate(my_layout())
               })

  #### Set node and edge details ####

  node_cols <- reactive({
    make_node_cols(my_layout(),
                   node_var = node_var,
                   group_var = "Type",
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
                      node_var = node_var,
                      node_label_var = node_labels,
                      node_cols = node_cols(),
                      edge_cols = edge_cols(),
                      weighted_edges = TRUE,
                      edge_transparency = input$edge_transparency,
                      node_size = input$node_size)

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



