library(shiny)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)
library(tidyr)
library(shinyWidgets)
library(shinythemes)
library(rlang)

# Some prep

dat <- read_csv("/Users/kelly/Dropbox/longnet/data/Full_Data.csv")
IA_info <- read_csv("/Users/kelly/Dropbox/longnet/data/IA_Meta.csv")
Mem_info <- read_csv("/Users/kelly/Dropbox/longnet/data/Member_Meta.csv")

centralities <- read.csv("/Users/kelly/Dropbox/longnet/data/IA_Centralities.csv", stringsAsFactors = FALSE) # %>%
  # mutate(
  #   Degree = Degree - Self_Degree
  # )

node_choices <- unique(centralities$IA.Name)


shinyApp(
  ui =
    navbarPage("Centrality of Institutions",
      #shinythemes::shinytheme("sandstone"),
      tabPanel("Trends over time",
               sidebarPanel(
                 radioButtons('metric2',
                              'Which measure do you want to see?',
                              choices = c(
                                "Betweenness" = "Betweenness",
                                "Degree" = "Degree",
                                "Number of members" = "Self_Degree"
                              )
                 ),
                 pickerInput('institutions',
                             'Choose Institution(s) to Plot',
                             choices = node_choices,
                             options = list(`actions-box` = TRUE),
                             multiple = T
                 )
               ),
               mainPanel(
                 plotOutput('time_plot')
               )
      ),
      tabPanel("Examine a single date",
               sidebarPanel(
                 # Highlight a node
                 selectInput('month',
                             'Month',
                             choices = 1:12
                 ),
                 selectInput('day',
                             'Day',
                             choices = 1:31
                 ),
                 sliderInput('year',
                             'Year',
                             value = 1979,
                             min = 1945, max = 1989,
                             sep = ""),
                 radioButtons('metric1',
                              'Which measure do you want to see?',
                              choices = c(
                                "Betweenness" = "Betweenness",
                                "Degree" = "Degree",
                                "Number of members" = "Self_Degree"
                              )
                 ),
                 radioButtons('groups',
                              'What category should we group by?',
                              choices = c(
                                "Meta-Groups" = "IA.Group.1",
                                "Type" = "Type",
                                "City" = "City",
                                "Voivodship" = "Voivodship"
                              )
                 )
               ),
               mainPanel(
                 plotOutput('single_day_plot')
               )
      )
  ),
  server = function(input, output) {

    my_date <- reactive({
      paste(input$year, input$month, input$day, sep = "-")
    })

    output$single_day_plot <- renderPlot({

      # Consistent colors
      centralities %>%
        filter(Date == ymd(my_date())) %>%
        group_by(!!sym(input$groups)) %>%
        summarize(`Centrality Measure` = sum(!!sym(input$metric1))) %>%
        ggplot(aes(x = !!sym(input$groups), y = `Centrality Measure`, fill = !!sym(input$groups))) +
        geom_col() + theme(legend.position = "none")

    })


      output$time_plot <- renderPlot({

        # prettier x-axis
        centralities %>%
          filter(IA.Name %in% input$institutions) %>%
          ggplot(aes(x = Date, y = !!sym(input$metric2), color = IA.Name)) +
          geom_line() +
          (scale_x_date(limits=start.end,
                        breaks=date_breaks("1 year"),
                        labels=date_format("%b %y")))

      })
  }
)

