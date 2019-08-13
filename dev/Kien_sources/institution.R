library(shiny)
library(dplyr)
library(RCurl)
library(DT)
library(lubridate)
library(shinyWidgets)
library(plotly)

#options(shiny.error = browser)

Sys.setlocale("LC_ALL", "Polish")

# Get data from github private repo
ia <- read.csv("https://www.dropbox.com/s/ux2cmu1qkcjiq75/Full_Data.csv?dl=1", header = T)
name <- sort(ia$Full.Name)
type <- sort(unique(na.omit(as.character(ia$Type))))

ia$Start.Date <- as.Date(ia$Start.Date, format = "%Y-%m-%d")
ia$End.Date <- as.Date(ia$End.Date, format = "%Y-%m-%d")

ui <- fluidPage(
  # App title
  titlePanel("Shared institutions of selected members"),
  
  sidebarLayout(
    sidebarPanel(

      # Select date range
      dateRangeInput("dateRange",
                     label = 'Date range input: yyyy-mm-dd',
                     start = min(ia$Start.Date), end = ymd(min(ia$Start.Date)) + years(10),
                     min = min(ia$Start.Date), max = max(ia$End.Date),
                     startview = 'year'
      ),
      
      # Select variable for first member
      selectInput(inputId = "member1", 
                  label = "Member 1",
                  choices = name),
      
      # Select variable for second member
      selectInput(inputId = "member2", 
                  label = "Member 2",
                  choices = name),
      
      # Select type for institutions
      pickerInput(inputId = "type",
                  label = "Type of Institution",
                  choices = type,
                  selected = type,
                  options = list(`actions-box` = TRUE), multiple = T)
      
    ),
    
    mainPanel(
      dataTableOutput(outputId = "iatable")
      #verbatimTextOutput("dateRangeText")
    )
  )
)

server <- function(input, output, session) {
  # output$dateRangeText  <- renderText({
  #   paste("input$dateRange is", (input$dateRange[1]))
  # })
  
  
  # Make sure date is input (requirement is met)
  inputDate <- reactive({
    req(input$dateRange)
    input$dateRange
  })
  
  # Reactive variable to be used many times
  newName <- reactive({
    req(input$dateRange)
    ia %>%
      filter((between(Start.Date, inputDate()[1], inputDate()[2]) | between(End.Date, inputDate()[1], inputDate()[2]))) %>%
      select(Full.Name, Type, IA.ID)
  })
  
  # Change values for input$member1
  observe({
    updateSelectInput(session, "member1", choices = sort(newName()$Full.Name))
  })
  
  member1 <- reactive(input$member1)
  
  # Change values for input$member2
  observe({
    # Get the IA.ID of member1
    iaID <- newName()[newName()$Full.Name == member1(), "IA.ID"]
    
    # Initialize a vector to store names of member who shares the same IA.ID with member 1
    newName2 = c()
    
    for (i in 1:length(iaID)){
      # Use as.character function since this is a factor, if not used, newName2 will store the
      # number in factor instead of string
      name <- as.character(newName()[newName()$IA.ID == iaID[i], "Full.Name"])
      newName2 <- c(newName2, name)
    }
    
    newName2 <- unique(newName2)
    updateSelectInput(session, "member2", choices = sort(newName2), selected = member1())
  })
  
  member2 <- reactive(input$member2)
  
  # Change values for input$type
  observe({
    newType <- newName() %>%
      filter((Full.Name %in% input$member1)) %>%
      select(Type)
    
    updatePickerInput(session, "type", choices = unique(as.character(newType$Type)),
                      selected = newType$Type)
  })
  
  output$iatable <- renderDataTable({
    req(input$dateRange)

    # Since PickerInput treat NA as "" after picking the type, this code is used to make sure input$type include
    # "real" NA when NA is picked in PickerInput. For example, Jan Zalewski 1949-1-1 has NA Type in institution.
    if ("" %in% input$type)
      ntype <- c(input$type, NA)
    else
      ntype <- input$type

    # This single line of code is supposed to behave the same as above but not sure why it crashes the app.
    # ntype <- ifelse("" %in% input$type, c(input$type, NA), input$type)
    
    dat <- ia %>%
      filter((between(Start.Date, inputDate()[1], inputDate()[2]) | between(End.Date, inputDate()[1], inputDate()[2])),
             (Full.Name %in% c(member1(), member2())),
             Type %in% ntype) %>%
      select(IA.Name, Type, Full.Name, IA.Group.1, IA.Group.2)

    inst1 <- (dat %>%
      filter(Full.Name == member1()) %>%
      select(IA.Name))
    #print(paste("List 1: ", as.character(inst1$IA.Name)))
    
    inst2 <- (dat %>%
      filter(Full.Name == member2()) %>%
      select(IA.Name))
    #print(paste("List2: ", as.character(inst2$IA.Name)))

    if (length(intersect(inst1$IA.Name, inst2$IA.Name)) != 0){
      dat <- dat %>%
        filter(IA.Name %in% intersect(inst1$IA.Name, inst2$IA.Name)) %>%
        select(IA.Name, IA.Group.1, IA.Group.2, Type) %>%
        unique()
    } else{
      # Pseudo name to make the data table empty
      #   For example, Jan Rosner and Zofia only share the same Warsaw institute. When the user unticks the Type "Postsecondary",
      # the data shoud return empty instead of other Jan Rosner's institutions.
      dat <- dat %>% filter(IA.Name == "")
    }

    datatable(data = dat)
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
