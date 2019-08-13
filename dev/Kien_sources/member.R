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
ia <- read.csv("https://www.dropbox.com/s/ux2cmu1qkcjiq75/Full_Data.csv?dl=1", header = TRUE)
ia_name <- sort(ia$IA.Name)
profession <- sort(unique(as.character(ia$Profession)))


ia$Start.Date <- as.Date(ia$Start.Date, format = "%Y-%m-%d")
ia$End.Date <- as.Date(ia$End.Date, format = "%Y-%m-%d")

ui <- fluidPage(
  # App title
  titlePanel("Shared members in selected institutions"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Select a date
      # dateInput(inputId = "date",
      #           label = "Date input: yyyy-mm-dd",
      #           min = min(ia$Start.Date),
      #           max = max(ia$End.Date),
      #           value = min(ia$Start.Date),
      #           startview = "year"),
      
      # Select date range
      dateRangeInput("dateRange",
                     label = 'Date range input: yyyy-mm-dd',
                     start = min(ia$Start.Date), end = ymd(min(ia$Start.Date)) + years(10),
                     min = min(ia$Start.Date), max = max(ia$End.Date),
                     startview = 'year'
      ),
      
      # Select variable for first member
      selectInput(inputId = "ia1", 
                  label = "Institution 1",
                  choices = ia_name),
      
      # Select variable for second member
      selectInput(inputId = "ia2", 
                  label = "Institution 2",
                  choices = ia_name),
      
      # Select type for institutions
      pickerInput(inputId = "profession",
                  label = "Profession of Member",
                  choices = profession,
                  selected = profession,
                  options = list(`actions-box` = TRUE), multiple = T)
      
    ),
    
    mainPanel(
      dataTableOutput(outputId = "membertable")
    )
  )
)

server <- function(input, output, session) {
  # Make sure date is input (requirement is met)
  inputDate <- reactive({
    req(input$dateRange)
    input$dateRange
  })
  
  # Reactive variable to be used many times
  newIAName <- reactive({
    req(input$dateRange)
    ia %>%
      filter((between(Start.Date, inputDate()[1], inputDate()[2]) | between(End.Date, inputDate()[1], inputDate()[2]))) %>%
      select(IA.Name, Profession, Member.ID)
    })
  
  # Change values for input$ia1
  observe({
    updateSelectInput(session, "ia1", choices = sort(newIAName()$IA.Name))
  })
  
  ia1 <- reactive(input$ia1)
  
  # Change values for input$ia2
  observe({
    #ianameList2 <- newIAName()$IA.Name[newIAName()$IA.Name != ia1()]

    memberID <- newIAName()[newIAName()$IA.Name == ia1(), "Member.ID"]
    newName2 = c()

    for (i in 1:length(memberID)){
      name <- as.character(newIAName()[newIAName()$Member.ID == memberID[i], "IA.Name"])
      newName2 <- c(newName2, name)
    }

    newName2 <- unique(newName2)
    updateSelectInput(session, "ia2", choices = sort(newName2), selected = ia1())
  })
  
  ia2 <- reactive(input$ia2)
  
  # Change values for input$profession
  observe({
    newProf <- newIAName() %>%
      filter((IA.Name %in% input$ia1)) %>%
      select(Profession)

    updatePickerInput(session, "profession", choices = unique(as.character(newProf$Profession)),
                      selected = newProf$Profession)
  })
  
  output$membertable <- renderDataTable({
    req(input$dateRange)
    # print(ia1())
    # print(ia2())
    
    dat <- ia %>% 
      filter((between(Start.Date, inputDate()[1], inputDate()[2]) | between(End.Date, inputDate()[1], inputDate()[2])), 
             (IA.Name %in% c(ia1(), ia2())),
             Profession %in% input$profession) %>%
      select(IA.Name, Full.Name, Member.ID, Age, Gender, Profession)
    
    #print(dat)
    mem1 <- dat %>%
          filter(IA.Name == ia1()) %>%
          select(Full.Name)
    #print(as.character(mem1$Full.Name))

    mem2 <- dat %>%
          filter(IA.Name == ia2()) %>%
          select(Full.Name)
    #print(as.character(mem2$Full.Name))

    if (length(intersect(mem1$Full.Name, mem2$Full.Name)) != 0){
      dat <- dat %>%
        filter(Full.Name %in% intersect(mem1$Full.Name, mem2$Full.Name)) %>%
        select(Full.Name, Member.ID, Age, Gender, Profession) %>%
        unique()
    }
    else {
      dat <- dat %>% filter(Full.Name == "")
    }
    
    datatable(data = dat)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
