library(shiny)
library(timevis)

shooting_data <- read.csv("school-shootings-data-timeline.csv")
df <- data.frame(shooting_data)


ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Choose a year:",
                  choices = unique(shooting_data$year))),
      
    mainPanel(
      tableOutput("shooting_table"))
  )
)
  

server <- function(input, output) {
  
  datasetInput <- reactive({
    filtered_data <- shooting_data[shooting_data$year == input$year, c("id", "School", "State", "Date", "Killed", "Injured")]
    filtered_data
  })
  
  output$shooting_table <- renderTable({
    datasetInput()
  })
  
}

shinyApp(ui = ui, server = server)
