library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)
library(timevis)
library(lubridate)
library(stringr)

shooting_data <- read.csv("school-shootings-data-currentsit.csv")


df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(Year) %>%
  mutate(Total_Shootings = n())

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  ),
  
  #plot1_overtheyears
  mainPanel(
    width = 12,
    fluidRow(
      column(
        8, align = "center",
        plotlyOutput("shooting_plot", width = "120%")
      )
    ))
)
  
# Define server logic required to draw 
server <- function(input, output) {
  
  #School Shootings Over the Years line plot
  output$shooting_plot <- renderPlotly({
    p <- ggplot(data = shooting_data, aes(x = Year, y = Total_Shootings)) +
      geom_line(color = "black") +
      geom_point(color = "black", size = 1) +
      labs(x = "Year",
           y = "Number of Shootings",
           title = "School Shootings Over the Years") +
      theme_minimal()
    ggplotly(p) %>%
      config(displayModeBar = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)