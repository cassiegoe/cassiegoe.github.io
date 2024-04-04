library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-age.csv")
df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(year) %>%
  mutate(Incidents = n())

incident_summary <- shooting_data %>%
  group_by(school_type, resource_officer) %>%
  summarize(total_incidents = n())

# Define UI for application 
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  ),
  
  #section for indiv characteristics
  br(),
  h4(strong("Age")),
  
  fluidRow(
    column(6, 
           plotlyOutput("age_plot")),
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert elab"))
  )
  
  
)


# Define server logic required to draw 
server <- function(input, output) {
  
  #Age plot
  output$age_plot <- renderPlotly({
    x <- ggplot(data = shooting_data, aes(x=age_shooter1)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = "Age of Shooter",
           x = "Age",
           y = "Number of Incidents") +
      theme_minimal()
    ggplotly(x)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
