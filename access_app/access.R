library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-access.csv")
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
  
  #Section for access to firearms
  br(),
  h4(strong("Access to Firearms")),
  p(style="text-align: justify; font-size = 20px",
    "Insert plot"),
  
  fluidRow(
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert elab")),
    column(6,
           card(
             style = "background-color: white; font-family: Arial, sans-serif;", 
             card_header(
               class = "bg-dark text-white",
               "Swipe to view the most common types of weapons used"
             ),
             slickROutput("slickr", width="300px"))
    )),
  
  br(),
  card(
    style = "background-color: white; font-family: Arial, sans-serif;", 
    card_header(
      class = "bg-dark text-white",
      "What can be done?"
    ),
    markdown(
      "Insert Solution")
  ),
  
  
)


# Define server logic required to draw 
server <- function(input, output) {
  
  #Access to firearms slideshow
  output$slickr <- renderSlickR({
    imgs <- list.files("www", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
    
  })
  

  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
