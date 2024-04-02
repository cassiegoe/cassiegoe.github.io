library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)

shooting_data <- read.csv("school-shootings-data.csv")
df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(year) %>%
  mutate(Incidents = n())

# Define UI for application 
ui <- fluidPage(
  titlePanel("How Can We Prevent Gun Violence in Schools?"),
  
  #self-intro
  h4(strong("Self-Intro")),
  p(style="text-align: justify; font-size = 25px",
    "Insert Self-Intro"),
  br(),
  
  #Data set intro
  h4(strong("Data Set Intro")),
  p(style="text-align: justify; font-size = 25px",
    "Insert Data Set Intro"),
  br(),
  
  #current situation
  h4(strong("Current Situation")),
  p(style="text-align: justify; font-size = 25px",
    "Insert current situation"),
  br(),
  
  #plot1_overtheyears
  mainPanel(
    width = 12,
    fluidRow( column(8, align="center",
                     plotlyOutput("shooting_plot")
    )),
    
    br(),
    p(style="text-align: justify; font-size = 25px",
      "Insert elaboration of graph and prevalence"),
    br(),
    h4(strong("Information on incidents")),
  
  #cards1_incidents    
  layout_sidebar(
      sidebar = sidebar(
        selectInput("year_select_sidebar", "Select Year:", choices = unique(shooting_data$year))
      ),
      "Main contents"
    ),
  
  br(), 
  p(style="text-align: justify; font-size = 25px",
    "Insert link to factors and solution"),
  ),
  
  )



# Define server logic required to draw 
server <- function(input, output) {
  
  #School Shootings Over the Years
  output$shooting_plot <- renderPlotly({
    p <- ggplot(data = shooting_data, aes(x = year, y = Incidents)) +
      geom_line(color = "black") +
      geom_point(color = "black", size = 1) +
      labs(x = "Year",
           y = "Number of Incidents",
           title = "School Shootings Over the Years") +
      theme_minimal()
    ggplotly(p)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)