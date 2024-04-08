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
  plotlyOutput("weaponsource_plot"),
  br(),
  br(),
  
  fluidRow(
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert elab")),
    column(6,
           card(
             style = "background-color: white; font-family: Arial, sans-serif;", 
             card_header(
               class = "bg-dark text-white",
               "View firearms used in deadliest school shootings:"
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
  
  #Weapon Source plot
  output$weaponsource_plot <- renderPlotly({
    b <- ggplot(data = shooting_data[!is.na(shooting_data$Weapon_source_edited), ], aes(x = Weapon_source_edited)) +
      geom_bar(fill = "purple", color = "black") +
      labs(title ="Sources of Shooters' Weapons",
           x = "Weapon Sources",
           y = " Number of Incidents") +
      theme_minimal() + coord_flip()
    ggplotly(b)
  })

  
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
