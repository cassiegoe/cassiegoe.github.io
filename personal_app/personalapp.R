library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-personal.csv")
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
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"),
    tags$script(src = "https://kit.fontawesome.com/<you>.js")
  ),
  
  #Sidebar Layout
   sidebarLayout(
    sidebarPanel(
      radioButtons("plot", h3("Choose Factor:"), 
                   c("Type of Shooting", "Shooter's Relationship to School"))),

      mainPanel(
        conditionalPanel(
          condition = "input.plot == 'Type of Shooting'",
          plotlyOutput("shootingtype_plot", width = "100%"),
          p("shooting type text")
        ),
        conditionalPanel(
          condition = "input.plot == 'Shooter\\'s Relationship to School'",
          plotlyOutput("shooterrelation_plot", width = "100%"),
          p("shooter relationship text")
          
        )
      )
   ),
  
  
  
  #section for personal motivations  
  br(),
  fluidRow(
    column(
      width = 6,
      strong("Connecting the dots"),
      p("snjeifiknnvsnjeifiknnvsnjeifiknnvsnjeifiknnv")
    ),
    column(
      width = 6,
      card(
        style = "background-color: white; font-family: Arial, sans-serif;", 
        card_header(
          class = "bg-dark text-white",
          "What can be done?"
        ),
        markdown(
          "Insert Solution")
      )
     )
   )
)





# Define server logic required to draw 
server <- function(input, output) {
  
  
  #Shooting Type
  output$shootingtype_plot <- renderPlotly({
    a <- ggplot(data = shooting_data, aes(x=shooting_type_edited)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = "Shooting Type",
           x = "Shooting Type",
           y = "Number of Incidents") +
      theme_minimal() + coord_flip()
    ggplotly(a)
  })
  
  #Shooter Relationship
  output$shooterrelation_plot <- renderPlotly({
    b <- ggplot(data = shooting_data[!is.na(shooting_data$shooter_relationship_edited), ], aes(x = shooter_relationship_edited)) +
      geom_bar(fill = "orange", color = "black") +
      labs(title ="Shooter Relationship with School",
           x = "Shooter Relationship",
           y = " Number of Incidents") +
      theme_minimal() + coord_flip()
    ggplotly(b)
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
