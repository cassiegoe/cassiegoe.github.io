library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-schoolenv.csv")
shooting_data_frame <- data.frame(shooting_data)

incident_summary <- shooting_data %>%
  group_by(school_type, resource_officer) %>%
  summarize(total_incidents = n())


# Define UI for application 
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"),
    tags$script(src = "https://kit.fontawesome.com/<you>.js")
  ),
  
  #school environment: section for school type & security
  fluidRow(
    column(8, 
           plotlyOutput("officerxtype_plot")),
    column(4, 
           p(style="text-align: justify; font-size = 20px",
             "Insert elab"))
  ),
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
  
  my_colors <- c("#af4444","#769776")
  
  # officer x type plot
  output$officerxtype_plot <- renderPlotly({
    y <- ggplot(data = incident_summary, aes(x = school_type, y = total_incidents, fill = resource_officer)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      labs(title = "School Type and Resource Officer Presence",
           x = "School Type",
           y = "Number of Incidents",
           fill = "Presence of Resource Officer") +
      scale_fill_manual(values = my_colors)
      theme_minimal()
    ggplotly(y)
  })

  }

# Run the application 
shinyApp(ui = ui, server = server)
