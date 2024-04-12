library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-age.csv")
df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(age_shooter1) %>%
  mutate(Incidents = n()) 

# Define UI for application 
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  ),
  
  #section for indiv characteristics
  br(),
  
  fluidRow(
    column(7, 
           plotlyOutput("age_plot")),
    column(5, 
           p("The median age of school shooters between the year 1999 and 2023 were 
             15-year-olds, with 48 counts of school shooting incidents. The second highest 
             number of school shootings were perpetrated by 16-year-olds with 39 counts. 
             Despite the legal age requirement for firearm purchases, which varies between 18 
             and 21 depending on state policies, over half of these shootings were carried out 
             by children and adolescents under the age of 18. So where are these young 
             individuals obtaining their weapons from?", style="text-align: justify; font-size: 15px;")))
  
  
)


# Define server logic required to draw 
server <- function(input, output) {
  
  #Age plot
  output$age_plot <- renderPlotly({
    x <- ggplot(data = shooting_data, aes(x=age_shooter1,
                                          text = paste("Age of Soooter: ",age_shooter1, "<br>",
                                                       "Total Shootings: ", Incidents))) +
      geom_bar(fill = "#af4444",color = "#dee2d0", width = 1) +
      labs(title = "Age of Shooter",
           x = "Age",
           y = "Number of Shootings") +
      theme_minimal()
    ggplotly(x, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
