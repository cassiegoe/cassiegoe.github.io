library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)
library(timevis)
library(lubridate)
library(stringr)

shooting_data <- read.csv("school-shootings-data-time.csv")

hv <- as.numeric(str_split(shooting_data$time, ":", simplify = T)[,1])
am_info <- str_split(shooting_data$time, " ", simplify = T)[, 2]
final_hv <- ifelse(am_info == "AM", hv, hv + 12)
final_hv = ifelse(am_info == "PM" & final_hv == 24, 12, final_hv)
final_hv = ifelse(am_info == "AM" & final_hv == 12, 0, final_hv)
shooting_data$hour = final_hv


df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(year) %>%
  mutate(Incidents = n())

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")),
  
  plotlyOutput("time_plot")  
)

# Define server logic required to draw 
server <- function(input, output) {
  
  #heatmap of time
  output$time_plot <- renderPlotly({
    shooting_data %>%
      filter(hour <= 20) %>%
      group_by(year, hour, day) %>%
      count() %>%
      ggplot(aes(x = year, y = hour, size = n, text = paste("Year: ",year, "<br>",
                                                            "Hour: ", hour, "<br>",
                                                            "Total Shootings: ", n))) +
      geom_point(alpha = 0.6 ,color= "#9d735d") +
      facet_wrap(~ day) +
      labs(x = "Year", y = "Hour of Day", fill = "Number of Shootings") +
      theme_minimal()
    
    ggplotly(tooltip = "text") %>%
      config(displayModeBar = FALSE)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)