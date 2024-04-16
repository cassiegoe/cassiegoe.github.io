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
    column(7, 
           plotlyOutput("officerxtype_plot")),
    column(5, 
           card(
             style = " font-size: 13px; background-color: #EAD196; font-family: Arial, sans-serif;", 
             card_header(
               style = "background-color: #BF3131;",
               strong("What can be done?", style="text-align: justify; font-size: 15px;"),
             ),
             markdown(
               "- **Mentorship programs and extracurricular activities:** Foster stronger connections 
               between students, teachers, and schools to promote a sense of belonging in students.
               - **Increased Resource Officer Deployment:** Deploying resource officers particularly 
               at identified high-risk areas, can help deter potential perpetrators and swiftly 
               respond to security threats.
               - **Allocate Funding to School Security:** Prioritizing funding towards 
               security enhancements, such as surveillance systems and emergency response protocols, 
               can bolster school security infrastructure and efforts to mitigate the risk of violence.")
           )))

)


# Define server logic required to draw 
server <- function(input, output) {
  
  my_colors <- c("#af4444","#769776")
  
  # officer x type plot
  output$officerxtype_plot <- renderPlotly({
    y <- ggplot(data = incident_summary, aes(x = school_type, y = total_incidents, fill = resource_officer, 
                                             text = paste("School Type: ", school_type, "<br>",
                                                          "Resource Officer: ", resource_officer, "<br>",
                                                          "Total Shootings: ", total_incidents))) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      labs(title = "School Type and Resource Officer Presence",
           x = "School Type",
           y = "Number of Shootings",
           fill = "Presence of Resource Officer") +
      scale_fill_manual(values = my_colors) +
      theme_minimal()
      
    ggplotly(y, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })

  }

# Run the application 
shinyApp(ui = ui, server = server)
