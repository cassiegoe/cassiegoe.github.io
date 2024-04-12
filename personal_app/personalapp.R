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
          br(),
          p("The most common type of shootings that occurred were targeted shootings 
            where a specific individual or group is singled out as the primary target. 
            These shootings are often premeditated and deliberate, with the perpetrator 
            purposefully firing their weapon with intended victims in mind. Unlike 
            indiscriminate shootings, which may involve random victims, targeted shootings 
            typically involve specific motives or grievances against the victims.", style="text-align: justify; font-size: 15px;")
        ),
        conditionalPanel(
          condition = "input.plot == 'Shooter\\'s Relationship to School'",
          plotlyOutput("shooterrelation_plot", width = "100%"),
          br(),
          p("A significant proportion of perpetrators were either current students
            or former students at the school where the shooting occurred.  ", style="text-align: justify; font-size: 15px;")
          
        )
      )
   ),
  
  
  
  #section for personal motivations  
  br(),
  fluidRow(
    column(
      width = 6,
      strong("Connecting the dots...", style="text-align: justify; font-size: 15px;"),
      p("The majority of perpetrators involved in targeted shootings are typically 
        current or former members of a school community, where their primary motive often 
        stems from seeking revenge due to real or perceived mistreatment by a particular individual 
        or group [6]. This mistreatment encompasses any form of harm, injustice, or abuse experienced 
        by the perpetrator at the hands of specific individuals or groups within the school. For instance, both perpetrators 
        and victims of bullying show a higher correlation with violent behavior and weapon carrying [7]. 
        The primary motives of shooters could reflect deeper systemic issues that can encompass various 
        aspects, including racial discrimination. ", style="text-align: justify; font-size: 15px;")
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
    a <- ggplot(data = shooting_data, aes(x=shooting_type_edited,
                                          text = paste("Shooting Type: ",shooting_type_edited, "<br>",
                                                       "Total Shootings: ", Incidents))) +
      geom_bar(fill = "#9d735d") +
      labs(title = "Shooting Type",
           x = "Shooting Type",
           y = "Number of Shootings") +
      theme_minimal() + coord_flip()
    ggplotly(a, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  #Shooter Relationship
  output$shooterrelation_plot <- renderPlotly({
    b <- ggplot(data = shooting_data[!is.na(shooting_data$shooter_relationship_edited), ], aes(x = shooter_relationship_edited,
                                                                                               text = paste("Shooter Relation: ",shooter_relationship_edited, "<br>",
                                                                                                      "Total Shootings: ", Incidents))) +
      geom_bar(fill = "#c3cba8") +
      labs(title ="Shooter Relation to School",
           x = "Shooter Relation",
           y = " Number of Shootings") +
      theme_minimal() + coord_flip()
    
    ggplotly(b, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
