library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data.csv")
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

  titlePanel("How Can We Prevent Gun Violence in Schools"),
  
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
        selectInput("year_select_sidebar", "Select Year:", choices = unique(shooting_data$year)),
        card()
      )),
    
    br(), 
    p(style="text-align: justify; font-size = 25px",
      "Insert link to factors and solution")),
  
  #school environment: section for school type & security
  h4(strong("School Environment")),
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
             "Swipe to see"
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
  
  #section for personal motivations  
  br(),
  h4(strong("Personal Motivations (2 variables)")),
  
  fluidRow(
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert plot")),
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert elab"))
  ),
  
  plotlyOutput("shootingtype_plot"),
  
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
  
  #School Shootings Over the Years line plot
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

  # officer x type plot
  output$officerxtype_plot <- renderPlotly({
    y <- ggplot(data = incident_summary, aes(x = school_type, y = total_incidents, fill = resource_officer)) +
      geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
      labs(title = "School Type and Resource Officer Presence",
           x = "School Type",
           y = "Number of Incidents",
           fill = "Presence of Resource Officer") +
      theme_minimal()
    ggplotly(y)
  })
  
  #Access to firearms slideshow
  output$slickr <- renderSlickR({
    imgs <- list.files("www", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  
})
  
  #Shooting Type
  output$shootingtype_plot <- renderPlotly({
    a <- ggplot(data = shooting_data, aes(x=shooting_type)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = "Shooting Type",
           x = "Shooting Type",
           y = "Number of Incidents") +
      theme_minimal() + coord_flip()
    ggplotly(a)
  })

  
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
