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
  
  #section for school type
  h4(strong("School Type")),
  fluidRow(
      column(6, 
           plotOutput("schooltype_plot")),
      column(6, 
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
  
  #section for school security
  br(),
  h4(strong("School Security -> remove? ")),
  p(style="text-align: justify; font-size = 20px",
    "Insert elab"),
  p(style="text-align: justify; font-size = 20px",
    "Insert plot"),
  plotOutput("schoolsecurity_plot"),
  
  #Section for access to firearms
  h4(strong("Acess to Firearms")),
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
  h4(strong("Characteristics? - age, race etc")),
  
  fluidRow(
    column(6, 
           p(style="text-align: justify; font-size = 20px",
             "Insert plot")),
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
  
  #School type bar plot
  output$schooltype_plot <- renderPlot({
    ggplot(data = shooting_data, aes(x=school_type)) +
      geom_bar(fill = "black", color = "black") +
      labs(title = "Number of School Shootings by School Type",
           x = "School Type",
           y = "Number of Incidents") +
      theme_minimal()
  })
  
  #school security bar plot
  output$schoolsecurity_plot <- renderPlot({
    ggplot(data = shooting_data, aes(x=resource_officer)) +
      geom_bar(fill = "black", color = "black") +
      labs(title = "School Security",
           x = "Presence of Security Officer",
           y = "Number of Incidents") +
      theme_minimal()
  })
  
  #Access to firearms slideshow
  output$slickr <- renderSlickR({
    imgs <- list.files("www", pattern=".jpg", full.names = TRUE)
    slickR(imgs)
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)
