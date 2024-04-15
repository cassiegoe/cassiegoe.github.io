library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-personal.csv")
df <- data.frame(shooting_data)

shooting_data1 <- shooting_data %>%
  group_by(shooting_type_edited) %>%
  mutate(Incidents1 = n())

shooting_data2 <- shooting_data %>%
  group_by(shooter_relationship_edited) %>%
  mutate(Incidents2 = n())

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
                   c("Type of Shooting", "Shooter's Relation to School"))),

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
          condition = "input.plot == 'Shooter\\'s Relation to School'",
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
        or group", span("[4]", style = "color: #2780e3;"),". This mistreatment encompasses any form of harm, injustice, or abuse experienced 
        by the perpetrator at the hands of specific individuals or groups within the school. For instance, both perpetrators 
        and victims of bullying show a higher correlation with violent behavior and weapon carrying ", span(" [5]", style = "color: #2780e3;"),". 
        The primary motives of shooters could reflect deeper systemic issues that can encompass various 
        aspects, including racial discrimination. ", style="text-align: justify; font-size: 15px;")
    ),
    column(
      width = 6,
      card(
        style = " font-size: 13px; background-color: #F0EBE3; font-family: Arial, sans-serif;", 
        card_header(
         style = "background-color: #D37676;",
         strong("What can be done?", style="text-align: justify; font-size: 15px;"),
        ),
        markdown(
          "- **Early Identification and Intervention:** Implement proactive measures to identify 
          students who may be at risk of engaging in targeted violence. This can include 
          enhanced threat assessment protocols, regular mental health screenings, and 
          training for school staff to recognize warning signs of distress or potential violent behavior.
          - **Improved Student Support Services:** Increase access to mental health 
          resources and support services within schools to address the underlying 
          issues contributing to students' feelings of isolation, anger, or resentment. 
          This may involve hiring additional school counsellors, establishing peer support groups, 
          and providing trauma-informed care for students who have experienced mistreatment or trauma.")
      )
     )
   )
)





# Define server logic required to draw 
server <- function(input, output) {
  
  
  #Shooting Type
  output$shootingtype_plot <- renderPlotly({
    a <- ggplot(data = shooting_data1, aes(x=shooting_type_edited,
                                          text = paste("Shooting Type: ",shooting_type_edited, "<br>",
                                                       "Total Shootings: ", Incidents1))) +
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
    b <- ggplot(data = shooting_data2[!is.na(shooting_data2$shooter_relationship_edited), ], aes(x = shooter_relationship_edited, 
                                                                                                 text = paste("Shooter Relation: ",shooter_relationship_edited, "<br>",
                                                                                                              "Total Shootings: ", Incidents2))) +
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
