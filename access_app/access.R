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
           p("A significant portion of shooters acquired their weapons used in shooting 
             incidents through theft from their family members or homes, with a striking 
             87% of them falling within the adolescent age range of 10 to 19. 35% of families 
             with children reported owning at least one firearm, and of these, 69% possessed 
             multiple firearms [3]. However, the storage of these firearms appears to be relatively 
             accessible to children, with 9% of homes leaving their firearms unlocked and loaded, 
             and an additional 4% leaving them unlocked and unloaded but stored with ammunition. 
             This leaves 13% of homes particularly vulnerable to children gaining access to firearms. The 
             heightened accessibility to weapons increases the risk of unintentional shootings by four times, 
             the risk of suicide by four times, and the risk of homicide by three times [4]. Moreover, 
             the next prevalent method of firearm acquisition is through legal purchases by the shooters 
             themselves. According    to Siegel et al. (2013), there is a notable correlation between higher 
             levels of gun ownership and increased firearm homicide rates [5]. Thus, gun availability is a 
             risk factor for homicide.",  style="text-align: justify; font-size: 15px;"),

  
  br(),
  
  fluidRow(
    column(6, 
  card(
    style = "font-size: 13px; background-color: white; font-family: Arial, sans-serif;", 
    card_header(
      class = "bg-dark text-white",
      "What can be done?"
    ),
    markdown(
      "Insert Solution")
  )),
  
  column(6,
         card(style = "font-size: 13px; background-color: white; font-family: Arial, sans-serif;", 
              card_header(
                class = "bg-dark text-white",
                "View Firearms Used in Deadliest School Shootings:"
              ),
              slickROutput("slickr", width="300px"))
  ))
  
  
  
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
    b <- ggplot(data = shooting_data[!is.na(shooting_data$Weapon_source_edited), ], aes(x = Weapon_source_edited,
                                                                                        text = paste("Weapon Source: ",Weapon_source_edited, "<br>",
                                                                                                     "Total Shootings: ", Incidents))) +
      geom_bar(fill = "#dee2d0") +
      labs(title ="Sources of Shooters' Weapons",
           x = "Weapon Source",
           y = " Number of School Shootings") +
      theme_minimal() + coord_flip()
    ggplotly(b, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })

  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
