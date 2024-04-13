library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(bslib)
library(slickR)

shooting_data <- read.csv("school-shootings-data-access.csv")
df <- data.frame(shooting_data)

shooting_data <- shooting_data %>%
  group_by(Weapon_source_edited) %>%
  mutate(Incidents = n())

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
             multiple firearms [6]. However, the storage of these firearms appears to be relatively 
             accessible to children, with 9% of homes leaving their firearms unlocked and loaded, 
             and an additional 4% leaving them unlocked and unloaded but stored with ammunition. 
             This leaves 13% of homes particularly vulnerable to children gaining access to firearms. The 
             heightened accessibility to weapons increases the risk of unintentional shootings by four times, 
             the risk of suicide by four times, and the risk of homicide by three times [7]. Moreover, 
             the next prevalent method of firearm acquisition is through legal purchases by the shooters 
             themselves. According    to Siegel et al. (2013), there is a notable correlation between higher 
             levels of gun ownership and increased firearm homicide rates [8]. Thus, gun availability is a 
             risk factor for homicide.",  style="text-align: justify; font-size: 15px;"),

  
  br(),
  
  fluidRow(
    column(6, 
           card(
             style = " font-size: 13px; background-color: #E9C874; font-family: Arial, sans-serif;", 
             card_header(
               style = "background-color: #9CAFAA;",
               strong("What can be done?", style="text-align: justify; font-size: 15px;")),
    markdown(
      "- **Safe gun storage:** Implementing secure firearm storage practices in households 
      where children reside is paramount to prevent accidental access. This includes using 
      gun trigger locks, safes, and ensuring firearms are unloaded and stored separately from 
      ammunition. Additionally, parents should try to limit unsupervised access to firearms by adolescents.
      - **Firearm Regulation:** Measures should be imposed to reduce availability of firearms to 
      individuals who may pose a risk to themselves or others, such as strengthening background check 
      requirements and implementing waiting periods for firearm purchases.
      - **Legislative Measures:** Advocate for legislation that promotes safe firearm storage practices, 
      such as laws requiring firearms to be stored securely when not in use. Additionally, support policies 
      that require background checks for all firearm purchases to deter individuals with a history of 
      violence or mental illness from acquiring firearms.")
  )),
  
  column(6,
         card(style = "font-size: 13px; background-color: white; font-family: Arial, sans-serif;", 
              card_header(
                style = "background-color: #9CAFAA;",
                strong("View Firearms Used in Deadliest School Shootings:", style="text-align: justify; font-size: 15px;")),
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
      geom_bar(fill = "#769776") +
      labs(title ="Source of Shooters' Weapon",
           x = "Weapon Source",
           y = " Number of School Shootings") +
      theme_minimal() + coord_flip()
    ggplotly(b, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })

  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
