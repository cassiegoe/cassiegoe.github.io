library(plotly)
library(dplyr)
library(readr)
library(bslib)

states <- read_csv("table-data.csv")
shooting_data_map <- read.csv("school-shootings-data-map.csv")



fontStyle = list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)


# Define UI for application 
ui <- fluidPage(

navbarPage("Filter by:",
           tabPanel("Total Incidents Across Years",
           mainPanel(plotlyOutput("choropleth",width = "150%"))),
           
           tabPanel("Total Incidents by Year",
            mainPanel(plotlyOutput("choropleth2", width = "150%")))
)

) 
  


# Define server logic required to draw 
server <- function(input, output) {
  
  
  output$choropleth <- renderPlotly({
    
    shooting_data_map <- shooting_data_map %>%
      group_by(State) %>%
      mutate(total_shootings = n()) 
    
    joined_data <- shooting_data_map %>%
      inner_join(states, by = c("State" = "State"))%>%
      select(State,year,code,total_shootings ) %>%
      mutate(hover = paste0(State,": " ,total_shootings))
    
    states_graph = plot_geo(joined_data,
                            locationmode = 'USA-states') %>%
      add_trace(locations = ~code,
                z = ~total_shootings,
                zmin = 0,
                zmax = max(joined_data$total_shootings),
                color = ~total_shootings,
                colorscale = 'YIOrRd',
                reversescale = TRUE,
                text = ~hover,
                hoverinfo = 'text') %>%
      layout(geo = list(scope = 'usa'),
             font = list(family ="DM Sans"),
             title = "School Shootings in the US\n1999 - 2023 ") %>%
      style(hoverlabel = label) %>%
      config(displayModeBar = FALSE)
    
    ggplotly(states_graph)
  })
  
  output$choropleth2 <- renderPlotly({
    
    shooting_data_map <- shooting_data_map %>%
      group_by(State, year) %>%
      mutate(total_shootings = n()) 
    
    joined_data <- shooting_data_map %>%
      inner_join(states, by = c("State" = "State"))%>%
      select(State,year,code,total_shootings ) %>%
      mutate(hover = paste0(State,": " ,total_shootings))
    
    states_graph2 = plot_geo(joined_data,
                            locationmode = 'USA-states',
                            frame=~year) %>%
      add_trace(locations = ~code,
                z = ~total_shootings,
                zmin = 0,
                zmax = max(joined_data$total_shootings),
                color = ~total_shootings,
                colorscale = 'YIOrRd',
                reversescale = TRUE,
                text = ~hover,
                hoverinfo = 'text') %>%
      layout(geo = list(scope = 'usa'),
             font = list(family ="DM Sans"),
             title = "School Shootings in the US\n1999 - 2023 ") %>%
      style(hoverlabel = label) %>%
      config(displayModeBar = FALSE)
    
    ggplotly(states_graph2)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
