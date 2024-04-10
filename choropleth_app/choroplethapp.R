library(plotly)
library(dplyr)
library(readr)

states <- read_csv("table-data.csv")
shooting_data_map <- read.csv("school-shootings-data-map.csv")

shooting_data_map <- shooting_data_map %>%
  group_by(State) %>%
  mutate(total_shootings = n()) 

joined_data <- shooting_data_map %>%
  inner_join(states, by = c("State" = "State"))%>%
  select(State,year,code,total_shootings ) %>%
  mutate(hover = paste0(State,": " ,total_shootings))

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
  
  plotlyOutput("choropleth")
)
  
  


# Define server logic required to draw 
server <- function(input, output) {
  
  output$choropleth <- renderPlotly({
    states_graph = plot_geo(joined_data,
                                 locationmode = 'USA-states',
                                 frame = ~year) %>%
      add_trace(locations = ~code,
                z = ~total_shootings,
                zmin = 0,
                zmax = max(joined_data$total_shootings),
                color = ~total_shootings,
                colorscale = 'Electric',
                text = ~hover,
                hoverinfo = 'text') %>%
      layout(geo = list(scope = 'usa'),
             font = list(family ="DM Sans"),
             title = "School Shootings in the US\n1999 - 2023 ") %>%
      style(hoverlabel = label) %>%
      config(displayModeBar = FALSE)
    ggplotly(states_graph)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
