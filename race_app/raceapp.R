library(treemap)
library(plotly)
library(tidyr)
library(dplyr)


shooting_data_1 <- read.csv("school-shootings-data-race-2.csv")


# Define UI for application 
ui <- fluidPage(
  plotOutput("race_plot")
  
)


# Define server logic required to draw 
server <- function(input, output) {
  output$race_plot <- renderPlot({
    treemap(shooting_data_1,
            index=c("majority_race", "school_name"),
            vSize="casualties",
            type="index",
            border.col=c("black","white"),  
            border.lwds=c(3,1.7), 
            mirror.y = TRUE,
            mirror.x = TRUE,
            aspRatio = 5/3,
            palette = "Set2",
            title = "Majority Race of Schools (Size = Number of Casualties)",
            fontsize.title=15, 
            bg.labels=c("transparent"),
            align.labels=list(
              c("center", "center"), 
              c("right", "bottom")),
            overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
            inflate.labels=F) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
