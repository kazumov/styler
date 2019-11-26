# ggplot2 style selector for the heatmaps
#
# author: Ruben R. Kazumov
#

library(shiny)
library(tidyverse) 

paletteNames <- RColorBrewer::brewer.pal.info %>% rownames()

paletteDescriptions <- paletteNames

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("GGPlot2 Palettes"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "palette", 
                     label = "Color Style",
                     choiceNames = paletteDescriptions,
                     choiceValues = paletteNames,
                     selected = paletteNames[1]),
        radioButtons(inputId = "direction",
                     label = "Direction",
                     choiceNames = c("Stright", "Backward"),
                     choiceValues = c(1, -1),
                     selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("heatmap"),
         plotOutput("pal")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  x <- LETTERS[1:20]
  y <- paste0("var", seq(1,20))
  data <- expand.grid(X=x, Y=y)
  data$Z <- runif(400, 0, 5)
  
   output$heatmap <- renderPlot({
      # draw the histogram with the specified number of bins
     data %>% 
       ggplot(aes(X, Y, fill = Z)) + 
       geom_tile() +
       scale_fill_distiller(palette = input$palette, direction = input$direction) +
       labs(title = input$palette)
   })
   
   output$pal <- renderPlot({
     RColorBrewer::display.brewer.pal(n = 10, name = input$palette)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

