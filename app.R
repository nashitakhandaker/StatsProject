# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/ / https://shiny.posit.co/r/reference/shiny/0.14/fluidpage.html
library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("train.csv")

# Filtering for only the required neighborhoods
data_filtered <- data %>%
  filter(Neighborhood %in% c("NAmes", "Edwards", "BrkSide"))

# Rshiny application
ui <- fluidPage(
  titlePanel("Home Prices vs Living Area"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood",
                  "Select Neighborhood:",
                  choices = c("All", "NAmes", "Edwards", "BrkSide"),
                  selected = "All")
    ),
    
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    if (input$neighborhood == "All") {
      return(data_filtered)
    } else {
      return(data_filtered %>%
               filter(Neighborhood == input$neighborhood))
    }
  })
  
  # Graphs representation
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = GrLivArea, y = SalePrice)) +
      geom_point(color = "steelblue", alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        title = "Sale Price vs Living Area",
        x = "Living Area (sq ft)",
        y = "Sale Price ($)"
      ) +
      theme_minimal()
  })
}

# Launch
shinyApp(ui = ui, server = server)


