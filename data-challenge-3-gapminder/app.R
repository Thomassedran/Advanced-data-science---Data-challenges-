library(shiny)
library(tidyverse)
library(gapminder)

ui <- fluidPage(
  
  titlePanel("Gapminder Data"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year",
        "Year",
        min = min(gapminder$year),
        max = max(gapminder$year),
        step = 5,
        value = min(gapminder$year)
      ),
      
      selectInput(
        "continent",
        "Continent",
        choices = c("Africa", "Americas", "Asia", "Europe", "Oceania", "All"),
        selected = "All"
      ),
      
      radioButtons(
        "x",
        "X",
        choices = list("Life expectancy" = "lifeExp",
                       "Population" = "pop",
                       "GDP per capita" = "gdpPercap"),
        selected = "gdpPercap"
      ),
      
      radioButtons(
        "y",
        "Y",
        choices = list("Life expectancy" = "lifeExp",
                       "Population" = "pop",
                       "GDP per capita" = "gdpPercap"),
        selected = "lifeExp"
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    
    gapminder |>
      filter(
        year == input$year,
        input$continent == "All" | continent == input$continent
      )
    
  })
  
  output$plot <- renderPlot({
    
    df() |>
      ggplot(aes_string(input$x, input$y, size = "pop", colour = "country")) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal()
    
  })
  
}

shinyApp(ui, server)
