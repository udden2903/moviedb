rm(list = ls())

library(shiny)
library(jsonlite)
library(tidyverse)
library(plotly)

#Insert API key below:
secret_key <- "Enter API key here"

genres <- fromJSON(sprintf("https://api.themoviedb.org/3/genre/movie/list?api_key=%s&language=en-US", secret_key))$genres %>% as_tibble()
your_choices <- as.list(genres$id)
names(your_choices) <- genres$name

ui <- fluidPage(
   
   titlePanel("The Movie DB"),
   
   # Sidebar with dropdown lists for year and genre.
   sidebarLayout(
      sidebarPanel(
        selectInput("selectYr",
                     "Year:",
                    choices = seq(2000, 2017, 1),
                     selected = 2016),
        selectInput("selectGenre", "Genre:", 
                    choices = your_choices)
      ),

      mainPanel(
         plotlyOutput("moviePlot"),
         verbatimTextOutput("movieText")
      )
   )
)


server <- function(input, output) {
  
  #Retrieve data from API
  movies_tibble <- reactive({fromJSON(
    sprintf("http://api.themoviedb.org/3/discover/movie?with_genres=%s&primary_release_year=%s&sort_by=vote_average.desc&vote_count.gte=10&api_key=%s",
            input$selectGenre,
            input$selectYr,
            secret_key))$results %>% select(title, original_language, release_date, popularity, vote_count, vote_average, overview) %>% as_tibble()
    })
  
   output$moviePlot <- renderPlotly({
     plot_ly(movies_tibble(), x = ~vote_average, y = ~vote_count, text = ~title, color = ~original_language, key = ~title) %>% 
       config(displayModeBar = F)
   })
   
   output$movieText <- renderPrint({
     d <- event_data("plotly_hover")
     if (is.null(d)) "Hover on a point to see synopsis" else movies_tibble()$overview[movies_tibble()$title == unlist(d$key)]
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

