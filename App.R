library(shiny)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)
library(plotly)

# Pre-processing
# Read in the file, re-code the lowercase states, & get the unique states
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
beer_awards <- beer_awards %>% mutate(state = 
                                        recode(state, "wa" = "WA",
                                               "Ak" = "AK"))
states <- distinct(beer_awards, state)

# Function that will be called by event reactive on button click 
getDetails <- function(s, startYear, endYear) {
  sYear <- as.numeric(substr(startYear, 1, 4))
  eYear <- as.numeric(substr(endYear, 1, 4))
  
  medals <- filter(beer_awards, state == s & year >= sYear & year <= eYear)
  # print(eYear)
  # glimpse(medals)
  return (medals)
}

# Basic UI for Shiny app with components
ui <- fluidPage(
  titlePanel("Great American Beer Festival"),
  sidebarLayout(
    sidebarPanel(
      selectInput("states_inp", "Choose a state:", choices = states),
      airDatepickerInput("startYear_inp",
                         label = "Start Year:",
                         value = "1987-1-01",
                         maxDate = "2020-12-30",
                         minDate = "1987-01-01",
                         view = "years",
                         minView = "years",
                         dateFormat = "yyyy" ),
      airDatepickerInput("endYear_inp",
                         label = "End Year:",
                         value = "2020-1-01",
                         maxDate = "2020-12-30",
                         minDate = "1987-01-01",
                         view = "years",
                         minView = "years",
                         dateFormat = "yyyy"),
      actionButton("run", "Run App")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Medals by State", plotlyOutput("medal", height = "600px")),
                  tabPanel("Breweries by State", plotOutput("brewery", height = "600px"))
      )
    )
  )
)

# Server side logic for UI components with reactive elements
server <- function(input, output) {
  
   evt <- eventReactive(input$run, {
    withProgress({
      setProgress(message = "Processing....")
      getDetails(input$states_inp, input$startYear_inp, input$endYear_inp)
    })
  })
   
   output$medal <- renderPlotly({
     m <- evt()
     md <- m %>%
       group_by(year, medal) %>%
       mutate(counts = n())
     
     p <- ggplot(md ,aes(
       x = year,
       fill = medal)) +
       geom_bar(position="stack", 
                stat='count')
     ggplotly(p)
   })
   
}

shinyApp(ui = ui, server = server)
