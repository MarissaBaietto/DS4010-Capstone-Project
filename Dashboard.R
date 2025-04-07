library(readr)
library(ggplot2)
library(shiny)
library(dplyr)
library(purrr)
library(gapminder)
library(highcharter)
library(shinyWidgets)
library(shinydashboard)
library(mapdata)
library(maps)

ui <- dashboardPage(
  dashboardHeader(title = "Graphs"),
  dashboardSidebar(disable = TRUE), # Removes sidebar
  dashboardBody(
    fluidRow(
      column(12,
             sliderInput("year_range",
                         "Select Year Range:",
                         min = 2014, max = 2024,
                         value = c(2015, 2020),
                         step = 1,
                         animate = FALSE,
                         ticks = TRUE,
                         sep = "",
                         width = "100%")  # Full width for better visibility
      )
    ),
    
    fluidRow(
      column(6,
             plotOutput("plot3")
      ),
      column(6,
             plotOutput("plot1"),
             selectInput(
               inputId = "y_axis_monthly",
               label = "Select a y axis variable:",
               choices = c("Average crash severity" = "avg_severity", "Number of accidents" = "num_accidents")
             ))),
    fluidRow(
      column(12,
             box(plotOutput("plot2"))
      )
    )
  )
)

server <- function(input, output) {
  
  Unique_Crashes <- read.csv("Unique Crashes.csv")
  Iowa_County_Merged <- read.csv("Iowa Counties.csv")
  state <- map_data("state")
  iowa <- subset(state, region=="iowa")
  
  print("Server is running")
  filtered_unique <- reactive({
    Unique_Crashes %>% filter(Year >= min(input$year_range) & Year <= max(input$year_range))
  })
  
  filtered_counties <- reactive({
    Iowa_County_Merged %>% filter(Year >= min(input$year_range) & Year <= max(input$year_range))
  })
  
  iowa_filtered <- reactive({
    iowa
  })
  
  unique_crashes <- reactive({
    Unique_Crashes %>% filter(Year != 2025)
  })
  
  
  
  output$plot1 <- renderPlot({
    filtered_unique() %>%
      group_by(Month.of.Crash) %>%
      mutate(Crash.Severity.Numeric = as.numeric(factor(Crash.Severity, 
                                                        levels = c("Property Damage Only", "Possible/Unknown", "Minor Injury", "Major Injury", "Fatal"), 
                                                        labels = c(1, 2, 3, 4, 5)))) %>% 
      summarise(
        avg_severity = mean(Crash.Severity.Numeric, na.rm=TRUE),
        num_accidents = n()) %>%
      ggplot(aes(x = Month.of.Crash)) +
      geom_bar(aes_string(y = input$y_axis_monthly), stat = "identity") +
      theme_minimal()
  })
  
  output$plot3 <- renderPlot({
    unique_crashes() %>%
      group_by(Year) %>%
      summarise(Number.of.Crashes = n()) %>%
      mutate(Selected = ifelse(Year >= min(input$year_range) & Year <= max(input$year_range), "Selected", "Not Selected")) %>%
      ggplot(aes(x = Year, y = Number.of.Crashes, fill = Selected)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Selected" = "red", "Not Selected" = "gray")) +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    ggplot(data = iowa_filtered(), mapping = aes(x=long, y=lat, group=group)) +
      coord_fixed(1.3) +
      geom_polygon(color="black", fill="white") +
      geom_polygon(data=filtered_counties(), aes(fill = log10(`Number.of.Crashes`)), color="black") +
      theme_classic() +
      scale_fill_gradientn(colors = c("blue", "lightblue", "white", "pink", "red"))
    
  })
}


shinyApp(ui = ui, server = server)
