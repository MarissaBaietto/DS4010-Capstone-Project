library(readr)
library(ggplot2)
library(shiny)
library(dplyr)
library(purrr)
library(tidyr)
library(gapminder)
library(highcharter)
library(shinyWidgets)
library(shinydashboardPlus)
library(mapdata)
library(maps)
library(plotly)
library(shinydashboard)
library(scales)
library(ggthemes)
library(stringr)
library(sf)
library(bslib)
library(ggalt)
options(shiny.trace = TRUE)
library(fresh)
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#e63946",   # header, primary button, etc.
    aqua       = "#8ecae6",   # info elements
    navy = "#3a86ff",
    fuchsia = "#ff0054"
  ),
  adminlte_sidebar(
    dark_bg = "#1d3557",       # sidebar background
    dark_hover_bg = "#457b9d", # sidebar hover color
    dark_color = "white"     # sidebar text color
  )
  # adminlte_global(
  #   content_bg = "#f2e9e4",
  #   box_bg = "#f0f0f0",
  #   info_box_bg = "#f0f0f0"
  # )
)

iowa_counties <- c(
  "Adair", "Adams", "Allamakee", "Appanoose", "Audubon", "Benton", "Black Hawk", "Boone", "Bremer", "Buchanan",
  "Buena Vista", "Butler", "Calhoun", "Carroll", "Cass", "Cedar", "Cerro Gordo", "Cherokee", "Chickasaw", "Clarke",
  "Clay", "Clayton", "Clinton", "Crawford", "Dallas", "Davis", "Decatur", "Delaware", "Des Moines", "Dickinson",
  "Dubuque", "Emmet", "Fayette", "Floyd", "Franklin", "Fremont", "Greene", "Grundy", "Guthrie", "Hamilton",
  "Hancock", "Hardin", "Harrison", "Henry", "Howard", "Humboldt", "Ida", "Iowa", "Jackson", "Jasper",
  "Jefferson", "Johnson", "Jones", "Keokuk", "Kossuth", "Lee", "Linn", "Louisa", "Lucas", "Lyon",
  "Madison", "Mahaska", "Marion", "Marshall", "Mills", "Mitchell", "Monona", "Monroe", "Montgomery", "Muscatine",
  "O'Brien", "Osceola", "Page", "Palo Alto", "Plymouth", "Pocahontas", "Polk", "Pottawattamie", "Poweshiek", "Ringgold",
  "Sac", "Scott", "Shelby", "Sioux", "Story", "Tama", "Taylor", "Union", "Van Buren", "Wapello", "Warren",
  "Washington", "Wayne", "Webster", "Winnebago", "Winneshiek", "Woodbury", "Worth", "Wright"
)

# Original values
hours_24 <- c("1100 Hours", "1600 Hours", "2100 Hours", "0600 Hours", "1000 Hours", "0900 Hours",
              "1200 Hours", "1500 Hours", "0700 Hours", "1700 Hours", "2000 Hours", "1300 Hours",
              "0000 Hours", "1900 Hours", "1800 Hours", "0100 Hours", "0400 Hours", "0800 Hours",
              "1400 Hours", "0300 Hours", "2300 Hours", "0200 Hours", "2200 Hours", "0500 Hours")

# Convert to 12-hour format
hours_12 <- format(strptime(gsub(" Hours", "", hours_24), "%H%M"), "%I:%M %p")

# Remove leading zero
hours_12 <- sub("^0", "", hours_12)

hours_named <- setNames(hours_24, hours_12)

format_value <- function(value) {
  if (value > 1) {
    return(paste(round((round(value, 2) - 1)*100,0), "%", "higher"))
  } else if (value < 1) {
    return(paste((round(1 - value, 2))*100, "%", "lower"))
  } else {
    return("Equal")
  }
}

format_value(0.9773854)

get_box_color <- function(odds_ratio) {
  if (odds_ratio > 1.2) {
    return("red")
  } else if (odds_ratio > 1) {
    return("yellow")
  } else {
    return("green")
  }
}


create_value_boxes <- function(values, subtitles) {
  return(mapply(function(value, subtitle) {
    valueBox(
      value = format_value(value), 
      subtitle = subtitle, 
      icon = icon("exclamation-circle"), 
      color = get_box_color(value), 
      width = 4
    )
  }, values, subtitles))
}

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Iowa Traffic Safety"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",  # FIXED this
      menuItem("Information", tabName = "information", icon = icon("info-circle")),
      menuItem("Graphics", tabName = "graphics", icon = icon("chart-bar"),
      menuSubItem("Summary Statistics", tabName = "summary"),
      menuSubItem("When Crashes Occured", tabName = "when"),
      #menuSubItem("When Crashes Occured Cont", tabName = "when2"),
      menuSubItem("Where Crashes Occured", tabName = "where"),
      #menuSubItem("Where Crashes Occured Cont", tabName = "where2"),
      menuSubItem("Driver Characteristics", tabName = "driver")),
      menuItem("Machine Learning Models", tabName = "model", icon = icon("robot"),
      menuSubItem("Crash Severity Factors", tabName = "severity"),
      menuSubItem("Predicting Property Damage", tabName = "damage")),
      
      conditionalPanel(
        condition = "input.sidebar %in% c('summary','when')",
        tags$h3("Filters"),
        sliderInput(
          "year_range",
          "Select Year Range:",
          min = 2014,
          max = 2024,
          value = c(2014, 2024),
          step = 1,
          ticks = TRUE,
          sep = "",
          width = "100%"
        )
      )
    )
  ),
  
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      tabItem(
        tabName = "information",
        fluidRow(
          column(12, h2("Dashboard Information", class = "text-center mb-4")),
          box(
            title = "Data Source",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            HTML('This dashboard is built using crash data from the Iowa Deparment of Transportation website. It contains all reported vehicle crashes in Iowa from 2014 to 2024.
')),
            box(
              title = "Project Goal",
              width = 6,
              solidHeader = TRUE,
              status = "primary",
              HTML('This dashboard is designed for the driving population of Iowa and driving safety advocacy groups, such as the Central Iowa Traffic Safety Task Force.

By providing pre-built visualizations and summary statistics, the dashboard empowers non-data scientists to explore traffic trends and identify key safety concerns. Its interactive nature allows users to generate custom figures for public education and policy recommendations. 
')
        ),
        column(12, h2("Dashboard Layout", class = "text-center mb-4")),
        box(
          title = "Graphics Tab",
          width = 6,
          solidHeader = TRUE,
          status = "info",
          HTML('
          Select year range using the input slider on the left side bar to filter data.
          <ul>
    <li>
      <strong>Summary Statistics</strong> 
      <ul>
      <li>Total Number of Crashes, Property Damage, Fatalities, Injuries</li>
      <li>Total number of accidents per year</li>
      <li>Driver characteristics</li>
    </ul>
    </li>
    <li>
      <strong>When Crashes Occured</strong> 
      <ul>
      <li>Number of crashes by month</li>
      <li>Number of crashes by day for selected month</li>
    </ul>
    </li>
    <li>
      <strong>Where Crashes Occured</strong> 
            <ul>
      <li>Map of log number of crashes by county</li>
      <li>Map of crash latitude and longitude by county</li>
    </ul>
    </li>
    <li>
      <strong>Driver Characteristics</strong>
                  <ul>
      <li>Age of drivers by sex</li>
      <li>Total number of male and female drivers</li>
    </ul>
    </li>
  </ul>
')),
          box(
            title = "Machine Learning Models Tab",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            HTML('<ul>
    <li>
      <strong>Crash Severity Factors</strong> – Explore how different crash factors affect the odds of a crash resulting in bodily harm.
    </li>
    <li>
      <strong>Predicting Property Damage</strong> – Input values for each crash factor to predict the amount of property damage resulting from the crash.
    </li>
  </ul>
'))
        )),
      # GRAPHICS TAB ----
      tabItem(
        tabName = "summary",
        fluidRow(
          column(12, h2("Summary Statistics" , class = "text-center mb-4")),
          valueBoxOutput("vbox_crashes", width = 3),
          valueBoxOutput("vbox_people", width = 3),
          valueBoxOutput("vbox_vehicles", width = 3),
          valueBoxOutput("vbox_propertydamage", width = 3),
          valueBoxOutput("vbox_fatalities", width = 3),
          valueBoxOutput("vbox_injuries", width = 3),
          valueBoxOutput("vbox_majorinjuries", width = 3),
          valueBoxOutput("vbox_minorinjuries", width = 3),
        box(
          title = "Number of Accidents Per Year",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot_years")
        )
        )),
      tabItem(
        tabName = "when",
        fluidRow(
          column(
            12,
            h2("When Crashes Occurred", class = "text-center mb-4"),
            tags$p("Explore patterns by month, day of week, and time of day to identify peak crash periods.",
                   style = "font-size:16px; color:gray; text-align:center;")
          )
        ),
        
        fluidRow(
          box(
            title = "Crashes by Month",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_month")
          ),
          box(
            title = "Crashes by Day of Month",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("month", "Select Month", choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
            plotlyOutput("plot_day")
          )
        )
      ),
      # tabItem(
      #   tabName = "when2",
      #   fluidRow(
      #     column(
      #       12,
      #       h2("When Crashes Occurred Continued", class = "text-center mb-4"),
      #       tags$p("Explore patterns by month, day of week, and time of day to identify peak crash periods.",
      #              style = "font-size:16px; color:gray; text-align:center;")
      #     )
      #   ),
      #   
      #   fluidRow(
      #     box(
      #       title = "Crash Frequency by Hour and Weekday",
      #       status = "info",
      #       solidHeader = TRUE,
      #       width = 12,
      #       plotOutput("plot_time"),
      #       tags$p("This heatmap reveals when crashes are most likely to occur during the week.",
      #              style = "font-size:14px; color:gray;")
      #     )
      #   )
      # ),
      tabItem(
        tabName = "where",
        fluidRow(
          column(12, h2("Where Crashes Occurred", class = "text-center mb-4"),
                 tags$p("Explore patterns by county and speed limit to identify peak crash location factors",
                        style = "font-size:16px; color:gray; text-align:center;")),
        box(
          title = "Crashes Per County",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput("plot_iowa"),
          tags$p("This map reveals which counties have the least and most number of crashes.",
                 style = "font-size:14px; color:gray;")
        ),
        box(
          title = "Crash Locations",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          selectInput("county", "Select County", choices = iowa_counties),
          tags$p("This map reveals which the location of each crash that occured in the selected county."),
          plotlyOutput("plot_locations")
        )
        )),
        # tabItem(
        #   tabName = "where2",
        #   fluidRow(
        #     column(12, h2("Where Crashes Occurred Continued", class = "text-center mb-4"),
        #            tags$p("Explore patterns by county and speed limit to identify peak crash location factors",
        #                   style = "font-size:16px; color:gray; text-align:center;")),
        #     box(
        #       title = "Road Speed Limit",
        #       status = "info",
        #       solidHeader = TRUE,
        #       width = 12,
        #       tags$p("This scatterplot reveals the number of crashes that occurred by speed limit, with the size of the dots equaling the average crash severity for the crashes that occurred."),
        #       plotOutput("plot_speed")
        #     ))),
      tabItem(
        tabName = "driver",
        fluidRow(
          column(12, h2("Driver Characteristics", class = "text-center mb-4"),
                 tags$p("Explore patterns by age and gender to identify discreptencies in number of crashes and crash severity",
                        style = "font-size:16px; color:gray; text-align:center;")),
        box(
          title = "Driver Age and Sex",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot_age")
        ),
        # box(
        #   title = "Number of Crashes and Crash Severity by Gender",
        #   status = "info",
        #   solidHeader = TRUE,
        #   width = 6,
        #   plotlyOutput("plot_gender")
        # ),
        valueBoxOutput("vbox_drivermale", width = 6),
        valueBoxOutput("vbox_driverfemale", width = 6),
        valueBoxOutput("vbox_driverfemale", width = 6),
        valueBoxOutput("vbox_driverfemale", width = 6)
       )),
      
      # MODEL TAB ----
      tabItem(
        tabName = "severity",
        column(12,h2("Understanding the Contributors to Crash Severity", class = "text-center mb-4"),
               tags$p("Explore how the following factors affect the odds of bodily harm resulting from a crash, compared to a baseline group.",
                      style = "font-size:16px; color:gray; text-align:center;")),
        box(
          title = "Model Information",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          HTML("<ul>
          <p>
      <strong>Bodily Harm</strong> indicates whether fatalities, major injuries, or minor injuries sustained by anyone involved in the crash.
      </p>
      <p>
      <strong>Odds</strong> are a way of expressing risk. For example, if the odds of bodily harm are 13% higher for a certain factor, that means bodily harm is 1.13 times as likely to occur compared to the baseline.
</p>
<p>
Note that this is different from probability — a 13% increase in odds doesn’t mean there's a 13% higher chance overall, but it does indicate increased risk.
    </p>
               </ul>")),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Drug Results")),
          column(12,h4("Compared to none indicated, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(1.1743187), 
            subtitle = "Alcohol (< Statutory)", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.1743187), 
            width = 3
          ),
          valueBox(
            value = format_value(1.1356980), 
            subtitle = "Alcohol (Statutory)", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.1356980), 
            width = 3
          ),
          valueBox(
            value = format_value(1.4097216), 
            subtitle = "Drug", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.4097216), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Environmental Conditions")),
          column(12,h4("Compared to none apparent, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(0.9184532), 
            subtitle = "Animal in roadway", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9184532), 
            width = 3
          ),
          valueBox(
            value = format_value(1.2446006), 
            subtitle = "Non-motorist action", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.2446006), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0489452), 
            subtitle = "Visual obstruction", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0489452), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Light Conditions")),
          column(12,h4("Compared to daylight, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(0.9773854), 
            subtitle = "Dark - roadway lighted", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9773854), 
            width = 3
          ),
          valueBox(
            value = format_value(0.9848399), 
            subtitle = "Dark - roadway not lighted", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9848399), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Surface Conditions")),
          column(12,h4("Compared to dry, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(1.0930576), 
            subtitle = "Gravel", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0930576), 
            width = 3
          ),
          valueBox(
            value = format_value(0.9800666), 
            subtitle = "Ice/frost", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9800666), 
            width = 3
          ),
          valueBox(
            value = format_value(1.2138796), 
            subtitle = "Sand", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.2138796), 
            width = 3
          ),
          valueBox(
            value = format_value(0.9457042), 
            subtitle = "Slush", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9457042), 
            width = 3
          ),
          valueBox(
            value = format_value(0.9618808), 
            subtitle = "Snow", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9618808), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Weather Conditions")),
          column(12,h4("Compared to clear, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(0.8747050), 
            subtitle = "Blowing sand, soil, dirt", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.8747050), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0388861), 
            subtitle = "Fog, smoke, smog", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0388861), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0731693), 
            subtitle = "Severe winds", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0731693), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Work Zone")),
          column(12,h4("Compared to no work zone, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(0.9618519), 
            subtitle = "Yes", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(0.9618519), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Driver Gender")),
          column(12,h4("Compared to female, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(1.0150797), 
            subtitle = "Male", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0150797), 
            width = 3
          )
        ),
        fluidRow(
          # Value Box for Drug Results
          column(12,h3("Speed Limit")),
          column(12,h4("Compared to 10 MPH, the odds of a crash resulting in bodily harm is")),
          valueBox(
            value = format_value(1.0481824), 
            subtitle = "30", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0481824), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0561113), 
            subtitle = "35", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0561113), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0521148), 
            subtitle = "40", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0521148), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0699168), 
            subtitle = "45", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0699168), 
            width = 3
          ),
          valueBox(
            value = format_value(1.1266324), 
            subtitle = "50", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.1266324), 
            width = 3
          ),
          valueBox(
            value = format_value(1.2060333), 
            subtitle = "55", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.2060333), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0649562), 
            subtitle = "60", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0649562), 
            width = 3
          ),
          valueBox(
            value = format_value(1.1145485), 
            subtitle = "65", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.1145485), 
            width = 3
          ),
          valueBox(
            value = format_value(1.0873463), 
            subtitle = "70", 
            icon = icon("exclamation-circle"), 
            color = get_box_color(1.0873463), 
            width = 3
          )
        )),
        tabItem(
          tabName = "damage",
        column(12,h2("Predicting the Resulting Property Damage", class = "text-center mb-4"),
               tags$p("Input values for the following variables to predict the amount of property damage resulting from the crash.",
                      style = "font-size:16px; color:gray; text-align:center;")),
        sliderTextInput(
          inputId = "month_slider",
          label = "Select Month of Crash",
          choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
          selected = "Jan",
          grid = TRUE
        ),
        sliderTextInput(
          inputId = "day_slider",
          label = "Select Day of Week",
          choices = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
          selected = "Sun",
          grid = TRUE
        ),
        sliderTextInput(
          inputId = "hour_slider",
          label = "Select Hour of Day",
          choices = hours_named,
          selected = hours_named["12:00 PM"],  # or whatever you want to default to
          grid = TRUE
        ),
        selectInput("drugoralcohol", "Drug or Alcohol", choices = c("Alcohol (< Statutory)", "Alcohol (Statutory)", "Drug", "Drug/Alcohol (< Statutory)", "Drug/Alcohol (Statutory)", "None Indicated", "Refused", "Under Influence of Alcohol/Drugs/Medications")),
        selectInput("environmentalconditions", "Environmental Conditions", choices = c("Animal in roadway", "Glare", "Non-motorist action", "Severe crosswind", "Visual obstruction", "Weather conditions", "None apparent")),
        selectInput("lightconditions", "Light Conditions", choices = c("Dark - roadway lighted", "Dark - roadway not lighted", "Dark - unknown roadway lighting", "Dawn", "Dusk", "Daylight")),
        selectInput("weatherconditions", "Weather Conditions", choices = c("Blowing sand, soil, dirt", "Blowing snow", "Clear", "Cloudy", "Fog, smoke, smog", 
                                                                           "Freezing rain/drizzle", "Rain", "Severe winds", "Sleet, hail", "Snow")),
        selectInput(
          inputId = "surface_condition",
          label = "Select Surface Condition",
          choices = c(
            "Snow", "Dry", "Ice/frost", "Wet", "Slush", "Unknown", "Mud, dirt",
            "Water (standing or moving)", "Gravel", "Sand", "Oil"
          ),
          selected = "Dry"
        ),
        selectInput(
          inputId = "work_zone",
          label = "Select Whether Crash Occurred In A Work Zone",
          choices = c("No", "Yes"),
          selected = "No"
        ),
        selectInput("drivergen", "Driver Gender", choices = c("Male" = "M", "Female" = "F")),
        numericInput("driverage", "Driver Age:", value = 1, min = 12),
        selectInput("speedlimit", "Speed limit:", choices = c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75")),
        selectInput("drugresult", "Drug Result:",
                    choices = c("Negative" = "1",
                                "Cannabis" = "2",
                                "Central Nervous Sys. depressants" = "3",
                                "Central Nervous Sys. stimulants" = "4",
                                "Hallucinogens" = "5",
                                "Inhalants" = "6",
                                "Narcotic Analgesics" = "7",
                                "Dissociative Anesthetic (PCP)" = "8",
                                "Prescription Drug" = "9")
        ),
        numericInput("alcresult", "Driver's Blood Alcohol Level (BAC):", value = 0.11, min = 0),
        tags$head(
          tags$style(HTML("
    #predict_btn {
      background-color: #e63946;
      color: white;
      border-color: #e63946;
    }
    #predict_btn:hover {
      background-color: #e63946;
      border-color: #e63946;
    }
  "))
        ),
        actionButton("predict_btn", "Predict Property Damage"),
        br(),
        verbatimTextOutput("new_model_prediction")
      )
    )
  )
  
)

server <- function(input, output) {
  Unique_Crashes <- readRDS("Unique Crashes.rds")
  Iowa_County_Merged <- readRDS("Iowa Counties.rds")
  
  state <- readRDS("state.rds")
  iowa <- readRDS("iowa.rds") 
  
  print("Server is running")
  filtered_unique <- reactive({
    Unique_Crashes %>% filter(Year >= min(input$year_range) &
                                Year <= max(input$year_range))
  })
  
  filtered_counties <- reactive({
    Iowa_County_Merged %>% filter(Year >= min(input$year_range) &
                                    Year <= max(input$year_range))
  })
  
  iowa_filtered <- reactive({
    iowa
  })
  
  unique_crashes <- reactive({
    Unique_Crashes %>% filter(Year != 2025)
  })
  
  prediction_val <- reactiveVal()
  
  output$plot_month <- renderPlotly({
    data <- filtered_unique() %>%
      mutate(
        Crash.Severity.Numeric = as.numeric(factor(
          Crash.Severity,
          levels = c(
            "Property Damage Only",
            "Possible/Unknown",
            "Minor Injury",
            "Major Injury",
            "Fatal"
          ),
          labels = c(1, 2, 3, 4, 5)
        )),
        Month.of.Crash = factor(
          Month.of.Crash,
          levels = c(
            "Jan",
            "Feb",
            "Mar",
            "Apr",
            "May",
            "Jun",
            "Jul",
            "Aug",
            "Sep",
            "Oct",
            "Nov",
            "Dec"
          )
        )
      )
    
    # Conditionally group and summarize
    summary_data <-
      data %>%
      group_by(Month.of.Crash) %>%
      summarise(
        avg_severity = mean(Crash.Severity.Numeric, na.rm = TRUE),
        num_accidents = n(),
        .groups = 'drop'
      ) # Dummy year to keep fill consistent
    
    p <- ggplot(summary_data,
                aes(
                  x = Month.of.Crash,
                  y = num_accidents,
                  text = paste("Month:", Month.of.Crash, "<br>Accidents:", num_accidents)
                )) +
      geom_bar(stat = "identity", fill = "#457b9d") +
      labs(x = "Month of Crash", y = "Number of Accidents", title = "Total Number of Crashes By Month") +
      theme_gdocs() 
    ggplotly(p, tooltip = "text")
  })


  output$plot_day <- renderPlotly({
    selected_month <- input$month
    selected_month_data <- filtered_unique() %>% filter(Month.of.Crash == selected_month) %>%
      mutate(month_day = substr(Date.of.Crash, 4, 5)) %>%
      group_by(month_day) %>%
      summarise(Number.of.Accidents = n())

    # Generate a new plot for the selected month's days

    p_day <- ggplot(selected_month_data,
                    aes(
                      x = factor(month_day),
                      y = Number.of.Accidents,
                      text = paste(
                        "Day:",
                        month_day,
                        "<br>Accidents:",
                        Number.of.Accidents
                      )
                    )) +
      geom_bar(stat = "identity", fill = "#457b9d") +
      labs(
        title = paste("Accidents Per Day for", selected_month),
        x = "Day",
        y = "Number of Accidents"
      ) +
      theme_gdocs() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p_day, tooltip = "text")
  })
  
  output$plot_time <- renderPlot({
    df <- unique_crashes() %>%
      filter(!is.na(Hour), !is.na(Day.of.Week)) %>%
      mutate(Hour = case_when(Hour == "0000 Hours" ~ "12 AM",
                              Hour == "0100 Hours" ~ "1 AM",
                              Hour == "0200 Hours" ~ "2 AM",
                              Hour == "0300 Hours" ~ "3 AM",
                              Hour == "0400 Hours" ~ "4 AM",
                              Hour == "0500 Hours" ~ "5 AM",
                              Hour == "0600 Hours" ~ "6 AM",
                              Hour == "0700 Hours" ~ "7 AM",
                              Hour == "0800 Hours" ~ "8 AM",
                              Hour == "0900 Hours" ~ "9 AM",
                              Hour == "1000 Hours" ~ "10 AM",
                              Hour == "1100 Hours" ~ "11 AM",
                              Hour == "1200 Hours" ~ "12 PM",
                              Hour == "1300 Hours" ~ "1 PM",
                              Hour == "1400 Hours" ~ "2 PM",
                              Hour == "1500 Hours" ~ "3 PM",
                              Hour == "1600 Hours" ~ "4 PM",
                              Hour == "1700 Hours" ~ "5 PM",
                              Hour == "1800 Hours" ~ "6 PM",
                              Hour == "1900 Hours" ~ "7 PM",
                              Hour == "2000 Hours" ~ "8 PM",
                              Hour == "2100 Hours" ~ "9 PM",
                              Hour == "2200 Hours" ~ "10 PM",
                              TRUE ~ "11 PM"),
             Hour = factor(Hour, levels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM",
                                            "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
                                            "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM",
                                            "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"))) %>%
      group_by(Hour, Day.of.Week) %>%
      summarise(Crashes = n(), .groups = "drop")
    
    # Ensure weekday is ordered (if it's a factor, great; if not, reorder it)
    
    p <- ggplot(df, aes(x = Hour, y = Day.of.Week, fill = Crashes)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red", name = "Crashes") +
      labs(title = "Crash Heatmap: Hour vs Day of Week",
           x = "Hour of Day",
           y = "Day of Week") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 9))
    
    p
  })
  
  
  output$plot_age <- renderPlotly({
    age_data <- filtered_unique() %>%
      filter(!is.na(DRIVERAGE) & !is.na(DRIVERGEN)) %>%
      group_by(DRIVERAGE, DRIVERGEN) %>%
      summarise(count = n(), .groups = "drop")

    p <- ggplot(age_data, aes(x = DRIVERAGE, y = count, fill = DRIVERGEN,
                              text = paste("Driver Age:", DRIVERAGE,
                                           "Driver Gender:", DRIVERGEN,
                                           "<br>Accidents:", count
                                           ))) +
      geom_col() +
      labs(
        x = "Driver Age",
        y = "Number of Accidents",
        title = "Number of Crashes by Driver Age and Gender",
        fill = "Driver Gender"
      ) +
      theme_gdocs() +
      scale_x_continuous(breaks = seq(10, 105, by = 5)) +
      scale_fill_manual(values = c("M" = "#3a86ff", "F" = "#ff006e")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })

  # output$plot_damage_sex <- renderPlotly({
  #   p <- filtered_unique() %>%
  #     filter(DRIVERGEN %in% c("M", "F")) %>%
  #     filter(Amount.of.Property.Damage > 0) %>%
  #     mutate(DRIVERGEN = factor(DRIVERGEN, levels = c("M", "F"), labels = c("Male", "Female"))) %>%
  #     ggplot(aes(x = DRIVERGEN, y = log(Amount.of.Property.Damage))) +
  #     geom_boxplot() +
  #     labs(
  #       x = "Sex",
  #       y = "Log of Property Damage",
  #       title = "Property Damage by Sex"
  #     ) +
  #     theme_gdocs()
  # 
  #   ggplotly(p)
  # })


  output$plot_years <- renderPlotly({
    p <- unique_crashes() %>%
      group_by(Year) %>%
      summarise(Number.of.Crashes = n()) %>%
      mutate(Selected = ifelse(
        Year >= min(input$year_range) & Year <= max(input$year_range),
        "Selected",
        "Not Selected"
      )) %>%
      ggplot(aes(x = Year, y = Number.of.Crashes, fill = Selected)) +
      geom_bar(stat = "identity") +
      geom_smooth(aes(color = Selected), method = "lm", se = FALSE, size = 1.2) +
      scale_color_manual(name = "Trendline", values = c("Selected" = "blue", "Not Selected" = "black")) +
      scale_fill_manual(values = c("Selected" = "red", "Not Selected" = "gray")) +
      labs(x = "Year", y = "Number of Crashes") +
      theme_gdocs()
    
    ggplotly(p)
  })

  output$plot_iowa <- renderPlotly({
    suppressWarnings({
    p <- ggplot(data = iowa_filtered(),
                mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(color = "black", fill = "white") +
      geom_polygon(data = filtered_counties(),
                   aes(fill = log10(`Number.of.Crashes`),
                       text = paste("County:", subregion,
                                    "<br>Accidents:", (`Number.of.Crashes`))),
                   color = "black") +
      theme_gdocs() +
      scale_fill_gradientn(colors = c("blue", "lightblue", "white", "pink", "red")) +
      theme(
        axis.title = element_blank(),     # remove axis titles
        axis.text = element_blank(),      # remove axis text
        axis.ticks = element_blank(),     # remove axis ticks
        axis.line = element_blank()       # remove axis lines
      )
    ggplotly(p,tooltip = "text", source = "map")
    })
  })

  # Generate a new plot for the selected month's days

  output$plot_locations <- renderPlotly({



    iowa_map <- map_data("county") %>% filter(region == "iowa") %>% filter(subregion == tolower(input$county))
    selected_county_data <- filtered_unique() %>% mutate(County.Name = tolower(County.Name)) %>% filter(County.Name == tolower(input$county)) %>%
      mutate(
        long = as.numeric(str_extract(Location, "(?<=\\().+?(?=\\s)")),
        lat = as.numeric(str_extract(Location, "(?<=\\s)-?\\d+\\.\\d+(?=\\))"))
      )
    crash_sf <- st_as_sf(selected_county_data, coords = c("long", "lat"), crs = 4326)

    # Step 2: Ensure counties are in the same CRS
    iowa_sf <- st_as_sf(iowa_map, coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
      group_by(group) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
    # Step 3: Check which crashes fall inside Iowa counties
    in_iowa <- st_within(crash_sf, iowa_sf, sparse = FALSE)

    # Step 4: Filter to just those inside a county polygon
    crash_inside_iowa <- selected_county_data[rowSums(in_iowa) > 0, ]

    p <- ggplot(crash_inside_iowa, aes(x = long, y = lat)) +
      geom_polygon(data = iowa_map,  # this should be the polygon for the selected county
                   aes(x = long, y = lat, group = group),
                   fill = NA, color = "black", linewidth = 1) +
      geom_point(color = "red", alpha = 0.5) +
      coord_fixed() +
      labs(title = "Crash Locations in Selected County") +
      theme_minimal()

    ggplotly(p)
  })

  output$plot_speed <- renderPlot({
    data <- filtered_unique() %>%
      filter(!is.na(SPEEDLIMIT), !is.na(Crash.Severity)) %>%
      mutate(
        Crash.Severity.Numeric = case_when(
          Crash.Severity == "Fatal" ~ 5,
          Crash.Severity == "Major Injury" ~ 4,
          Crash.Severity == "Minor Injury" ~ 3,
          Crash.Severity == "Possible Injury" ~ 2,
          Crash.Severity == "Property Damage Only" ~ 1,
          TRUE ~ NA_real_
        )) %>%
      group_by(SPEEDLIMIT) %>%
      summarise(
        crashes = n(),
        avg_severity = mean(Crash.Severity.Numeric, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(data, aes(x = SPEEDLIMIT, y = crashes,
                          size = avg_severity, color = avg_severity,
                          text = paste("Speed Limit:", SPEEDLIMIT,
                                       "<br>Crashes:", crashes,
                                       "<br>Avg Severity:", round(avg_severity, 2)))) +
      geom_point(alpha = 0.8) +
      scale_size(range = c(5, 15)) +
      scale_color_gradientn(colors = c("blue", "red")) +
      labs(x = "Speed Limit (mph)",
           y = "Number of Crashes",
           title = "Crashes and Severity by Speed Limit") +
      theme_minimal()
    
    p
  })
  
  output$plot_gender <- renderPlotly({
    gender_summary <- Unique_Crashes %>%
      filter(!is.na(Crash.Severity), !is.na(DRIVERGEN)) %>%
      mutate(
        Crash.Severity.Numeric = case_when(
          Crash.Severity == "Fatal" ~ 5,
          Crash.Severity == "Major Injury" ~ 4,
          Crash.Severity == "Minor Injury" ~ 3,
          Crash.Severity == "Possible Injury" ~ 2,
          Crash.Severity == "Property Damage Only" ~ 1,
          TRUE ~ NA_real_
        ),
        DRIVERGEN = factor(DRIVERGEN)) %>%
      group_by(DRIVERGEN) %>%
      summarise(
        num_crashes = n(),
        avg_severity = mean(Crash.Severity.Numeric, na.rm = TRUE)
      )
    
    # Create the plot
    p <- ggplot(gender_summary, aes(x = num_crashes, y = avg_severity, color = DRIVERGEN, text = paste("Gender: ", DRIVERGEN,
                                                                                                       "<br>Crashes:", num_crashes,
                                                                                                       "<br>Avg Severity:", round(avg_severity, 2)))) +
      geom_point(size = 6) +
      labs(
        x = "Number of Crashes",
        y = "Average Crash Severity",
        title = "Number of Crashes and Crash Severity by Gender",
        color = "Driver Gender"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("M" = "#3a86ff", "F" = "#ff006e"))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$vbox_drivermale <- renderValueBox({
    valueBox(
      value = comma(nrow(
        filtered_unique() %>% filter(DRIVERGEN == "M")
      )),
      subtitle = "Total Male Drivers",
      icon = icon("car-crash"),
      color = "navy",
      width = 6
    )
  })
  output$vbox_driverfemale <- renderValueBox({
    valueBox(
      value = comma(nrow(
        filtered_unique() %>% filter(DRIVERGEN == "F")
      )),
      subtitle = "Total Female Drivers",
      color = "fuchsia",
      icon = icon("car-crash"),
      width = 6
    )
  })
  
  output$vbox_crashes <- renderValueBox({
    valueBox(
      value = comma(nrow(filtered_unique())),
      subtitle = "Total Crashes",
      icon = icon("car-crash"),
      width = 3
    )
  })
  
  output$vbox_vehicles <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Number.of.Vehicles.Involved
      )),
      subtitle = "Total Vehicles Involved",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_propertydamage <- renderValueBox({
    valueBox(
      value = dollar(sum(
        filtered_unique()$Amount.of.Property.Damage
      )),
      subtitle = "Total Propery Damage",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_people <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Total.Number.of.Occupants
      )),
      subtitle = "Total People Involved",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_fatalities <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Number.of.Fatalities
      )),
      subtitle = "Total Fatalities",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_injuries <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Number.of.Injuries
      )),
      subtitle = "Total Injuries",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_majorinjuries <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Number.of.Major.Injuries
      )),
      subtitle = "Total Major Injuries",
      icon = icon("car-crash"),
      width = 3
    )
  })
  output$vbox_minorinjuries <- renderValueBox({
    valueBox(
      value = comma(sum(
        filtered_unique()$Number.of.Minor.Injuries
      )),
      subtitle = "Total Minor Injuries",
      icon = icon("car-crash"),
      width = 3
    )
  })
  
  load("Property_Damage_Model.RData") # Assuming the new model is in this file
  property_damage <- model_property
  
  observeEvent(input$predict_btn, {
    print("Button clicked")
    newdata <- data.frame(
      Month.of.Crash = input$month_slider,
      Day.of.Week = input$day_slider,
      Hour = input$hour_slider,
      Drug.or.Alcohol = input$drugoralcohol,
      Environmental.Conditions = input$environmentalconditions,
      Light.Conditions = input$lightconditions,
      Weather.Conditions = input$weatherconditions,
      Surface.Conditions = input$surface_condition,
      Work.Zone = input$work_zone,
      DRIVERGEN = input$drivergen,
      DRIVERAGE = input$driverage,
      SPEEDLIMIT = as.factor(input$speedlimit),
      DRUGRESULT = as.factor(input$drugresult),
      ALCRESULT = input$alcresult
    )
    
    # Make prediction using the new model
    new_prediction <- predict(property_damage, newdata, type = "response")
    
    # Display the predicted property damage amount
    output$new_model_prediction <- renderText({
      paste("Predicted Property Damage Amount: $", round(exp(new_prediction) -
                                                           1, 2))
    })
  })
}

shinyApp(ui = ui, server = server)
