library(shiny)      
library(dplyr)   
library(DT)          
library(lubridate)  
library(leaflet)    
library(scales)   

activities <- read.csv("activities.csv", stringsAsFactors = FALSE)

# Define the order of days for the Day filter
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Function to format time
format_time <- function(x, parse = FALSE, tz = "America/Vancouver") {
  if (parse) {
    as.POSIXct(x, format = "%H:%M", tz = tz) 
  } else {
    format(x, "%H:%M") 
  }
}

# Preprocess the activities data
activities <- activities %>%
  mutate(
    Start = format_time(Start, parse = TRUE),  
    End = format_time(End, parse = TRUE),      
    Day = factor(Day, levels = day_order),  
    Price = dollar(Price)                   
  ) %>%
  rename(
    'Prebooking required' = Prebooking,        
    'Activity type' = Activity
  ) %>%
  arrange(Day, Start)                      


ui <- fluidPage(
  titlePanel("Baby Drop-in Activities in West Side Vancouver"),  
  
  sidebarLayout(
    sidebarPanel(
      # Image in the sidebar
      img(src = "park.png", height = "200px", style = "display: block; margin-left: auto; margin-right: auto;"),
      br(), br(), br(), 
      # Dropdown to filter by activity type
      selectInput("typeInput", "Activity Type",  
                  choices = c("All", "Babytime", "Storytime", "Family playtime", "Open gym", "Gymnastics", "Mama papa goose", "Music"),
                  selected = "All"),
      # Checkbox to filter by day
      checkboxGroupInput("dayInput", "Day of the Week", choices = day_order), 
      # Time range slider
      sliderInput("timeInput", "Activity Start Time",  
                  min = format_time("09:00", parse = TRUE), 
                  max = format_time("16:00", parse = TRUE), 
                  value = c(format_time("09:00", parse = TRUE), format_time("16:00", parse = TRUE)), 
                  timeFormat = "%H:%M", step = 1800), 
      # Button to download filtered data as CSV
      downloadButton("downloadData", "Download .csv")  
    ),
    
    mainPanel(
      p(h2("Welcome!")),
      br(),
      p("This is a list of free or low-cost classes and gyms that you can go to with your baby in West Side Vancouver. Options range from community centre drop-in gyms, non-profits, like neighbourhood houses and family places, and other 'free play' spaces."),
      p("You can also view the options in either a table or map format. From the left navigation panel you'll be able to filter by activity type, day of the week, and start time. "),
      p(em("Please note: The drop-in schedule is subject to change. For the most up-to-date information on times and dates, please consult the official website.")),
      br(),
      # NEW FEATURE: Tabset panel to toggle between views
      tabsetPanel(  
        id = "viewType",
        tabPanel("Table View", DTOutput("results")), 
        tabPanel("Map View", leafletOutput("activityMap", height = "500px")) 
      ),
      textOutput("resultCount") 
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive function to parse the selected time input (start and end time)
  reactive_time_input <- reactive({
    list(
      start = format_time(input$timeInput[1], parse = TRUE),
      end = format_time(input$timeInput[2], parse = TRUE)
    )
  })
  
  # Reactive filtered data based on user input 
  filtered_data <- reactive({
    time_bounds <- reactive_time_input()  # Get the selected time bounds
    
    activities %>%
      filter(
        if (input$typeInput != "All") `Activity type` == input$typeInput else TRUE,
        if (length(input$dayInput) > 0) Day %in% input$dayInput else TRUE,  
        Start >= time_bounds$start,  
        Start <= time_bounds$end    
      ) %>%
      mutate(
        StartFormatted = format_time(Start),  
        EndFormatted = format_time(End)       
      ) %>%
      arrange(Day, Start)  
  })
  
  # NEW FEATURE: Render the activity data in a table format
  output$results <- renderDT({
    data <- filtered_data() %>%
      select(`Activity type`, Day, StartFormatted, EndFormatted, Location, Age, Price, `Prebooking required`) %>% 
      rename(
        Start = StartFormatted,
        End = EndFormatted
      ) 
    
    if (nrow(data) == 0) {
      datatable(data.frame(Message = "No activities match your criteria."), options = list(pageLength = 10), rownames = FALSE)
    } else {
      datatable(data, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
    }
  })
  
  # NEW FEATURE: Render the activity data on a map
  output$activityMap <- renderLeaflet({
    data <- filtered_data() 
    
    leaflet(data = data) %>%
      addProviderTiles("CartoDB.Positron") %>%  
      setView(lng = -123.17220, lat = 49.25983, zoom = 12) %>%  
      addMarkers(
        ~Longitude, ~Latitude, 
        popup = ~paste(
          "<strong>Activity type:</strong>", `Activity type`, "<br>",
          "<strong>Location:</strong>", Location, "<br>",
          "<strong>Day:</strong>", Day, "<br>",
          "<strong>Time:</strong>", StartFormatted, "-", EndFormatted, "<br>",
          "<strong>Price:</strong>", Price, "<br>",
          "<strong>Prebooking required:</strong>", `Prebooking required`
        ),
        clusterOptions = markerClusterOptions()  
      )
  })
  
  # Allow users to download the filtered data as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_activities_", Sys.Date(), ".csv", sep = "")  
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)  
    }
  )
}


shinyApp(ui = ui, server = server)
