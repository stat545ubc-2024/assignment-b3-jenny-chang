library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# Read in the activities data
activities <- read.csv("activities.csv", stringsAsFactors = FALSE)

# Convert Start column to POSIXct format
activities$Start <- as.POSIXct(activities$Start, format = "%H:%M")

# Define the custom order for days of the week
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
activities$Day <- factor(activities$Day, levels = day_order)

# Sort the initial activities data by Day and Start time
activities <- activities %>%
  arrange(Day, Start)

ui <- fluidPage(
  titlePanel("Baby Drop-in Activities in West Side Vancouver"),
  sidebarLayout(
    sidebarPanel(
###### FEATURE 1. Select Input: Allows the user to choose a single type of activity to filter the data. ######
      selectInput("typeInput", "Activity",
                  choices = c("All", "Babytime", "Storytime", "Family playtime", "Open gym", "Gymnastics", "Mama papa goose", "Music"),
                  selected = "All"),
      
# Checkbox Group Input: Allows the user to select multiple days of the week to filter the data. 
      checkboxGroupInput("dayInput", "Day of the week",
                         choices = day_order),
      
# Slider Input: Allows the user to filter activities by start time using a range slider. 
      sliderInput("timeInput", "Start time", 
                  min = as.POSIXct("09:00", format = "%H:%M"), 
                  max = as.POSIXct("16:00", format = "%H:%M"), 
                  value = c(as.POSIXct("09:00", format = "%H:%M"), 
                            as.POSIXct("16:00", format = "%H:%M")), 
                  timeFormat = "%H:%M",
                  step = 1800),
      
# 4. Text input: Allows the user search for activities/locations via keyword search.
      textInput("searchInput", "Search by keyword", value = ""),  

# 5. Action button: Allows the user to reset the inputs.
      actionButton("reset", "Reset", class = "btn-primary") 
    ),
    mainPanel(
###### 6. Download button: Allows the user to download the data as a .csv file. ######
      downloadButton("downloadData", "Download .csv"),  

      br(), br(),  
      p("Please note that the drop-in schedule is subject to change. For the most up-to-date information on times and dates, please consult the official website."),  # Added message
      br(),
      tableOutput("results"),
      fluidRow(
        column(6),  
        column(6, align = "right", textOutput("resultCount")) 
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to store filtered data
  filtered_data <- reactive({
    filtered <- activities %>%
      filter(
        if (input$typeInput != "All") Activity == input$typeInput else TRUE,  # Filter by activity
        if (length(input$dayInput) > 0) Day %in% input$dayInput else TRUE,  # Filter by selected days
        Start >= input$timeInput[1],  # Filter by start time
        Start <= input$timeInput[2],  # Filter by end time
        if (input$searchInput != "") 
          (str_detect(Activity, regex(input$searchInput, ignore_case = TRUE)) |
             str_detect(Location, regex(input$searchInput, ignore_case = TRUE)) |
             str_detect(Day, regex(input$searchInput, ignore_case = TRUE)) |
             str_detect(Prebooking, regex(input$searchInput, ignore_case = TRUE))) 
        else TRUE  
      ) %>%
      mutate(Start = format(Start, "%H:%M"),
             Prebooking = ifelse(Prebooking == 1, "Yes", "No")
      ) %>%
      arrange(Day, Start)  
    
    return(filtered)
  })
  
  output$results <- renderTable({
    filtered_data()
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "typeInput", selected = "All")
    updateCheckboxGroupInput(session, "dayInput", selected = day_order)
    updateSliderInput(session, "timeInput", value = c(as.POSIXct("09:00", format = "%H:%M"), as.POSIXct("16:00", format = "%H:%M")))
    updateTextInput(session, "searchInput", value = "")
  })
  
  # Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_activities_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Show the number of results found
  output$resultCount <- renderText({
    n <- nrow(filtered_data())
    paste("Number of results found:", n)
  })

}

shinyApp(ui = ui, server = server)
