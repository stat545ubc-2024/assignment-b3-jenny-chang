library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)

# Read in the activities data
activities <- read.csv("activities.csv", stringsAsFactors = FALSE)

# Convert Start column to POSIXct format, ensuring it's in the correct timezone
activities$Start <- as.POSIXct(activities$Start, format = "%H:%M", tz = "America/Vancouver")

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

###### 1. Select Input: Allows the user to choose a single type of activity to filter the data. ######
      selectInput("typeInput", "Activity",
                  choices = c("All", "Babytime", "Storytime", "Family Playtime", "Open gym", "Gymnastics", "Mama papa goose", "Music"),
                  selected = "All"),
###### 2. Checkbox Group Input: Allows the user to select multiple days of the week to filter the data. ######
      checkboxGroupInput("dayInput", "Day of the week",
                         choices = day_order),

###### 3. Slider Input: Allows the user to filter activities by start time using a range slider. ######
      sliderInput("timeInput", "Start time", 
                  min = as.POSIXct("09:00", format = "%H:%M", tz = "America/Vancouver"), 
                  max = as.POSIXct("16:00", format = "%H:%M", tz = "America/Vancouver"), 
                  value = c(as.POSIXct("09:00", format = "%H:%M", tz = "America/Vancouver"), 
                            as.POSIXct("16:00", format = "%H:%M", tz = "America/Vancouver")), 
                  timeFormat = "%H:%M", step = 1800),

###### 4. Text input: Allows the user search for activities/locations via keyword search. ######
      textInput("searchInput", "Search by keyword", value = ""),  

###### 5. Action button: Allows the user to reset the inputs. ######
      actionButton("reset", "Reset", class = "btn-primary")  
    ),


    mainPanel(
      
###### 6. Download button: Allows the user to download the data as a .csv file. ######
      downloadButton("downloadData", "Download .csv"),  # Download button above the table

      br(), br(),  
      p("Please note that the drop-in schedule is subject to change. For the most up-to-date information on times and dates, please consult the official website."),  # Added message
      br(),
      tableOutput("results"),

###### 7. fluidRow: Counts the number of rows in the table. ######
      fluidRow(
        column(6),  # Leave space on the left side
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
        else TRUE  # Search across multiple columns 
      ) %>%
      mutate(Start = format(Start, "%H:%M"),
             Prebooking = ifelse(Prebooking == 1, "Yes", "No")
      ) %>%
      arrange(Day, Start)  # Sort by Day and Start time
    
    return(filtered)
  })
  
  # Render the filtered data in the table
  output$results <- renderTable({
    filtered_data()
  })
  
  # Show the number of results found
  output$resultCount <- renderText({
    n <- nrow(filtered_data())
    paste("Number of results found:", n)
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
  
  # Reset button
  observeEvent(input$reset, {
    updateSelectInput(session, "typeInput", selected = "All")
    updateCheckboxGroupInput(session, "dayInput", selected = day_order)
    updateSliderInput(session, "timeInput", value = c(as.POSIXct("09:00", format = "%H:%M", tz = "America/Vancouver"), 
                                                      as.POSIXct("16:00", format = "%H:%M", tz = "America/Vancouver")))
    updateTextInput(session, "searchInput", value = "")
  })
}

shinyApp(ui = ui, server = server)
