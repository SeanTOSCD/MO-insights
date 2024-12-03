library(shiny)
library(tidyverse)
library(DT)

#####
# === Collect and prepare data
# Stacking CSV files. The first sets column standards, the rest follow.
#####

# Step 1 of 2: We're setting all columns to be read as characters by default
crash_data <- bind_rows(
  read_csv("./troop_a.csv",
           col_types = cols(
             PersonType = col_character(),
             Age = col_character(),
             CityState = col_character(),
             Injury = col_character(),
             SafetyDevice = col_character(),
             Date = col_character(),
             Time = col_character(),
             County = col_character(),
             Location = col_character(),
             Troop = col_character()
           ),
           show_col_types = FALSE
  ),
  read_csv("./troop_c.csv",
           col_types = cols(.default = "c"),
           show_col_types = FALSE),
  read_csv("./troop_f.csv",
           col_types = cols(.default = "c"),
           show_col_types = FALSE)
)

# Step 2 of 2: Now convert certain columns to other data types with special considerations
crash_data <- crash_data %>%
  mutate(
    Age = as.numeric(Age),  # Make Age a number
    Date = as.Date(Date),  # Format dates
    Time = format(strptime(Time, format = "%I:%M%p"), "%H:%M:%S"),  # Format time
    DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"),  # Combine Date and Time
    Hour = as.integer(format(DateTime, "%H")),  # Extract hour from DateTime
    HourLabel = format(DateTime, "%I %p"),  # Create readable hour labels
    CityState = str_trim(CityState),  # Remove extra space from CityState
    Injury = toupper(Injury),  # Convert Injury values to uppercase
    SafetyDevice = SafetyDevice == "YES"  # Set SafetyDevice to TRUE or FALSE
  )

#####
# === The User Interface!
#####
ui <- fluidPage(
  titlePanel("Missouri Traffic Crash Analysis"),
  
  fluidRow(
    column(3, wellPanel(
      selectInput("region", "Select Region/Troop:",
                  choices = c("All", "A", "C", "F")),
      dateRangeInput("date_range", "Date Range:",
                     start = "2023-11-01", end = "2024-12-31"),
      # Plot 1
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_1'",
        selectInput("injury_type", "Select Injury Type:",
                    choices = c("All", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
      # Safety Device selection for Plot 2
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_2'",
        selectInput("safety_device", "Select Safety Device:",
                    choices = c("All", "TRUE", "FALSE"))
      ),
      # Injury Severity selection for Plot 3
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_3'",
        selectInput("injury_severity", "Select Injury Severity:",
                    choices = c("All", "NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      )
    )
    ),
    column(9, tabsetPanel(id = "tabset_plots",
                          # Default Tab: Data Table
                          tabPanel("Data Table",
                                   value = "data_table",
                                   DTOutput("crash_data_table")
                          ),
                          tabPanel("Plot 1", 
                                   value = "tab_1",
                                   h3("Bar Plot of Age Distribution by Injury Type"),
                                   plotOutput("plot1")
                          ),
                          tabPanel("Plot 2", 
                                   value = "tab_2",
                                   h3("Time of Day vs. Safety Device"),
                                   plotOutput("plot2")
                          ),
                          tabPanel("Plot 3", 
                                   value = "tab_3",
                                   h3("Time of Day vs. Injury Severity"),
                                   plotOutput("plot3")
                          ),
                          tabPanel("Plot 4", 
                                   value = "tab_4",
                                   h3("Plot 4!")
                          ),
                          tabPanel("Plot 5", 
                                   value = "tab_5",
                                   h3("Plot 5!")
                          ),
                          tabPanel("Plot 6", 
                                   value = "tab_6",
                                   h3("Plot 6!")
                          ),
                          tabPanel("Plot 7", 
                                   value = "tab_7",
                                   h3("Plot 7!")
                          ),
                          tabPanel("Plot 8", 
                                   value = "tab_8",
                                   h3("Plot 8!")
                          ),
                          tabPanel("Plot 9", 
                                   value = "tab_9",
                                   h3("Plot 9!")
                          ),
                          tabPanel("Plot 10", 
                                   value = "tab_10",
                                   h3("Plot 10!")
                          )
    )
    )
  )
)

#####
# === Server logic!
#####
server <- function(input, output, session) {
  
  # Reactive data filtering
  reactive_crash_data <- reactive({
    filtered_data <- crash_data
    
    # Filter by region (Troop)
    if (input$region != "All") {
      filtered_data <- filtered_data %>% filter(Troop == input$region)
    }
    
    # Filter by date range
    filtered_data <- filtered_data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    return(filtered_data)
  })
  
  #####
  # === Plots!!!
  #####
  
  # Reactive filtered data for Plot 1
  reactive_plot1_data <- reactive({
    data <- reactive_crash_data()
    if (input$injury_type != "All") {
      data <- data %>% filter(Injury == input$injury_type)
    }
    return(data)
  })
  
  # Plot for Tab 1
  output$plot1 <- renderPlot({
    reactive_plot1_data() %>%
      ggplot(aes(x = Age)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "Age Distribution",
        x = "Age",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # Reactive filtered data for Plot 2
  reactive_plot2_data <- reactive({
    data <- reactive_crash_data()
    if (input$safety_device != "All") {
      data <- data %>% filter(SafetyDevice == as.logical(input$safety_device))
    }
    data %>%
      group_by(Hour, HourLabel, SafetyDevice) %>%
      summarise(Count = n(), .groups = 'drop')
  })
  
  # Plot for Tab 2
  output$plot2 <- renderPlot({
    reactive_plot2_data() %>%
      ggplot(aes(x = HourLabel, y = Count, fill = SafetyDevice)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Time of Day vs. Safety Device",
        x = "Time of Day",
        y = "Count",
        fill = "Safety Device"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # Reactive filtered data for Plot 3
  reactive_plot3_data <- reactive({
    data <- reactive_crash_data()
    if (input$injury_severity != "All") {
      data <- data %>% filter(Injury == input$injury_severity)
    }
    data %>%
      group_by(Hour, HourLabel, Injury) %>%
      summarise(Count = n(), .groups = 'drop')
  })
  
  # Plot for Tab 3
  output$plot3 <- renderPlot({
    reactive_plot3_data() %>%
      ggplot(aes(x = HourLabel, y = Count, fill = Injury)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Time of Day vs. Injury Severity",
        x = "Time of Day",
        y = "Count",
        fill = "Injury Severity"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  # The main data table
  output$crash_data_table <- renderDT({
    datatable(
      reactive_crash_data(),
      options = list(
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'font-size': '12px'});",
          "}"
        )
      )
    )
  })
}

#####
# === Run the App!
#####
shinyApp(ui = ui, server = server)