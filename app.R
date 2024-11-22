library(shiny)
library(tidyverse)
#install.packages("DT")
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
    # Made Age a number
    Age = as.numeric(Age),
    # Formatted dates
    Date = as.Date(Date),
    # Formatted time
    Time = format(strptime(Time, format = "%I:%M%p"), "%H:%M:%S"),
    # Made a new column with Date and Time combined (so a full timestamp)
    DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"),
    # Remove extra space from city & state
    CityState = str_trim(CityState),
    # Converted injury values to uppercase
    Injury = toupper(Injury),
    # Set the values to TRUE or FALSE
    SafetyDevice = SafetyDevice == "YES"
  )


# === Just was to look at the data!
# glimpse(crash_data)
# head(crash_data)
# summary(crash_data)
# unique(crash_data$Injury)
# table(crash_data$County)
# crash_data_70_plus <- crash_data %>% filter(Age >= 70)
# rm(crash_data_70_plus)


#####
# === The User Interface (UI)!
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
             # Plot 2
             conditionalPanel(
               condition = "input.tabset_plots == 'tab_2'",
               selectInput("injury_type", "Choices Again:",
                           choices = c("All", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
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
                                p("This is a paragraph!"),
                                plotOutput("plot1")
                       ),
                       tabPanel("Plot 2", 
                                value = "tab_2",
                                h3("Plot 2!"),
                                p("NICE."),
                                plotOutput("plot2")
                       ),
                       tabPanel("Plot 3", 
                                value = "tab_3",
                                h3("Plot 3!")
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
  
  # Plot for Tab 2
  output$plot2 <- renderPlot({
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