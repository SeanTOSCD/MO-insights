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
    SafetyDevice = SafetyDevice == "YES",
    
    # Made a new column with the Season of the crash based on date
    Season = case_when(
      format(Date, "%m") %in% c("12", "01", "02") ~ "Winter",
      format(Date, "%m") %in% c("03", "04", "05") ~ "Spring",
      format(Date, "%m") %in% c("06", "07", "08") ~ "Summer",
      format(Date, "%m") %in% c("09", "10", "11") ~ "Fall"
    )
  )


# === Just to look at the data!
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
    
    # Controls panel
    column(3, wellPanel(
             
             # Data table controls (default)
             selectInput("region", "Troop Selection:",
                         choices = c("All", "A", "C", "F")),
             dateRangeInput("date_range", "Date Range:",
                            start = "2023-11-01", end = "2024-12-31"),
             
             # Plot 1 controls
             conditionalPanel(
               condition = "input.tabset_plots == 'tab_1'",
               selectInput("injury_type", "Select Injury Type:",
                           choices = c("All", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
             ),
             
             # Plot 2 controls
             conditionalPanel(
               condition = "input.tabset_plots == 'tab_2'",
               checkboxGroupInput("injury_filter_plot2", "Select Injury Types:", 
                                  choices = c("MINOR", "MODERATE", "SERIOUS", "FATAL", "NO INJURY"),
                                  selected = c("MINOR", "MODERATE", "SERIOUS", "FATAL", "NO INJURY")),
               radioButtons("metric_plot2", "Display:", 
                            choices = c("Count" = "count", "Percentage" = "percent"), 
                            selected = "count")
             )
           )
    ),
    
    # Output panel
    column(9, tabsetPanel(id = "tabset_plots",
                          
                       # Default Tab: Data table
                       tabPanel("Data Table",
                                value = "data_table",
                                DTOutput("crash_data_table")
                       ),
                       tabPanel("Plot 1", 
                                value = "tab_1",
                                h3("Bar Plot of Age Distribution by Injury Type"),
                                p("Distribution of ages in 10-year bins, broken down by injury type."),
                                plotOutput("plot1")
                       ),
                       tabPanel("Plot 2", 
                                value = "tab_2",
                                h3("Crashes by Season"),
                                p("This plot shows the number of crashes by season, broken down by injury type."),
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
  
  # Main data table filtering by Troop or date range
  reactive_crash_data <- reactive({
    filtered_data <- crash_data
    
    if (input$region != "All") {
      filtered_data <- filtered_data %>% filter(Troop == input$region)
    }
    
    filtered_data <- filtered_data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    return(filtered_data)
  })
  
  # Main data table output
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
  
  
  #####
  # === Custom plots!
  #####s
  
  # Reactive data for Plot 1
  reactive_plot1_data <- reactive({
    data <- reactive_crash_data()
    if (input$injury_type != "All") {
      data <- data %>% filter(Injury == input$injury_type)
    }
    
    # Bin ages into groups
    data <- data %>%
      mutate(AgeGroup = cut(Age, breaks = seq(0, 100, by = 10), right = FALSE, 
                            labels = paste(seq(0, 90, by = 10), seq(10, 100, by = 10) - 1, sep = "-")))
    return(data)
  })
  
  # Plot 1 output
  output$plot1 <- renderPlot({
    reactive_plot1_data() %>%
      ggplot(aes(x = AgeGroup, fill = Injury)) +
      geom_bar(position = "dodge") +
      facet_wrap(~Injury, scales = "free_y") +
      labs(
        title = "Age Distribution by Injury Type",
        x = "Age Group",
        y = "Count",
        fill = "Injury Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Reactive data for Plot 2
  reactive_plot2_data <- reactive({
    data <- reactive_crash_data()  # Use globally filtered data
    
    # Filter by selected injury types
    if (!is.null(input$injury_filter_plot2)) {
      data <- data %>% filter(Injury %in% input$injury_filter_plot2)
    }
    
    # Group data by Season and Injury
    data <- data %>%
      group_by(Season, Injury) %>%
      summarise(crash_count = n(), .groups = "drop") %>%
      mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))  # Order seasons
    
    # Adjust for percentage if selected
    if (input$metric_plot2 == "percent") {
      data <- data %>%
        group_by(Season) %>%
        mutate(crash_count = crash_count / sum(crash_count) * 100)
    }
    
    return(data)
  })
  
  # Plot 2 output
  output$plot2 <- renderPlot({
    reactive_plot2_data() %>%
      ggplot(aes(x = Season, y = crash_count, fill = Injury)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Crashes by Season and Injury Type",
        x = "Season",
        y = "Number of Crashes",
        fill = "Injury Type"
      ) +
      theme_minimal()
  })
}


#####
# === Run the App!
#####
shinyApp(ui = ui, server = server)