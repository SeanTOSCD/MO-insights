library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)

#### ========== Data Preparation ==========

# Step 1 of 2:
# We're setting all columns to be read as characters by default.
# The first file sets column standards and the rest follow beneath.
crash_data <- bind_rows(
  read_csv(
    "./troop_a.csv",
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
  read_csv(
    "./troop_c.csv",
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ),
  read_csv(
    "./troop_f.csv",
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  )
)

# Step 2 of 2:
# Now convert specific columns to other data types as needed.
# Also create new columns from existing data.
crash_data <- crash_data %>%
  mutate(
    
    # Made Age a number
    Age = as.numeric(Age),
    
    # Formatted dates
    Date = as.Date(Date),
    
    # Formatted time
    Time = format(strptime(Time, format = "%I:%M%p"), "%H:%M:%S"),
    
    # NEW COLUMN: Made a new column with Date and Time combined (so a full timestamp)
    DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"),
    
    # Extract hour from DateTime
    Hour = as.integer(format(DateTime, "%H")),
    
    # Create readable hour labels
    HourLabel = format(DateTime, "%I %p"),
    
    # Remove extra space from city & state
    CityState = str_trim(CityState),
    
    # Combine "ST. LOUIS CITY" into "ST. LOUIS"
    County = ifelse(County == "ST. LOUIS CITY", "ST. LOUIS", County),
    
    # Set order for injury severity
    Injury = factor(
      Injury,
      levels = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL")
    ),
    
    # Set the values to TRUE or FALSE
    SafetyDevice = SafetyDevice == "YES",
    
    # NEW COLUMN: Made a new column with the Season of the crash based on date
    Season = case_when(
      format(Date, "%m") %in% c("12", "01", "02") ~ "Winter",
      format(Date, "%m") %in% c("03", "04", "05") ~ "Spring",
      format(Date, "%m") %in% c("06", "07", "08") ~ "Summer",
      format(Date, "%m") %in% c("09", "10", "11") ~ "Fall"
    )
  )

#### ========== UI Components ==========

ui <- fluidPage(

  theme = shinytheme("cosmo"),
  
  tags$head(
    tags$style(HTML("
      .title-panel {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 20px;
      }
      h1, h2, h3, h4, h5, h6 {
        font-weight: 600;
      }
      .nav-pills > li > a {
        padding: 5px 10px;
        font-size: 14px;
        margin-right: 5px;
      }
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover,
      .nav-pills > li.active > a:focus {
        background-color: #337ab7;
        color: white;
      }
    "))
  ),
  
  titlePanel(
    title = div("Missouri Traffic Crash Analysis", class = "title-panel")
  ),

  sidebarLayout(

    sidebarPanel(
      
      tags$div(
        style = "margin-bottom: 0;",
        tags$strong("Application Instructions: "),
        tags$p("Navigate tabs for different data visualizations and data-specific controls.
               All visualizations allow Troop selection and date range.")
      ),

      tags$img(
        src = "missouri_county_map_w_troops.png",
        alt = "Missouri Troop Areas",
        style = "max-width: 100%; height: 170px; margin-top: 3px; border: 1px solid rgba(0,0,0,.1);"
      ),

      tags$hr(),
             
      # Default controls for ALL panels
      selectInput("region", "Select a Troop to View:",
          choices = c("All Troops" = "All", 
                      "Troop A (includes: Kansas City)" = "A", 
                      "Troop C (includes: St. Louis)" = "C", 
                      "Troop F (includes: Columbia, Jefferson City)" = "F")),
      
      dateRangeInput("date_range", "Date Range:",
                    start = "2023-11-01", end = "2024-12-05"),
             
      # Plot 1 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_1'",
        selectInput("injury_type_plot1", "Select Injury Type:",
                    choices = c("All", "NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
      
      # Plot 2 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_2'",
        checkboxGroupInput("injury_filter_plot2", "Select Injury Types:", 
                          choices = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"),
                          selected = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL")),
        radioButtons("metric_plot2", "Display:", 
                    choices = c("Count" = "count", "Percentage" = "percent"),
                    selected = "count")
      ),
      
      # Plot 3 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_3'",
        sliderInput("time_range_plot3", "Select Time Range (Hours):",
                    min = 0, max = 24, value = c(0, 24), step = 1)
      ),
      
      # Plot 4 controls (NONE)
      
      # Plot 5 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_5'",
        radioButtons("aggregation_level", "Select Time Interval:",
                    choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"),
                    selected = "week")
      ),
      
      # Plot 6 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_6'",
        sliderInput("top_n_counties", "Number of Counties to Display:",
                    min = 5, max = 20, value = 10, step = 1),
        checkboxGroupInput("county_injury_filter", "Select Injury Types:",
                           choices = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"),
                           selected = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
            
      # Plot 7 controls (NONE)

      # Plot 7_0 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_7_0'",
        selectInput("injury_type_plot7_0", "Select Injury Type:",
                    choices = c("All", "NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
      
      # Plot 8 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_8'",
        selectInput("safety_device", "Select Safety Device:",
                    choices = c("All", "TRUE", "FALSE"))
      ),
      
      # Plot 9 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_9'",
        selectInput("injury_severity", "Select Injury Severity:",
                    choices = c("All", "NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
      
      # Plot 10 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_10'",
        selectInput("injury_type_plot10", "Select Injury Type:",
                    choices = c("All", "NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
      
      # Plot 11 controls
      conditionalPanel(
        condition = "input.tabset_plots == 'tab_11'",
        selectInput("injury_type_plot11", "Select Injury Type:",
                    choices = c("NO INJURY", "MINOR", "MODERATE", "SERIOUS", "FATAL"))
      ),
    ),

    mainPanel(

      tabsetPanel(
        id = "tabset_plots",
        type = "pills",
                          
        # Default Tab: All Data
        tabPanel("All Data",
          value = "data_table",
          h3("All Crashes: Troops A, C, and F"),
          hr(),
          DTOutput("crash_data_table")
        ),
        
        # Plot 1 output
        tabPanel("Age Distribution by Injury", 
          value = "tab_1",
          h3("Age Distribution by Injury Type"),
          p("This chart visualizes the age distribution of individuals involved in traffic crashes, grouped by injury severity."),
          tags$ul(
            tags$li("Each bar represents a 10-year age group."),
            tags$li("Use the dropdown menu to filter by specific injury types."),
            tags$li("This visualization helps identify which age groups are most affected by different levels of crash severity.")
          ),
          hr(),
          plotOutput("plot1"),
        ),
        
        # Plot 2 output
        tabPanel("Seasonal Injury Types", 
          value = "tab_2",
          h3("Crashes by Season and Injury Type"),
          p("This chart shows the distribution of traffic crashes across seasons, broken down by injury severity."),
          tags$ul(
            tags$li("Each bar represents the count or percentage of crashes for a specific season."),
            tags$li("Use the checkboxes to filter by specific injury types."),
            tags$li("Switch between counts and percentages using the radio buttons to analyze proportions.")
          ),
          hr(),
          plotOutput("plot2"),
        ),

        # Plot 3 output
        tabPanel("Injuries by the Hour", 
          value = "tab_3",
          h3("Crashes by Time of Day"),
          p("This chart illustrates the distribution of traffic crashes throughout the day, grouped by injury severity."),
          tags$ul(
            tags$li("Each bar represents the number of crashes during a specific hour of the day."),
            tags$li("Use the slider to adjust the time range displayed."),
            tags$li("This visualization helps identify high-risk times of the day for different levels of crash severity.")
          ),
          hr(),
          plotOutput("plot3"),
        ),
        
        # Plot 4 output
        tabPanel("Crash Day/Hour Heatmap", 
          value = "tab_4",
          h3("Heatmap of Crashes by Day of Week and Hour"),
          p("This heatmap visualizes the distribution of traffic crashes by day of the week and time of day."),
          tags$ul(
            tags$li("Each cell represents the number of crashes for a specific hour and day of the week."),
            tags$li("The color intensity indicates the crash count, with darker colors representing higher counts."),
            tags$li("This visualization helps identify patterns in crash occurrences by time and day.")
          ),
          hr(),
          plotOutput("plot4"),
        ),
        
        # Plot 5 output
        tabPanel("Crashes Over Time", 
          value = "tab_5",
          h3("Traffic Crashes Over Time"),
          p("This line chart displays the trend of traffic crashes over time, based on the selected date range."),
          tags$ul(
            tags$li("Each point represents the number of crashes within the selected time interval."),
            tags$li("Use the radio buttons to select daily, weekly, or monthly aggregation."),
            tags$li("This visualization helps identify trends or seasonal patterns in crash occurrences.")
          ),
          hr(),
          plotOutput("plot5"),
        ),
        
        # Plot 6 output
        tabPanel("Crashes by County", 
          value = "tab_6",
          h3("Top Counties by Crash Count"),
          p("This bar chart highlights the counties with the highest number of crashes, grouped by injury severity."),
          tags$ul(
            tags$li("Each bar represents the crash count for a specific county and injury type."),
            tags$li("Use the slider to adjust the number of counties displayed."),
            tags$li("Filter the injury types displayed using the checkboxes to focus on specific crash severities.")
          ),
          hr(),
          plotOutput("plot6"),
        ),

        # Plot 7 output
        tabPanel("Crashes Month/Hour Bubble Chart", 
          value = "tab_7",
          h3("Crashes by Month and Hour"),
          p("This bubble chart shows the distribution of traffic crashes by month and hour of the day. Bubble size represents the number of crashes, and colors represent different troops."),
          tags$ul(
            tags$li("The x-axis shows the months of the year."),
            tags$li("The y-axis shows the hour of the day (0-23)."),
            tags$li("Bubble size indicates the crash count, making it easy to spot high-risk times."),
            tags$li("Use the troop selection filter to focus on specific regions.")
          ),
          hr(),
          plotOutput("plot7"),
        ),

        # Plot 7_0 output
        tabPanel("Injuries Month/Hour Bubble Chart", 
          value = "tab_7_0",
          h3("Injuries by Month and Hour"),
          p("This bubble chart illustrates the distribution of injuries from traffic crashes by month and hour. Bubble size represents the number of injuries, and colors represent injury severity."),
          tags$ul(
            tags$li("The x-axis represents the months of the year."),
            tags$li("The y-axis shows the hour of the day (0-23)."),
            tags$li("Bubble size represents the number of injuries."),
            tags$li("Colors distinguish between levels of injury severity.")
          ),
          hr(),
          plotOutput("plot7_0"),
        ), 
        
        # Plot 8 output
        tabPanel("Count by Time of Day",
          value = "tab_8",
          h3("Crash Counts by Time of Day"),        
          p("This bar chart shows the number of traffic crashes at different times of the day, grouped by safety device usage."),
          tags$ul(
            tags$li("The x-axis represents the time of day, grouped by hour."),
            tags$li("The y-axis shows the number of crashes."),
            tags$li("Colors differentiate between crashes involving safety device use and those without.")
          ),
          hr(),
          plotOutput("plot8"),
        ),
        
        # Plot 9 output
        tabPanel("Severity by Time of Day", 
          value = "tab_9",
          h3("Injury Severity by Time of Day"),
          p("This bar chart displays the number of traffic crashes at different times of the day, categorized by injury severity."),
          tags$ul(
            tags$li("The x-axis represents the time of day, grouped by hour."),
            tags$li("The y-axis shows the number of crashes."),
            tags$li("Colors represent different levels of injury severity.")
          ),
          hr(),
          plotOutput("plot9"),
        ),
        
        # Plot 10 output
        tabPanel("Safety Device Usage", 
          value = "tab_10",
          h3("Safety Device Usage by Injury Type"),
          p("This bar chart illustrates the usage of safety devices during crashes, categorized by injury severity."),
          tags$ul(
            tags$li("The x-axis differentiates between crashes with and without safety device usage."),
            tags$li("The y-axis shows the total number of crashes."),
            tags$li("Use the injury type filter to explore specific categories.")
          ),
          hr(),
          plotOutput("plot10"),
        ),
        
        # Plot 11 output
        tabPanel("Frequency of Age by Injury Severity", 
          value = "tab_11",
          h3("Frequency of Age by Injury Severity"),
          p("This bar chart shows the frequency of different ages involved in traffic crashes, categorized by injury severity."),
          tags$ul(
            tags$li("The x-axis represents the ages of individuals involved in crashes."),
            tags$li("The y-axis shows the frequency of crashes."),
            tags$li("Use the injury severity filter to analyze specific categories.")
          ),
          hr(),
          plotOutput("plot11"),
        ),
      )
    )
  )
) # End UI

#### ========== Server Logic ==========

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

  # ========== Begin custom plots
  
  # Reactive data for Plot 1
  reactive_plot1_data <- reactive({
    data <- reactive_crash_data()
    if (input$injury_type_plot1 != "All") {
      data <- data %>% filter(Injury == input$injury_type_plot1)
    }
    
    # Bin ages into groups
    data <- data %>%
      mutate(
        AgeGroup = cut(
          Age,
          breaks = seq(0, 100, by = 10),
          right = FALSE,
          labels = paste(seq(0, 90, by = 10), seq(10, 100, by = 10) - 1, sep = "-")
        )
      )
    
    return(data)
  })
  
  # Plot 1 output
  output$plot1 <- renderPlot({
    reactive_plot1_data() %>%
      ggplot(aes(x = AgeGroup, fill = Injury)) +
      geom_bar(position = "dodge", color = "white") +
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
    data <- reactive_crash_data()
    
    # Filter by selected injury types
    if (!is.null(input$injury_filter_plot2)) {
      data <- data %>% filter(Injury %in% input$injury_filter_plot2)
    }
    
    # Group data by Season and Injury
    data <- data %>%
      group_by(Season, Injury) %>%
      summarise(crash_count = n(), .groups = "drop") %>%
      mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))
    
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
    plot_data <- reactive_plot2_data()
    y_label <- ifelse(input$metric_plot2 == "count", "Number of Crashes", "Percentage of Crashes")
    plot_data %>%
      ggplot(aes(x = Season, y = crash_count, fill = Injury)) +
      geom_bar(stat = "identity", position = "dodge", color = "white") +
      labs(
        title = "Crashes by Season and Injury Type",
        x = "Season",
        y = y_label,
        fill = "Injury Type"
      ) +
      theme_minimal()
  })
  
  # Reactive data for plot 3
  reactive_plot3_data <- reactive({
    data <- reactive_crash_data()  # Use globally filtered data
    
    # Get the hour from DateTime
    data <- data %>%
      mutate(Hour = as.numeric(format(DateTime, "%H")))
    
    # Filter by the selected time range
    data <- data %>%
      filter(Hour >= input$time_range_plot3[1], Hour <= input$time_range_plot3[2])
    
    return(data)
  })
  
  # Plot 3 output
  output$plot3 <- renderPlot({
    reactive_plot3_data() %>%
      ggplot(aes(x = Hour, fill = Injury)) +
      geom_histogram(binwidth = 1, position = "dodge", color = "white") +
      labs(
        title = "Crashes by Hour of Day",
        x = "Hour of Day",
        y = "Number of Crashes",
        fill = "Injury Type"
      ) +
      scale_x_continuous(breaks = 0:24) +
      theme_minimal()
  })
  
  # Reactive data for plot 4
  reactive_plot4_data <- reactive({
    data <- reactive_crash_data()  # Use globally filtered data
    
    # Extract day of the week and hour
    data <- data %>%
      mutate(
        DayOfWeek = weekdays(Date, abbreviate = TRUE),
        Hour = as.numeric(format(DateTime, "%H"))
      ) %>%
      group_by(DayOfWeek, Hour) %>%
      summarise(crash_count = n(), .groups = "drop")
    
    return(data)
  })
  
  # Plot 4 output
  output$plot4 <- renderPlot({
    reactive_plot4_data() %>%
      ggplot(aes(x = Hour, y = fct_reorder(DayOfWeek, -match(DayOfWeek, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))), 
                 fill = crash_count)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red", name = "Crash Count") +
      labs(
        title = "Crashes by Day of Week and Hour of Day",
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )
  })
  
  # Reactive data for plot 5
  reactive_plot5_data <- reactive({
    data <- reactive_crash_data()
    
    aggregation_level <- input$aggregation_level
    
    data <- data %>%
      mutate(
        Period = case_when(
          aggregation_level == "day"   ~ as.Date(Date),
          aggregation_level == "week"  ~ as.Date(cut(Date, "week", start.on.monday = TRUE)),
          aggregation_level == "month" ~ as.Date(format(Date, "%Y-%m-01"))
        )
      )
    
    # Group data by Period and count crashes
    data <- data %>%
      group_by(Period) %>%
      summarise(crash_count = n(), .groups = "drop")
    
    return(data)
  })
  
  # Plot 5 output
  output$plot5 <- renderPlot({
    plot_data <- reactive_plot5_data()
    
    # Determine the x-axis label based on the aggregation level
    x_label <- switch(input$aggregation_level,
                      "day" = "Date",
                      "week" = "Week Starting",
                      "month" = "Month")
    
    # Determine date breaks and labels for better readability
    date_breaks <- switch(input$aggregation_level,
                          "day" = "1 week",
                          "week" = "1 month",
                          "month" = "1 month")
    
    date_labels <- switch(input$aggregation_level,
                          "day" = "%Y-%m-%d",
                          "week" = "%Y-%m-%d",
                          "month" = "%Y-%m")
    
    # Now plot the data
    ggplot(plot_data, aes(x = Period, y = crash_count)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "darkblue", size = 2) +
      labs(
        title = "Crashes Over Time",
        x = x_label,
        y = "Number of Crashes"
      ) +
      scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  # Reactive data for plot 6
  reactive_plot6_data <- reactive({
    data <- reactive_crash_data()
    
    # Filter by selected injury types
    if (!is.null(input$county_injury_filter)) {
      data <- data %>% filter(Injury %in% input$county_injury_filter)
    }
    
    # Create a unique County_Troop identifier
    data <- data %>%
      mutate(County_Troop = paste(County, Troop, sep = "_"))
    
    # Aggregate data by County_Troop for total crashes
    total_crashes_data <- data %>%
      group_by(County_Troop, County, Troop) %>%
      summarise(total_crashes = n(), .groups = "drop") %>%
      arrange(desc(total_crashes)) %>%
      slice_head(n = input$top_n_counties)
    
    # Filter original data for the selected counties
    filtered_data <- data %>%
      filter(County_Troop %in% total_crashes_data$County_Troop) %>%
      group_by(County_Troop, County, Troop, Injury) %>%
      summarise(crash_count = n(), .groups = "drop") %>%
      left_join(total_crashes_data, by = c("County_Troop", "County", "Troop"))
    
    # Update County labels to include Troop designation
    filtered_data <- filtered_data %>%
      mutate(County = paste(County, paste0("(", Troop, ")")))
    
    return(filtered_data)
  })
  
  # Plot 6 output
  output$plot6 <- renderPlot({
    reactive_plot6_data() %>%
      ggplot(aes(x = reorder(County, total_crashes), y = crash_count, fill = Injury)) +
      geom_bar(stat = "identity", color = "white") +
      coord_flip() +
      labs(
        title = "Top Counties by Crash Count (Stacked)",
        x = "County (Troop)",
        y = "Crash Count",
        fill = "Injury Type"
      ) +
      theme_minimal()
  })
  
  # Reactive data for plot 7
  reactive_plot7_data <- reactive({
    crash_data <- reactive_crash_data() 
    crash_data %>%
      group_by(Date, Time, Troop) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      mutate(
        Hour = as.numeric(format(strptime(Time, "%H:%M:%S"), "%H")),
        Month = format(Date, "%Y-%m")
      )
  })
  
  # Plot 7 output
  output$plot7 <- renderPlot({
    reactive_plot7_data() %>%
      ggplot(aes(x = Month, y = Hour, size = Count, color = Troop)) +
      geom_point(alpha = 0.6) +
      scale_size_continuous(name = "Count of Troop", range = c(3, 15)) +
      scale_y_continuous(
        breaks = seq(0, 24, by = 2),
        limits = c(0, 24),
        expand = c(0, 0)
      ) +
      labs(
        title = "Crash Data Bubble Chart (Count of Troop)",
        x = "Month",
        y = "Hour of the Day"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 1.5),
        axis.title.x = element_text(vjust = -0.5)
      )
  }, height = 650)

  # Reactive data for plot 7_0
  reactive_plot7_0_data <- reactive({
    crash_data <- reactive_crash_data() 
    
    # Ensure input$injury_type_plot7_0 exists and filter by Injury type
    if (input$injury_type_plot7_0 != "All") {
      crash_data <- crash_data %>% filter(Injury == input$injury_type_plot7_0)
    }
    
    # Process the data
    crash_data %>%
      group_by(Date, Time, Injury) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      mutate(
        Hour = as.numeric(format(strptime(Time, "%H:%M:%S"), "%H")),
        Month = format(Date, "%Y-%m")  # Formats to "YYYY-MM"
      )
  })
  
  # Plot 7_0 output
  output$plot7_0 <- renderPlot({
    # Access the reactive data
    plot_data <- reactive_plot7_0_data()
    
    # Generate the plot
    ggplot(plot_data, aes(x = Month, y = Hour, size = Count, color = Injury)) +
      geom_point(alpha = 0.6) +
      scale_size_continuous(name = "Count of Injury", range = c(3, 15)) +
      scale_y_continuous(
        breaks = seq(0, 24, by = 2),
        limits = c(0, 24),
        expand = c(0, 0)
      ) +
      labs(
        x = "Month",
        y = "Hour of the Day"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 1.5),
        axis.title.x = element_text(vjust = -0.5)
      )
  }, height = 650)
  
  # Reactive data for plot 8
  reactive_plot8_data <- reactive({
    data <- reactive_crash_data()
    if (input$safety_device != "All") {
      data <- data %>% filter(SafetyDevice == as.logical(input$safety_device))
    }
    data %>%
      group_by(Hour, HourLabel, SafetyDevice) %>%
      summarise(Count = n(), .groups = 'drop')
  })
  
  # Plot 8 output
  output$plot8 <- renderPlot({
    reactive_plot8_data() %>%
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
  
  # Reactive data for plot 9
  reactive_plot9_data <- reactive({
    data <- reactive_crash_data()
    if (input$injury_severity != "All") {
      data <- data %>% filter(Injury == input$injury_severity)
    }
    data %>%
      group_by(Hour, HourLabel, Injury) %>%
      summarise(Count = n(), .groups = 'drop')
  })
  
  # Plot 9 output
  output$plot9 <- renderPlot({
    reactive_plot9_data() %>%
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
  
  # Reactive data for plot 10
  reactive_plot10_data <- reactive({
    data <- reactive_crash_data() 
    if (input$injury_type_plot10 != "All") {
      data <- data %>% filter(Injury == input$injury_type_plot10)
    }
    
    return(data)
  })
  
  #Plot for Tab 10
  output$plot10 <- renderPlot({
    reactive_plot10_data() %>%
      ggplot(aes(x = as.factor(SafetyDevice))) + 
      geom_bar(fill = "steelblue") +
      labs(
        title = "Safety Device Usage by Injury Type",
        x = "Safety Device Used (Not Used/Used)",
        y = "Count"
      ) +
      scale_x_discrete(labels = c("FALSE" = "Not Used", "TRUE" = "Used")) +
      theme_minimal()
  })
  
  # Reactive data for plot 11
  reactive_plot11_data <- reactive({
    data <- reactive_crash_data()  
    if (input$injury_type_plot11 != "All") {
      data <- data %>% filter(Injury == input$injury_type_plot11)
    }
    
    return(data)
  })
  
  #Plot for Tab 11
  output$plot11 <- renderPlot({
    reactive_plot11_data() %>%
      ggplot(aes(x = Age)) + 
      geom_bar(fill = "steelblue") +
      scale_x_continuous(
        breaks = seq(min(reactive_plot11_data()$Age, na.rm = TRUE), 
                     max(reactive_plot11_data()$Age, na.rm = TRUE), 
                     by = 2)
      ) +
      labs(
        title = "Frequency of Age by Injury Severity",
        x = "Age",
        y = "Count"
      ) +
      theme_minimal()
  })
} # End server

#### ========== Run the App ==========
shinyApp(ui = ui, server = server)