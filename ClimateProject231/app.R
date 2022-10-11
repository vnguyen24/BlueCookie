# Load necessary packages
library(shiny)
library(tidyverse)
library(ggrepel)
library(lubridate)

# tab 1: justin
# tab 2: cuong
# tab 3: justin

# Import data
avg_temp_data <- read_csv("data/first_visual.csv")
data <- read.csv("data/second_visual.csv")
yearly_precipitation <- read_csv("data/third_visual.csv")

#############################################################
# Define choice values and labels for widgets (user inputs) #
# - Define vectors for choice values and labels             #
# - Can then refer to them in server                        #
#############################################################

# For TAB 1 & 3 widgets:

city_choice_values <- list("Boston" = "boston", "Seattle" = "seattle", 
                           "New York City" = "nyc", "Anchorage" = "anchorage", 
                           "Amherst" = "amherst", "Chicago" = "chicago", 
                           "Dallas" = "dallas", "Honolulu" = "honolulu", 
                           "Los Angeles" = "los angeles", "Miami" = "miami")
month_choice_values <- list("January" = "1", "February" = "2", "March" = "3", 
                            "April" = "4", "May" = "5", "June" = "6", 
                            "July" = "7", "August" = "8", "September" = "9", 
                            "October" = "10", "November" = "11", 
                            "December" = "12")

# For the scatter plot widget:
# For selectInput, choices object should be a NAMED list 
choice_values <- c("min_temp", "max_temp", "avg_temp", "precipitation")
choice_names <- c("Minimum temperature (°F)", "Maximum temperature (°F)", 
                  "Mean temperature (°F)", "Precipitation (inches)")
names(choice_values) <- choice_names

############
#    ui    #
############
ui <- navbarPage(
  
  title = "Climate of the US",
  
  # Tab 1: Timeplot
  tabPanel(
    title = "Timeplot",
    
    sidebarLayout(
    
       sidebarPanel(
         # pull-down menu for month
        selectInput(inputId = "month_time",
                     label = "Pick a month:",
                     choices = month_choice_values,
                     selected = "1"),
        
        # checkboxes for cities
        checkboxGroupInput(inputId = "cities_time",
                     label = "Choose cities:",
                     choices = city_choice_values,
                     selected = 'amherst'),
        
        # date range
        dateRangeInput(inputId = "dates_time", 
                       label = "Date range",
                       start = "1980-01-01",
                       end = NULL)
 
       ),
      mainPanel(plotOutput(outputId = "time")),
    )
  ),
  
  #Tab 2: Scatterplot
  tabPanel(
    title = "Scatterplot",
    
    sidebarLayout(
      
      sidebarPanel(
        
        numericInput(inputId = "year",
                     label = "Specify the year on interest (1869 - 2022)",
                     value = 2002, min = 1869, max = 2022),
        
        selectInput(inputId = "x",
                    label = "Choose variable x",
                    choices = choice_values,
                    selected = "min_temp"),
        
        selectInput(inputId = "y",
                    label = "Choose variable y",
                    choices = choice_values,
                    selected = "max_temp")
      ),
      mainPanel(plotOutput(outputId = "scatter")),
    )
  ),
    
    #Tab 3: Lineplot
    tabPanel(
      title = "Lineplot",
      
      sidebarLayout(
        
        sidebarPanel(
  
        # pick a city button  
        radioButtons(inputId = "cities_line",
                             label = "Choose cities:",
                             choices = city_choice_values,
                             selected = 'amherst'),
        
        # choose a year range
        sliderInput(inputId = "date_line", 
                    label = "Pick a Year Range: ", 
                    min = min(yearly_precipitation$year), 
                    max =  max(yearly_precipitation$year), 
                    value = c(1980, 2020),
                    sep = "")
        ),
        
        mainPanel(plotOutput(outputId = "lineplot"))
      )
    )
  )
  


############
# server   #
############
server <- function(input, output){

  
  # TAB 1: INTERACTIVE TIMEPLOT 
  output$time <- renderPlot({
    avg_temp_data %>%
      # only use the cities that are checked off
      # only use the month selected
      # only use the date range selected
      filter(city %in% c(input$cities_time), 
             month_num %in% input$month_time,
             date >= input$dates_time[1] & date <= input$dates_time[2])%>%
      ggplot(aes(x = date, y = avg_temp, color = city)) +
      # create a scatterplot and a best-fit line (no error)
      geom_point() +
      geom_smooth(se = FALSE) +
      labs(title = "Average Temperature Trends", 
           x = "Year", 
           y = "Average Temperature (F°)",
           color = "City") +
      # use the same scale for all cities
      scale_y_continuous(limits = c(0, 80)) +
      # bold title
      theme(plot.title = element_text(face = "bold")) 
  })
  

  #TAB 2: Interactive scatter plot
  output$scatter <- renderPlot({
    data %>%
      #First interactive component: filter year by what the user picks
      filter(year %in% c(input$year)) %>%
      #Second interactive component: Create a plot using variables that users choose
      #Note that we have to use .data[[input$var_name]] to force shiny to look into the data set
      #Else the code will not function properly.
      ggplot(aes(x = .data[[input$x]], y = .data[[input$y]], color = city)) +
      geom_point(size = 5) +
      #Third interactive component: The labels will be reflected accordingly to what users choose
      labs(x = choice_names[choice_values == input$x],
           y = choice_names[choice_values == input$y],
           title = "Relationship between variables of choice of all cities, 
           subset by year",
           color = "City") +
      theme(plot.title = element_text(face = "bold"))
      
  })

  #TAB 3: INTERACTIVE LINEPLOT
  output$lineplot <-renderPlot({
    yearly_precipitation %>%
      # only use selected city
      # only use years in the year range
      filter(city == input$cities_line) %>%
      filter(year >= input$date_line[1] & year <= input$date_line[2]) %>% 
      ggplot(aes(x = year, y = precipitation)) +
      # create line plot and line of best fit (with error)
      geom_smooth() + 
      geom_line() + 
      # same scale for all cities
      scale_y_continuous(limits = c(0, 100)) +      
      labs(title = "Annual Precipitation over Time", 
           x = "Year", 
           y = "Annual Precipitation (inches)") +
      # bold title
      theme(plot.title = element_text(face = "bold")) 
  })
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)