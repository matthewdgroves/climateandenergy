#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(zoo)

#source('scripts/utils.R', chdir = T)
source('scripts/modtran.R', chdir = T)

mlo_data = read_csv('data/mlo_data.csv', 
                    skip = 57,  # skip the first 57 rows
                    col_names = c('year', 'month', 'date.excel', 'date',
                                  'co2.raw', 'co2.raw.seas', 
                                  'co2.fit', 'co2.fit.seas',
                                  'co2.filled', 'co2.filled.seas'),   # give reasonable titles for each column
                    col_types = 'iiiddddddd', # the first three columns are integers
                    # and the next 7 are real numbers
                    na = '-99.99' # interpret -99.99 as a missing value
)

mlo_simple = mlo_data %>% select(date, co2 = co2.filled) # keeping only the regular CO2 data with interpolated numbers for holes in the data

# head(mlo_simple) #sampling the first few rows


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Global Carbon Dioxide Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("months",
                        "Number of Months for the Rolling Average:",
                        min = 1,
                        max = 36,
                        value = 12),
        
          sliderInput("yearsRange",
                      "Date",
                      min = 1955,
                      max = 2018,
                      value = c(1955,2018),
                      sep=''),
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({

      mlo_simple <- mutate(mlo_simple, 
                           trend = rollapply(data = co2, width = input$months, FUN = mean, fill = NA, align = "center"))
      
      mlo_simple <- mlo_simple %>%
        filter(date >= input$yearsRange[1] & date <= input$yearsRange[2])
      
      
      ggplot(data = mlo_simple, aes(x = date)) + 
        geom_line(aes(y = co2, color = "Raw")) +
        geom_line(aes(y = trend, color = "rolling average"), size = 1.5) +
        scale_color_manual(values = c("Raw" = "black", "rolling average" = "blue"), name = "Key:") +
        labs(x = "Year", y = "CO2 concentration (ppm)", title = "Measured and Seasonally Adjusted CO2")
      
      #linear relationship:
      # mlo_simple %>% mutate(trend = rollapply(data = co2, width = 12, FUN = mean, fill = NA, align = "center")) %>%
      #   ggplot(aes(x = date, y = co2)) + 
      #   geom_line() +
      #   geom_smooth(method = 'lm', color="red") +  #linear relationship of co2 and the date
      #   labs(x = "Year", y = "CO2 concentration (ppm)", title = "Measured CO2 and Linear Fit")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
