

###SHINY APP FOR ADJUSTABLE INFLATION RATE PLOT (X AXIS WILL CHANGE (YEAR))###


#setup
library(shiny)
library(tidyverse)

#read in data
inflation_rates_s <- read_csv("Inflation_Rates_sourced.csv")

#Inflation Rates Cleaning
inflation_rates_cleaned <- 
    inflation_rates_s %>% 
    filter(Year <= 2014 & Year >= 1960)

#convert inflation rate to numeric value
inflation_rates_cleaned$inflation_rate <- 
    as.numeric(as.character(inflation_rates_cleaned$inflation_rate))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Historic Canadian Inflation Rates (1960-2014)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("inflation_year",
                        "Select the Last Year You Wish to Include in the Range",
                        min = 1960,
                        max = 2014,
                        value = 2014,
                        sep = "") #remove commas for years (i.e. 2,001 becomes 2001)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to generate plot
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        inflation_rates_cleaned %>% 
            filter(Year < input$inflation_year) %>% 
            ggplot(aes(x = Year, y = inflation_rate)) + 
            theme_minimal() + 
            geom_point() + 
            geom_line() + 
            scale_y_continuous(labels=scales::percent_format(scale = 1)) + #to put % sign on y axis
            labs(x = "Year", 
                 y = "Inflation Rate",
                 title = "Inflation Rates in Canada with an Adjustable X Axis Range")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
