###SHINY APP FOR ADJUSTABLE AUTO SALES PLOT (X AXIS WILL CHANGE (YEAR))###


#setup
library(shiny)
library(tidyverse)

#read in data
car_data <- 
    read_csv("yearly_vehicle_sales.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Historic Canadian Automobile Sales (1960-2014)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sales_year",
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

# Define server logic required to draw a plot
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        car_data %>% 
            filter(Year < input$sales_year) %>% 
            ggplot(aes(x = Year, y = Total )) + 
            theme_minimal() + 
            geom_point() + 
            geom_line() +             
            labs(x = "Year", 
                 y = "Total Number of New Vehicles Sold",
                 title = "
       Historic Canadian Automobile Sales with a Variable X Axis Range")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
