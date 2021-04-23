

###SHINY APP FOR ADJUSTABLE OIL PRICE PLOT (X AXIS WILL CHANGE (YEAR))###


#setup
library(shiny)
library(tidyverse)

#read in data
oil_data <- 
    read_csv("OPEC_Historic_Oil_Prices_Statista_CSV.csv")
    
#Trim oil price range 
oil_trimmed <- 
    oil_data %>% 
    filter(`OPEC oil price annually 1960-2021` <= 2014) %>% 
    rename(c('Year' = 'OPEC oil price annually 1960-2021')) %>% 
    rename(c('price_in_usd' = "X2"))

#Change oil price year from character to numeric
oil_trimmed$Year <- as.numeric(as.character(oil_trimmed$Year))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Historic OPEC Oil Prices (1960-2014)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("oil_year",
                        "Select the Last Year You Wish to Include in the Range",
                        min = 1960,
                        max = 2014,
                        value = 2014,
                        sep = "")
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
        
        oil_trimmed %>% 
            filter(Year < input$oil_year) %>% 
            ggplot(aes(x = Year, y = price_in_usd )) +
                theme_minimal() + 
                geom_point() + 
                geom_line() +
                scale_y_continuous(labels=scales::dollar_format()) + #creates $ on the y axis
                labs(x = "Year", 
                     y = "OPEC Price of Oil per Barrel (in USD)", 
                     title = "
           OPEC World Oil Prices With an Adjustable X-Axis Range")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
