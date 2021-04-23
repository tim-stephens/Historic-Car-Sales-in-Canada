

###SHINY APP FOR ADJUSTABLE UNEMPLOYMENT RATE PLOT (X AXIS WILL CHANGE (YEAR))###


#setup
library(shiny)
library(tidyverse)
library(janitor)

#read in data
unemp_data <- 
    read_csv("unemployment_rates_can.csv")

#Cleaning the unemployment rate data set
unemployment_cleaned <- 
    unemp_data %>%
    clean_names() %>% 
    filter(geo == 'Canada' & # we want national data
               sex == 'Both sexes' & # for both sexes
               age_group == '15 years and over' & # who are over the age of 15
               labour_force_characteristics == 'Unemployment rate') %>% # we only want unemployment rate data
    select(c(ref_date, geo, labour_force_characteristics, age_group, sex, value ))

#trim to columns of interest (year, unemployment rate)
unemployment_trimmed <- 
    unemployment_cleaned %>% 
    select(c(ref_date, value)) %>% 
    rename(c('Year' = 'ref_date')) %>% 
    rename(c('unemployment_rate' = 'value'))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Historic Canadian Unemployment Rates (1976-2018)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("unemployment_year",
                        "Select the Last Year You Wish to Include in the Range",
                        min = 1976,
                        max = 2018,
                        value = 2018,
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
        
        unemployment_trimmed %>% 
            filter(Year < input$unemployment_year) %>% 
            ggplot(aes(x = Year, y = unemployment_rate )) + 
            theme_minimal() + 
            geom_point() + 
            geom_line() + 
            scale_y_continuous(labels=scales::percent_format(scale = 1)) + #this gives percentage signs on y axis
            labs(x = "Year", 
                 y = "Unemployment Rate",
                 title = "
       Unemployment Rates in Canada with a Variable X Axis Range")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
