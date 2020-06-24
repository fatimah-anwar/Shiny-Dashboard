# Install the required packages
install.packages('tidyverse')
install.packages('leaflet')
install.packages('shinydashboard')
install.packages('plotly')

# Import the required packages
library(tidyverse)
library(shiny)
library(leaflet)
library(shinydashboard)
library(plotly)

# Read the dataset
att_df <- read_csv("attendance_dataset.csv")

# Find the total number of attendees for each country
total_df <- att_df %>%
    group_by(country) %>%
    summarise(total = sum(attendees)) %>%
    left_join(att_df) %>%
    select(-date , -attendees) %>%
    rename(attendees = total) %>%
    distinct(country , .keep_all = TRUE)

# Define UI for the application
ui <- dashboardPage(
    dashboardHeader(title = "Summer Workshops"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Line Plot",
                     tabName = "line_plot_tab"
            ),
            menuItem(text = "Map",
                     tabName = "map_tab"
            )
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "line_plot_tab",
                
                titlePanel(title = "Attendees Daily Trends"),
                
                selectInput(inputId = "countries",
                            label = "Select a Country:",
                            choices = c(total_df %>% 
                                            filter(attendees > 10) %>% 
                                            select(country)
                            )
                ),
                
                plotlyOutput(outputId = "line_plot",
                             width = "60%", 
                             height = "300px"
                )
            ),
            tabItem(
                tabName = "map_tab",
                
                titlePanel(title = "Distribution of Workshops Attendance"),
                
                leafletOutput(outputId = 'map'), 
                
                absolutePanel(top = 140, 
                              right = 30, 
                              width = 200,
                              selectInput(inputId = "dates",
                                          label = "Select a Date:",
                                          choices = c("All", att_df %>%
                                                          select(date) %>%
                                                          distinct(date)
                                          )
                              )
                )
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    # 1. Code for the line plot tab
    
    # Create a dataframe based on the selected country
    country_df <- reactive({
        att_df %>%
            filter(country == input$countries)
    }) 
    
    # Render the line plot
    output$line_plot <- renderPlotly ({
        country_df() %>% 
            ggplot() +
            geom_line(aes(x=date, y=attendees), color="#088ca3", alpha=0.5 , size = 1) +
            ggtitle("Number of Attendees per Day") +
            xlab('Dates') +
            ylab('Attendees')
    })
    
    
    # 2. Code for the map tab
    
    # Create a dataframe based on the selected date
    dates_df <- reactive({
        if(input$dates == "All"){
            total_df
        }
        else{
            att_df %>%
                filter(date == input$dates)
        }
    }) 
    
    # Prepare the flag icons for the map markers based on the selected date
    filtered_flags <- reactive({
        makeIcon(
            iconUrl = paste0("Flags/", dates_df()$country, ".png"),
            iconWidth = 25,
            iconHeight = 25
        )
    })
    
    # Render the map
    output$map <- renderLeaflet({
        dates_df() %>% 
            leaflet() %>% 
            addProviderTiles(providers$Esri.WorldTopoMap) %>% 
            setView(26.3351,17.228331, zoom = 2) %>% 
            addMarkers(
                lng = ~longitude,
                lat = ~latitude,
                icon = filtered_flags(),
                labelOptions = labelOptions(textsize = "12px"),
                popup = ~paste0(
                    "<center>", 
                    "<b>", 
                    country,
                    "<br>", 
                    formatC(attendees, format="d", big.mark=","),
                    "</b>", 
                    " attendee(s)", 
                    "</center>"
                )
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
