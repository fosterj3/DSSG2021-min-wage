#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggdist)
library(tidyquant)

hud_long <- read_csv("hud_long.csv") %>% 
    select(-1) %>%  arrange(region,year)

hud_long$Bedrooms <- factor(hud_long$Bedrooms, levels = c("Studio", "One Bedroom", "Two Bedrooms", "Three Bedrooms", "Four Bedrooms"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fair Market Rent in Washington State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
    #Bedroom Slider 
        sidebarPanel(
        selectInput("region", 
                    "Region:", 
                    unique(hud_long$region)),
        selectInput("year",
                    "Year:",
                    unique(hud_long$year))),
        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
           tabPanel("Visualization",plotOutput("trend")),
           tabPanel("Data", DT::DTOutput("table"))
        )
    )
))

# Define server logic 
server <- function(input, output, session) {

    output$trend <- renderPlot({
        # generate plot
        kraken <- c("#001628", "#355464", "#68a2b9", "#99d9d9", "#e9072b")
        
        hud_long %>%
            filter(region == input$region) %>%
            filter(year == input$year) %>% 
            ggplot(aes(x = factor(Bedrooms), y = Rent, fill = factor(Bedrooms))) +
            # add half-violin from ggdist package
            ggdist::stat_halfeye(
                ## remove slab interval
                .width = 0,
                point_colour = NA) +
            
            # Adjust theme
            scale_fill_tq() +
            theme_tq() +
            labs(
                title = "Distribution of Fair Market Rent by Region",
                caption = "Data from Department of Housing and Urban Development",
                x = "",
                y = "Rental Price",
                fill = "Bedrooms") +
            theme(legend.position = "none") +
            scale_y_discrete(limits = c(500, 1000, 1500, 2000, 2500, 3000), labels=scales::dollar_format()) +
            scale_fill_manual(values = kraken) +
            coord_flip()
    })
    output$table <- DT::renderDT({
        hud_long %>%
            select(-c(cbsa, state)) %>% 
            filter(region == input$region) %>% 
            filter(year == input$year)
    })

}
# Run the application 
shinyApp(ui = ui, server = server)

        
        
        
        
        