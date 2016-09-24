library(readr)
library(shiny)
library(dplyr)
library(ggplot2)

scorecard_df <- read_csv('./data_sources/Scorecard_v3.csv')
zip_map <- read_csv('./data_sources/zipcode_info_table.csv')

ui <- shinyUI(fluidPage(
   # Application title
   titlePanel("College Score Card Exploration"),
   fluidRow(
     column(width = 2,
                   textInput('zipcode', 'Enter Zipcode')
            ),
     column(width = 8,
            plotOutput("distPlot")
            )
   )
)
)

server <- shinyServer(function(input, output) {
   output$distPlot <- renderPlot({
     if ((nchar(input$zipcode)==5) & (input$zipcode %in% zip_map$ZIP)){
     example_zip <- zip_map[zip_map$ZIP==input$zipcode, ]
     print(input$zipcode)
     scorecard_df$in_state <-  ifelse(scorecard_df$STABBR==example_zip$state_abb, 'In-state', 'Out-of-state')

     scorecard_df$DIST_MILES <- unlist(
       sapply(1:nrow(scorecard_df),
              function(zip_index){
                geosphere::distm(c(scorecard_df$LONGITUDE[zip_index], scorecard_df$LATITUDE[zip_index]),
                                 c(example_zip$LNG, example_zip$LAT),
                                 fun = geosphere::distHaversine)/1609.34
              }
       ))
     ggplot(scorecard_df, aes(x=DIST_MILES, fill=in_state)) + 
       geom_histogram(bins = 30) +
       ggtitle('Occurrences of universities at binned distances from entered zipcode')
       
     }
   })
})

# Run the application
shinyApp(ui = ui, server = server)
