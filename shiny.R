library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(lubridate)
library(RColorBrewer)

# Read and preprocess the datasets
counties_geojson <- st_read("/Users/kai/Downloads/counties.geojson")
tweets_df <- read.csv("/Users/kai/Documents/STAT679/tweets_data.csv")

# Transform CRS and preprocess date
counties_geojson <- st_transform(counties_geojson, crs = 4326)
tweets_df$time <- ymd_hms(tweets_df$time)

# Define UI
ui <- fluidPage(
  titlePanel("US Counties and Tweets Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categoryInput", "Select Category:", 
                  choices = unique(tweets_df$category)),
      dateRangeInput("dateInput", "Select Date Range:",
                     start = min(tweets_df$time), 
                     end = max(tweets_df$time),
                     min = min(tweets_df$time), 
                     max = max(tweets_df$time)),
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_tweets <- reactive({
    tweets_df %>%
      filter(category == input$categoryInput,
             time >= input$dateInput[1],
             time <= input$dateInput[2],
             )
  })
  
  output$map <- renderLeaflet({
    # Count tweets per county
    tweet_counts <- filtered_tweets() %>%
      group_by(county) %>%
      summarise(count = n()) %>%
      mutate(NAME = county) # Adjust this line as needed to match county names
    
    # Join with county data
    counties_with_tweets <- left_join(counties_geojson, tweet_counts, by = "NAME")
    
    # Create a color palette
    pal <- colorBin("YlOrRd", domain = counties_with_tweets$count, bins = 10, na.color = "#808080")
    
    leaflet(data = counties_with_tweets) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(count),
        weight = 2,
        color = "#BDBDC3",
        fillOpacity = 0.7,
        popup = ~paste(NAME, ": ", count, " tweets")
      )
  })
}

# Run the app
shinyApp(ui, server)

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(lubridate)
library(RColorBrewer)

# Read and preprocess the datasets
counties_geojson <- st_read("/Users/kai/Downloads/counties.geojson")
tweets_df <- read.csv("/Users/kai/Documents/STAT679/tweets_data.csv")

# Transform CRS and preprocess date
counties_geojson <- st_transform(counties_geojson, crs = 4326)
tweets_df$time <- ymd_hms(tweets_df$time)

# Define UI
ui <- fluidPage(
  titlePanel("US Counties and Tweets Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("categoryInput", "Select Category:", 
                  choices = unique(tweets_df$category)),
      sliderInput("dateInput", "Select Year:",
                  min = year(min(tweets_df$time)), 
                  max = year(max(tweets_df$time)),
                  value = range(year(min(tweets_df$time)), year(max(tweets_df$time))),
                  step = 1,  # Assuming yearly granularity
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_tweets <- reactive({
    tweets_df %>%
      filter(category == input$categoryInput,
             year(time) >= input$dateInput[1],
             year(time) <= input$dateInput[2])
  })
  
  output$map <- renderLeaflet({
    # Count tweets per county
    tweet_counts <- filtered_tweets() %>%
      group_by(county) %>%
      summarise(count = n()) %>%
      mutate(NAME = county) # Adjust this line as needed to match county names
    
    # Join with county data
    counties_with_tweets <- left_join(counties_geojson, tweet_counts, by = "NAME")
    
    # Create a color palette
    pal <- colorBin("Viridis", domain = counties_with_tweets$count, bins = 10, na.color = "#808080")
    
    leaflet(data = counties_with_tweets) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(
        fillColor = ~pal(count),
        weight = 2,
        color = "#BDBDC3",
        fillOpacity = 0.7,
        popup = ~paste(NAME, ": ", count, " tweets")
      )
  })
}

# Run the app
shinyApp(ui, server)
