library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(plotly)
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
                  value = range(year(min(tweets_df$time)),
                                year(max(tweets_df$time))),
                  step = 1,  # Assuming yearly granularity
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("topCountiesHistogram")
    )
  )
)

server <- function(input, output, session) {
  selected_counties <- reactiveVal(c()) # Initialize with an empty vector
  
  filtered_tweets <- reactive({
    tweets_df %>%
      filter(category == input$categoryInput,
             year(time) >= input$dateInput[1],
             year(time) <= input$dateInput[2])
  })
  
  output$topCountiesHistogram <- renderPlotly({
    top_counties <- filtered_tweets() %>%
      group_by(county) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(5)
    
    p <- ggplot(top_counties, aes(x = reorder(county, -count), y = count)) +
      geom_bar(stat = "identity", fill = "chocolate") +
      labs(x = "County", y = "Number of Tweets", title = "Top 5 Counties") +
      theme_classic()
    ggplotly(p) %>% layout(dragmode = "select")
  })
  
  observe({
    brush <- input$topCountiesHistogram_brush
    if (!is.null(brush)) {
      top_counties <- filtered_tweets() %>%
        group_by(county) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(5)
      
      selected <- top_counties[brush$ymin <= top_counties$count & top_counties$count <= brush$ymax, ]$county
      selected_counties(selected)
    }
  })
  
  # Create a reactive expression for the Leaflet polygons
  county_polygons <- reactive({
    tweet_counts <- filtered_tweets() %>%
      group_by(county) %>%
      summarise(count = n()) %>%
      mutate(NAME = county)
    
    counties_with_tweets <- left_join(counties_geojson, tweet_counts, by = "NAME")
    
    pal <- colorBin("YlOrRd", domain = counties_with_tweets$count, bins = 10, na.color = "#808080")
    sel <- selected_counties()
    counties_with_tweets$selected <- counties_with_tweets$NAME %in% sel
    
    list(data = counties_with_tweets, pal = pal)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addProviderTiles(providers$OpenStreetMap)
  })
  
  # Observe changes in selected counties and update the map
  observe({
    leafletProxy("map", data = county_polygons()$data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ifelse(selected, "black", county_polygons()$pal(count)),
        weight = 2,
        color = "#BDBDC3",
        fillOpacity = 0.7,
        popup = ~paste(NAME, ": ", count, " tweets")
      )
  })
}

# Run the app
shinyApp(ui, server)