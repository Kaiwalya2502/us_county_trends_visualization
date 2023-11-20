library(shiny)
library(leaflet)
library(sf)

# Define UI
ui <- fluidPage(
    titlePanel("US Counties Map"),
    leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
    # Read the GeoJSON file
    counties_geojson <- st_read("/Users/kai/Downloads/counties.geojson")

    # Transform CRS to WGS84 if needed
    if (st_crs(counties_geojson)$epsg != 4326) {
        counties_geojson <- st_transform(counties_geojson, crs = 4326)
    }

    output$map <- renderLeaflet({
        leaflet(data = counties_geojson) %>% 
            addProviderTiles(providers$OpenStreetMap) %>%
            addPolygons(popup = ~NAME, weight = 1)
    })
}

# Run the app
shinyApp(ui, server)
