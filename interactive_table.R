library(shiny)
library(dplyr)
library(DT)

# Read and preprocess the datasets
tweets_df <- read.csv("tweets_data.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Tweets Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = unique(tweets_df$state)),
      selectInput("county", "Select a County:", choices = ""),
      selectInput("category", "Select a Category:", choices = ""),
      actionButton("showTableButton", "Show Most Retweeted Tweets"),
      DTOutput("retweetTable")
    ),
    mainPanel()
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Update county choices based on selected state
  observe({
    state_data <- subset(tweets_df, state == input$state)
    county_choices <- unique(state_data$county)
    updateSelectInput(session, "county", choices = county_choices)
  })
  
  # Update category choices based on selected state and county
  observe({
    filtered_data <- tweets_df %>%
      filter(state == input$state, county == input$county)
    category_choices <- unique(filtered_data$category)
    updateSelectInput(session, "category", choices = category_choices)
  })
  
  # Interactive table for most retweeted tweets
  output$retweetTable <- renderDT({
    filtered_data <- tweets_df %>%
      filter(state == input$state, county == input$county, category == input$category) %>%
      arrange(desc(retweet)) %>%
      head(10)  # Display the top 10 most retweeted tweets
    
    datatable(filtered_data)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
