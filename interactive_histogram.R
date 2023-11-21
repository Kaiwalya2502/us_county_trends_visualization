library(shiny)
library(dplyr)
library(ggplot2)

# Read and preprocess the datasets
tweets_df <- read.csv("tweets_data.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Tweets Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = unique(tweets_df$state)),
      actionButton("plotButton", "Generate Histogram"),
      uiOutput("categorySelector")
    ),
    mainPanel(
      plotOutput("topStatesPlot"),
      plotOutput("countyCategoryPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Static histogram for top 30 states
  output$topStatesPlot <- renderPlot({
    top_states <- tweets_df %>%
      group_by(state) %>%
      summarize(total_tweets = n()) %>%
      arrange(desc(total_tweets)) %>%
      head(30)
    
    ggplot(data = top_states, aes(x = reorder(state, -total_tweets), y = total_tweets)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Top 30 States with Most Tweets",
           x = "State",
           y = "Total Tweets") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Dynamic UI for category selection
  output$categorySelector <- renderUI({
    state_data <- subset(tweets_df, state == input$state)
    category_choices <- unique(state_data$category)
    checkboxGroupInput("categories", "Select Categories:", choices = category_choices, selected = category_choices)
  })
  
  # Interactive histogram for county-level tweets by category
  output$countyCategoryPlot <- renderPlot({
    state_data <- subset(tweets_df, state == input$state)
    filtered_data <- state_data %>%
      filter(category %in% input$categories) %>%
      group_by(county, category) %>%
      summarize(total_tweets = n())
    
    ggplot(data = filtered_data, aes(x = county, y = total_tweets, fill = category)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Tweets by Category in", input$state),
           x = "County",
           y = "Total Tweets") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
