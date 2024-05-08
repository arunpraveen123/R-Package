# Load required libraries
library(shiny)
library(recommenderlab)
library(reshape2)
library(data.table)
library(ggplot2)

# Load 
movie_data <- fread("movies.csv", stringsAsFactors = FALSE)
rating_data <- fread("ratings.csv")

# Data preprocessing
# Transform rating data to matrix format
rating_matrix <- dcast(rating_data, userId ~ movieId, value.var = "rating", na.rm = FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1]) # Remove userId column

# Convert to recommenderlab's realRatingMatrix
rating_matrix <- as(rating_matrix, "realRatingMatrix")

# Define UI
ui <- fluidPage(
  titlePanel("Movie Recommendation System"),
  sidebarLayout(
    sidebarPanel(
      # User input for movie recommendation
      numericInput("user_id", "Enter User ID:", value = 1, min = 1),
      actionButton("recommend_button", "Get Recommendations"),
      br(),
      br(),
      p("Note: Recommendations are based on user ID entered above.")
    ),
    mainPanel(
      h3("Recommended Movies:"),
      tableOutput("recommendation_table"),
      br(),
      plotOutput("similarity_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load the recommender model
  recommender_model <- reactive({
    # Train the item-based collaborative filtering model
    recommender <- Recommender(data = rating_matrix, method = "IBCF")
    return(recommender)
  })

  # Generate recommendations based on user input
  recommendations <- eventReactive(input$recommend_button, {
    user_id <- input$user_id

    # Generate top 10 movie recommendations for the specified user
    recommender <- recommender_model()
    predicted_recommendations <- predict(recommender, rating_matrix[user_id, ], n = 10)

    # Retrieve movie titles from recommendations
    recommended_movies <- predicted_recommendations@items[[1]]
    recommended_titles <- movie_data$title[movie_data$movieId %in% recommended_movies]

    # Return the recommendations as a data frame
    data.frame(Movie_Title = recommended_titles)
  })

  # Display the recommendations in a table
  output$recommendation_table <- renderTable({
    recommendations()
  })

  # Plot similarity matrix
  output$similarity_plot <- renderPlot({
    recommender <- recommender_model()
    model_info <- getModel(recommender)

    # Plot similarity matrix as heatmap
    image(model_info$sim, main = "Item Similarity Heatmap", axes = FALSE)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
