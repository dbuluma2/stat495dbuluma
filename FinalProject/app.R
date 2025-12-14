library(shiny)
library(tidyverse)
library(Matrix)
library(recommenderlab)


# Load raw data
books   <- read.csv("Data/books.csv")
ratings <- read.csv("Data/ratings.csv")
users   <- read.csv("Data/users.csv")

# Clean & format books table
books_clean <- books |>
  select(ISBN, Book.Title, Book.Author) |>
  rename(book_id = ISBN, book_title = Book.Title, author = Book.Author) |>
  mutate(book_id = as.character(book_id)) |>
  distinct(book_id, .keep_all = TRUE)

# Clean & format ratings table
ratings_clean <- ratings |>
  rename(user_id = User.ID, book_id = ISBN, book_rating = Book.Rating) |>
  mutate(
    user_id = as.character(user_id),
    book_id = as.character(book_id)
  ) |>
  filter(book_rating > 0)

# Clean & format users table
users_clean <- users |>
  rename(user_id = User.ID) |>
  mutate(user_id = as.character(user_id)) |>
  distinct(user_id, .keep_all = TRUE)

# Join ratings with book and user metadata
ratings_full <- ratings_clean |>
  inner_join(books_clean, by = "book_id") |>
  inner_join(users_clean, by = "user_id")

# Apply thresholds: require minimum ratings per user and per item
min_user_ratings <- 10
min_item_ratings <- 10

user_counts <- ratings_full |> count(user_id, name = "n_user")
item_counts <- ratings_full |> count(book_id, name = "n_item")

ratings_filtered <- ratings_full |>
  inner_join(user_counts |> filter(n_user >= min_user_ratings), by = "user_id") |>
  inner_join(item_counts |> filter(n_item >= min_item_ratings), by = "book_id")

# Build user–item matrix
user_levels <- sort(unique(ratings_filtered$user_id))
item_levels <- sort(unique(ratings_filtered$book_id))

rating_sparse <- sparseMatrix(
  i = match(ratings_filtered$user_id, user_levels),
  j = match(ratings_filtered$book_id, item_levels),
  x = ratings_filtered$book_rating,
  dims = c(length(user_levels), length(item_levels)),
  dimnames = list(user_levels, item_levels)
)

rating_matrix <- as(rating_sparse, "realRatingMatrix")

# Lookup table for book metadata
book_lookup <- ratings_filtered |>
  select(book_id, book_title, author) |>
  distinct(book_id, .keep_all = TRUE)


# Shiny UI with tabs
ui <- fluidPage(
  titlePanel("Simple Interactive kNN Book Recommender"),
  tabsetPanel(
    tabPanel("Recommendations",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   "user",
                   "Select User ID:",
                   choices = rownames(rating_matrix),
                   options = list(placeholder = "Type to search...", maxOptions = 2000)
                 ),
                 sliderInput("k", "Neighborhood size (k):", min = 5, max = 100, value = 20, step = 5),
                 sliderInput("n", "Number of recommendations:", min = 1, max = 20, value = 10),
                 checkboxInput("show_ids", "Show book IDs (ISBN) too", value = FALSE)
               ),
               mainPanel(
                 h3("Recommendations"),
                 tableOutput("recs")
               )
             )
    ),
# Lookup Tab
    tabPanel("User Lookup",
             sidebarLayout(
               sidebarPanel(
                 textInput("lookup_id", "Enter User ID:", ""),
                 actionButton("lookup_btn", "Lookup")
               ),
               mainPanel(
                 h3("User Information"),
                 tableOutput("user_info")
               )
             )
    )
  )
)


# Shiny server

server <- function(input, output, session) {
  
  # Reactive recommender model based on current k
  model_reactive <- reactive({
    Recommender(
      rating_matrix,
      method = "UBCF",
      parameter = list(nn = input$k, method = "Cosine", normalize = "center")
    )
  })
  
  # Generate recommendations for selected user
  output$recs <- renderTable({
    
    # Ensure both user and number of recommendations (n) are provided
    req(input$user, input$n)
    
    # If the selected user is not in the rating matrix, return a message
    if (!input$user %in% rownames(rating_matrix)) {
      return(data.frame(Message = "Selected user not found in matrix."))
    }
    
    # Build the recommender model reactively based on current k
    model <- model_reactive()
    
    # Extract the rating row for the selected user
    user_rrm <- rating_matrix[input$user, , drop = FALSE]
    
    # Generate top-n recommendations for this user
    recs <- predict(model, user_rrm, n = input$n)
    
    # Convert recommendations to a list of item IDs
    rec_ids <- as(recs, "list")[[1]]
    
    # If no recommendations are returned, display a message
    if (length(rec_ids) == 0) {
      return(data.frame(Message = "No recommendations returned."))
    }
    
    # Predict preference scores (estimated ratings) for recommended items
    score_mat <- as(predict(model, user_rrm, type = "ratings"), "matrix")
    pref_scores <- round(as.numeric(score_mat[1, rec_ids]), 2)
    
    # Build output table: recommended book IDs + metadata + scores
    out <- tibble(book_id = rec_ids, preference_score = pref_scores) |>
      left_join(book_lookup, by = "book_id") |>
      select(book_title, author, preference_score, book_id)
    
    # Optionally hide book IDs if the user unchecked "show_ids"
    if (!input$show_ids) out <- out |> select(-book_id)
    
    # Return 
    out
  })
  
  # User lookup tab
  observeEvent(input$lookup_btn, {
    user_row <- users_clean |> filter(user_id == input$lookup_id)
    if (nrow(user_row) == 0) {
      output$user_info <- renderTable(data.frame(Message = "User not found"))
    } else {
      output$user_info <- renderTable(user_row)
    }
  })
}

shinyApp(ui, server)
