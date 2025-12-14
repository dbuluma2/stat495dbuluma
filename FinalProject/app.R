library(shiny)
library(tidyverse)
library(Matrix)
library(recommenderlab)

# -----------------------------
# Build rating_matrix + book lookup (runs once when app starts)
# -----------------------------
books   <- read.csv("Data/books.csv")
ratings <- read.csv("Data/ratings.csv")
users   <- read.csv("Data/users.csv")

books_clean <- books |>
  select(ISBN, Book.Title, Book.Author) |>
  rename(book_id = ISBN, book_title = Book.Title, author = Book.Author) |>
  mutate(book_id = as.character(book_id)) |>
  distinct(book_id, .keep_all = TRUE)

ratings_clean <- ratings |>
  rename(user_id = User.ID, book_id = ISBN, book_rating = Book.Rating) |>
  mutate(
    user_id = as.character(user_id),
    book_id = as.character(book_id)
  ) |>
  filter(book_rating > 0)

users_clean <- users |>
  rename(user_id = User.ID) |>
  mutate(user_id = as.character(user_id)) |>
  distinct(user_id, .keep_all = TRUE)

ratings_full <- ratings_clean |>
  inner_join(books_clean, by = "book_id") |>
  inner_join(users_clean, by = "user_id")

# Thresholds
min_user_ratings <- 10
min_item_ratings <- 10

user_counts <- ratings_full |> count(user_id, name = "n_user")
item_counts <- ratings_full |> count(book_id, name = "n_item")

ratings_filtered <- ratings_full |>
  inner_join(user_counts |> filter(n_user >= min_user_ratings), by = "user_id") |>
  inner_join(item_counts |> filter(n_item >= min_item_ratings), by = "book_id")

# Build sparse user–item matrix
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

book_lookup <- ratings_filtered |>
  select(book_id, book_title, author) |>
  distinct(book_id, .keep_all = TRUE)

# -----------------------------
# Shiny UI with tabs
# -----------------------------
ui <- fluidPage(
  titlePanel("Interactive kNN Book Recommender (Demo)"),
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
                 tableOutput("recs"),
                 br(),
                 h4("Debug / sanity checks"),
                 verbatimTextOutput("debug")
               )
             )
    ),
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

# -----------------------------
# Shiny server
# -----------------------------
server <- function(input, output, session) {
  
  model_reactive <- reactive({
    Recommender(
      rating_matrix,
      method = "UBCF",
      parameter = list(nn = input$k, method = "Cosine", normalize = "center")
    )
  })
  
  output$recs <- renderTable({
    req(input$user, input$n)
    if (!input$user %in% rownames(rating_matrix)) {
      return(data.frame(Message = "Selected user not found in matrix."))
    }
    model <- model_reactive()
    user_rrm <- rating_matrix[input$user, , drop = FALSE]
    recs <- predict(model, user_rrm, n = input$n)
    rec_ids <- as(recs, "list")[[1]]
    if (length(rec_ids) == 0) {
      return(data.frame(Message = "No recommendations returned."))
    }
    score_mat <- as(predict(model, user_rrm, type = "ratings"), "matrix")
    pref_scores <- round(as.numeric(score_mat[1, rec_ids]), 2)
    out <- tibble(book_id = rec_ids, preference_score = pref_scores) |>
      left_join(book_lookup, by = "book_id") |>
      select(book_title, author, preference_score, book_id)
    if (!input$show_ids) out <- out |> select(-book_id)
    out
  })
  
  output$debug <- renderPrint({
    list(
      users = nrow(rating_matrix),
      books = ncol(rating_matrix),
      selected_user = input$user,
      k = input$k
    )
  })
  
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
