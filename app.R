#############################################
# app.R  —  4-gram OPM predictor (Shiny)
#############################################

# ---------- packages ----------
required_pkgs <- c("shiny", "collapse", "jsonlite")
for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(shiny)
library(collapse)
library(jsonlite)

# ---------- load OPM ----------
source("code/OPM_nominal.R")  # <-

# ---------- paths to books ----------
BOOK_PATHS <- list(
  child   = "Books/The Adventures of Pinocchio.txt",
  student = "Books/University_Physics_Volume_1_txt.txt",
  adult   = "Books/Bram Stokers Dracula.txt"
)

# ---------- text cleaning & tokenising ----------
clean_and_tokenize <- function(txt) {
  txt <- tolower(txt)
  txt <- gsub("’", "'", txt)                        # normalize apostrophe
  
  # allow only letters, hyphen, apostrophe, spaces
  txt <- gsub("[^a-z\\s'-]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  
  tokens <- unlist(strsplit(txt, " "))
  tokens <- tokens[tokens != ""]
  
  # Remove tokens that contain no letters at all
  tokens <- tokens[grepl("[a-z]", tokens)]
  
  # Remove tokens that start with symbols like --word
  tokens <- tokens[grepl("^[a-z]", tokens)]
  
  tokens
}
# ---------- build 4-gram dataframe ----------
# Each row: p1 p2 p3 next
make_4gram_df <- function(tokens) {
  n <- length(tokens)
  if (n < 4) stop("Not enough tokens for 4-grams.")
  
  p1 <- tokens[1:(n-3)]
  p2 <- tokens[2:(n-2)]
  p3 <- tokens[3:(n-1)]
  next_word <- tokens[4:n]   # temp-var, ikke reserverte ord
  
  data.frame(
    p1 = p1,
    p2 = p2,
    p3 = p3,
    nextw = next_word,        # kolonnen heter fortsatt "next"
    stringsAsFactors = FALSE
  )
}

# ---------- frequency table for 4-grams ----------
make_freq_table <- function(df4) {
  collapse::fcount(df4)
}

# ---------- build OPM agent ----------
build_opm_agent <- function(df4) {
  meta <- guessmetadata(df4, file = NULL)  # teacher helper
  buildagent(metadata = meta, data = df4, savememory = TRUE)
}

# ============================================================
#           FIXED BOTANICAL TABLES
# ============================================================

# child botanical table (Pinocchio) – 
botanical_child <- data.frame(
  item = c(
    "cherry", "pear", "grapes", "fruit",
    "tree", "oak", "willow", "pine",
    "bush", "bramble", "brush",
    "cauliflower", "pepper", "squash",
    "corn", "wheat", "bean", "chick_pea",
    "grass", "straw", "leaves"
  ),
  count = c(
    8, 6, 3, 2,
    9, 7, 1, 1,
    2, 1, 1,
    1, 1, 1,
    1, 1, 1, 2,
    3, 4, 1
  ),
  stringsAsFactors = FALSE
)

# student botanical table (University Physics) – 
botanical_student <- data.frame(
  item  = c("phytoplankton",
            "apple",
            "corn",
            "coconut",
            "palm tree",
            "tree",
            "branch",
            "oak"),
  count = c(1,
            3,
            1,
            1,
            1,
            3,
            1,
            1),
  stringsAsFactors = FALSE
)

# adult botanical table (Dracula) – 
botanical_adult <- data.frame(
  item = c(
    "oleander","blossom","fruit","bush","ash","orange","tree","flower",
    "lily","rose","pine","fir","birch","oak","beech","thorn","elm",
    "juniper","cypress","furze bush","garlic","maize","plum",
    "egg-plant","corn","woods","forest","foliage","grass"
  ),
  count = c(
    1,2,1,7,73,2,48,28,
    3,45,20,205,1,8,1,3,0,
    2,1,1,21,1,3,
    1,36,44,6,1,8
  ),
  stringsAsFactors = FALSE
)

BOTANICAL_TABLES <- list(
  child = botanical_child,
  student = botanical_student,
  adult = botanical_adult
)

# ---------- probability that next is botanical ----------
botanical_probability <- function(prob_vec, botanical_table) {
  # prob_vec: named numeric vector of probs for next word
  # botanical_table: data.frame(item, count) from your constants
  
  botanical_words <- botanical_table$item
  in_vocab <- intersect(names(prob_vec), botanical_words)
  
  if (length(in_vocab) == 0) {
    return(list(p_botanical = 0, top_botanical = numeric(0)))
  }
  
  p_botanical <- sum(prob_vec[in_vocab])
  
  # show top botanical candidates among the next-word distribution
  top_botanical <- sort(prob_vec[in_vocab], decreasing = TRUE)
  
  list(p_botanical = p_botanical, top_botanical = top_botanical)
}

# ---------- load & train all corpora ----------
load_and_train_all <- function() {
  models <- list()
  
  for (grp in names(BOOK_PATHS)) {
    path <- BOOK_PATHS[[grp]]
    if (!file.exists(path)) stop(paste("Missing file:", path))
    
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
    tokens <- clean_and_tokenize(txt)
    
    df4 <- make_4gram_df(tokens)
    freq4 <- make_freq_table(df4)
    agent <- build_opm_agent(df4)
    
    # attaches  botanical list 
    botanical_tbl <- BOTANICAL_TABLES[[grp]]
    
    models[[grp]] <- list(
      tokens = tokens,
      df4 = df4,
      freq4 = freq4,
      agent = agent,
      botanical_tbl = botanical_tbl
    )
  }
  models
}

MODELS <- load_and_train_all()

# ============================================================
#                      SHINY APP
# ============================================================

ui <- fluidPage(
  titlePanel("OPM 4-gram Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Corpus:",
                  choices = c("child", "student", "adult"),
                  selected = "adult"),
      
      textInput("context", "Context (0–3 words):", value = ""),
      actionButton("predict", "Predict next word"),
      
      hr(),
      actionButton("accept_best", "Accept best word"),
      actionButton("accept_botanical", "Accept best botanical"),
      hr(),
      strong("Current text:"),
      verbatimTextOutput("current_text", placeholder = TRUE)
    ),
    
    mainPanel(
      h4("Top-3 next word predictions"),
      tableOutput("top3"),
      
      hr(),
      h4("Botanical probability"),
      textOutput("botanical_prob"),
      tableOutput("top_botanical")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    current = "",
    last_top3 = NULL,
    last_botanical = NULL
  )
  
  # Parse input context to up to 3 tokens
  get_context_tokens <- reactive({
    ctx <- clean_and_tokenize(input$context)
    if (length(ctx) > 3) ctx <- tail(ctx, 3)
    ctx
  })
  
  # Prediction event
  observeEvent(input$predict, {
    grp <- input$group
    model <- MODELS[[grp]]
    agent <- model$agent
    
    ctx <- get_context_tokens()
    
    predictor <- list()
    if (length(ctx) >= 1) predictor$p3 <- ctx[length(ctx)]     # closest word
    if (length(ctx) >= 2) predictor$p2 <- ctx[length(ctx)-1]
    if (length(ctx) >= 3) predictor$p1 <- ctx[length(ctx)-2]
    
    # infer next-word distribution
    probs_arr <- infer(agent, predictand = "nextw", predictor = predictor)
    probs_vec <- as.numeric(probs_arr)
    names(probs_vec) <- dimnames(probs_arr)[["nextw"]]
    
    # top-3 candidates
    top3 <- sort(probs_vec, decreasing = TRUE)[1:3]
    rv$last_top3 <- top3
    
    # botanical stats using  fixed table
    bt <- botanical_probability(probs_vec, model$botanical_tbl)
    rv$last_botanical <- bt
  })
  
  observeEvent(input$context, {
    rv$current <- input$context
  })
  
  output$top3 <- renderTable({
    req(rv$last_top3)
    data.frame(
      word = names(rv$last_top3),
      probability = round(as.numeric(rv$last_top3), 6)
    )
  })
  
  output$botanical_prob <- renderText({
    req(rv$last_botanical)
    paste0("P(next word is botanical) = ",
           round(rv$last_botanical$p_botanical, 6))
  })
  
  output$top_botanical <- renderTable({
    req(rv$last_botanical)
    tb <- rv$last_botanical$top_botanical
    if (length(tb) == 0) return(NULL)
    data.frame(
      botanical_word = names(tb),
      probability = round(as.numeric(tb), 6)
    )
  })
  
  # Accept best predicted word
  observeEvent(input$accept_best, {
    req(rv$last_top3)
    best <- names(rv$last_top3)[1]
    
    # append to existing prompt text
    old_txt <- input$context
    new_txt <- trimws(paste(old_txt, best))
    
    # update prompt + current text state
    updateTextInput(session, "context", value = new_txt)
    rv$current <- new_txt
  })
  
  # Accept best botanical word
  observeEvent(input$accept_botanical, {
    req(rv$last_botanical)
    tb <- rv$last_botanical$top_botanical
    if (length(tb) == 0) return()
    
    best_botanical <- names(tb)[1]
    
    old_txt <- input$context
    new_txt <- trimws(paste(old_txt, best_botanical))
    
    updateTextInput(session, "context", value = new_txt)
    rv$current <- new_txt
  })
  
  output$current_text <- renderText({
    rv$current
  })
}

shinyApp(ui, server)
