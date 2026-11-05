# app.R — Shiny dashboard: Amazon Fine Food Reviews
# Self-contained: does not need analysis.R.
# Put this file in the same folder as amazon_fine_food_reviews.csv, then click "Run App".

library(shiny)
library(tidyverse)
library(tidytext)

DATA_PATH <- "amazon_fine_food_reviews.csv"

COL_POS <- "#2f7d4f"
COL_NEU <- "#b9bfb3"
COL_NEG <- "#c8472f"
SENT_COLS <- c(Positive = COL_POS, Neutral = COL_NEU, Negative = COL_NEG)

# ---- data -------------------------------------------------------------------
load_reviews <- function(path) {
  df <- read_csv(path, show_col_types = FALSE) %>%
    filter(!is.na(Text), !is.na(Score), str_trim(Text) != "") %>%
    distinct(UserId, ProductId, Time, Text, .keep_all = TRUE) %>%
    mutate(
      Text = iconv(Text, to = "UTF-8", sub = ""),
      Text = str_replace_all(Text, "<[^>]+>", " "),
      Text = str_squish(Text)
    ) %>%
    filter(Text != "")

  # Time is a Unix timestamp (seconds) in the Kaggle file
  if (!inherits(df$Time, "Date")) {
    df <- df %>%
      mutate(Time = as.Date(as.POSIXct(as.numeric(Time),
                                       origin = "1970-01-01", tz = "UTC")))
  }

  df %>%
    mutate(
      review_length = str_count(Text, "\\S+"),
      sentiment = case_when(Score >= 4 ~ "Positive",
                            Score <= 2 ~ "Negative",
                            TRUE       ~ "Neutral"),
      sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative")),
      year = lubridate::year(Time)
    )
}

reviews <- if (file.exists(DATA_PATH)) load_reviews(DATA_PATH) else NULL
yr <- if (!is.null(reviews)) range(reviews$year, na.rm = TRUE) else c(2000, 2012)

top_words <- function(d, n = 12) {
  d %>%
    select(Text) %>%
    unnest_tokens(word, Text) %>%
    filter(str_detect(word, "^[a-z]+$"), nchar(word) > 2) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% c("br", "href", "http", "www", "com")) %>%
    count(word, sort = TRUE) %>%
    slice_head(n = n) %>%
    mutate(word = fct_reorder(word, n))
}

# ---- UI ---------------------------------------------------------------------
kpi <- function(value, label) {
  column(3, div(style = "margin-bottom:18px;",
                div(style = "font-size:30px;font-weight:700;line-height:1.1;", value),
                div(style = "color:#5d6b63;", label)))
}

ui <- fluidPage(
  titlePanel("Amazon Fine Food Reviews"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("stars", "Star rating", min = 1, max = 5, value = c(1, 5), step = 1),
      sliderInput("years", "Year", min = yr[1], max = yr[2], value = yr,
                  step = 1, sep = ""),
      helpText("Charts update when you move a slider.")
    ),
    mainPanel(
      width = 9,
      uiOutput("kpis"),
      tabsetPanel(
        tabPanel("Ratings and length",
                 fluidRow(column(6, plotOutput("p_score", height = 320)),
                          column(6, plotOutput("p_length", height = 320)))),
        tabPanel("Words",
                 fluidRow(column(6, plotOutput("p_pos", height = 380)),
                          column(6, plotOutput("p_neg", height = 380)))),
        tabPanel("Over time", plotOutput("p_time", height = 460)),
        tabPanel("Data", br(), tableOutput("tbl"))
      )
    )
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  filtered <- reactive({
    validate(need(!is.null(reviews),
                  paste0("Could not find ", DATA_PATH,
                         ". Put it in the same folder as app.R and run the app again.")))
    d <- reviews %>%
      filter(Score >= input$stars[1], Score <= input$stars[2],
             year >= input$years[1], year <= input$years[2])
    validate(need(nrow(d) > 0, "No reviews match these filters."))
    d
  })

  output$kpis <- renderUI({
    d <- filtered()
    fluidRow(
      kpi(format(nrow(d), big.mark = ","), "reviews shown"),
      kpi(sprintf("%.2f", mean(d$Score)), "average star rating"),
      kpi(sprintf("%.0f%%", mean(d$Score >= 4) * 100), "positive (4-5 stars)"),
      kpi(median(d$review_length), "median words per review")
    )
  })

  output$p_score <- renderPlot({
    filtered() %>%
      count(Score) %>%
      complete(Score = 1:5, fill = list(n = 0L)) %>%
      mutate(sentiment = case_when(Score >= 4 ~ "Positive",
                                   Score <= 2 ~ "Negative",
                                   TRUE       ~ "Neutral")) %>%
      ggplot(aes(factor(Score), n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = SENT_COLS) +
      labs(title = "Star ratings", x = "Stars", y = "Reviews") +
      theme_minimal(base_size = 14)
  })

  output$p_length <- renderPlot({
    d <- filtered()
    ggplot(d, aes(review_length)) +
      geom_histogram(bins = 40, fill = "#4b7f8c", colour = "white") +
      coord_cartesian(xlim = c(0, quantile(d$review_length, 0.99))) +
      labs(title = "Review length", x = "Words per review (top 1% hidden)",
           y = "Reviews") +
      theme_minimal(base_size = 14)
  })

  word_plot <- function(d, colour, title) {
    validate(need(nrow(d) > 0, "No reviews in this group for the current filters."))
    top_words(d) %>%
      ggplot(aes(n, word)) +
      geom_col(fill = colour) +
      labs(title = title, x = "Mentions", y = NULL) +
      theme_minimal(base_size = 14)
  }
  output$p_pos <- renderPlot(word_plot(filter(filtered(), Score >= 4), COL_POS,
                                       "Top words in positive reviews"))
  output$p_neg <- renderPlot(word_plot(filter(filtered(), Score <= 2), COL_NEG,
                                       "Top words in negative reviews"))

  output$p_time <- renderPlot({
    filtered() %>%
      filter(!is.na(Time)) %>%
      mutate(period = lubridate::floor_date(Time, "quarter")) %>%
      group_by(period) %>%
      summarise(`Number of reviews` = n(),
                `Mean star rating` = mean(Score), .groups = "drop") %>%
      pivot_longer(-period, names_to = "metric", values_to = "value") %>%
      ggplot(aes(period, value)) +
      geom_line(colour = COL_POS) +
      geom_point(colour = COL_POS) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      labs(title = "Review volume and rating by quarter", x = NULL, y = NULL) +
      theme_minimal(base_size = 14)
  })

  output$tbl <- renderTable({
    filtered() %>%
      transmute(Date = format(Time), Stars = Score, Sentiment = as.character(sentiment),
                Words = review_length, Review = str_trunc(Text, 140)) %>%
      slice_head(n = 20)
  })
}

shinyApp(ui, server)
