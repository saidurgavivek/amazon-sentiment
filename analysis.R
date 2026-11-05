# analysis.R — Project 2: Amazon Fine Food Reviews Sentiment
library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(tm)

load_data <- function(path) {
  read_csv(path)
}

clean_data <- function(df) {
  df <- df %>%
    filter(!is.na(Text), !is.na(Score), str_trim(Text) != "") %>%
    distinct(UserId, ProductId, Time, Text, .keep_all = TRUE) %>%
    mutate(
      Text = iconv(Text, to = "UTF-8", sub = ""),
      Text = str_replace_all(Text, "<[^>]+>", " "),
      Text = str_squish(Text)
    ) %>%
    filter(Text != "")
  
  # Time is a Unix timestamp (seconds) in the Kaggle file -> Date
  if (!inherits(df$Time, "Date")) {
    df <- df %>%
      mutate(Time = as.Date(as.POSIXct(as.numeric(Time),
                                       origin = "1970-01-01", tz = "UTC")))
  }
  df
}

engineer_features <- function(df) {
  df %>%
    mutate(
      review_length = str_count(Text, "\\S+"),
      helpfulness_ratio = if_else(HelpfulnessDenominator > 0,
                                  HelpfulnessNumerator / HelpfulnessDenominator,
                                  0),
      sentiment_label = case_when(
        Score >= 4 ~ "positive",
        Score <= 2 ~ "negative",
        TRUE       ~ NA_character_
      )
    )
}

select_features <- function(df, target = "sentiment_label") {
  numeric_cols <- df %>% select(where(is.numeric)) %>% names()
  setdiff(numeric_cols, c("Id", "Score", target))
}

plot_score_distribution <- function(df) {
  p <- df %>%
    count(Score) %>%
    ggplot(aes(x = factor(Score), y = n)) +
    geom_col(fill = "#4C78A8") +
    labs(title = "Distribution of star ratings",
         x = "Star rating", y = "Number of reviews") +
    theme_minimal()
  print(p)
  invisible(NULL)
}

plot_review_length_distribution <- function(df) {
  if (!"review_length" %in% names(df)) {
    df <- df %>% mutate(review_length = str_count(Text, "\\S+"))
  }
  p <- df %>%
    ggplot(aes(x = review_length)) +
    geom_histogram(bins = 50, fill = "#F58518", colour = "white") +
    coord_cartesian(xlim = c(0, quantile(df$review_length, 0.99))) +
    labs(title = "Review length distribution",
         x = "Words per review", y = "Number of reviews") +
    theme_minimal()
  print(p)
  invisible(NULL)
}

plot_wordcloud_by_sentiment <- function(df) {
  top_words <- function(d) {
    d %>%
      select(Text) %>%
      unnest_tokens(word, Text) %>%
      filter(str_detect(word, "^[a-z]+$"), nchar(word) > 2) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      slice_head(n = 100)
  }
  pos <- top_words(filter(df, Score >= 4))
  neg <- top_words(filter(df, Score <= 2))
  
  old_par <- par(mfrow = c(1, 2), mar = c(0, 0, 2, 0))
  on.exit(par(old_par), add = TRUE)
  
  wordcloud(pos$word, pos$n, max.words = 100, random.order = FALSE,
            scale = c(3, 0.5), colors = brewer.pal(8, "Greens")[4:8])
  title("Positive reviews (score >= 4)")
  
  wordcloud(neg$word, neg$n, max.words = 100, random.order = FALSE,
            scale = c(3, 0.5), colors = brewer.pal(8, "Reds")[4:8])
  title("Negative reviews (score <= 2)")
  invisible(NULL)
}

plot_sentiment_over_time <- function(df) {
  trend <- df %>%
    filter(!is.na(Time)) %>%
    mutate(period = lubridate::floor_date(Time, "quarter")) %>%
    group_by(period) %>%
    summarise(`Number of reviews` = n(),
              `Mean star rating`  = mean(Score),
              .groups = "drop") %>%
    pivot_longer(-period, names_to = "metric", values_to = "value")
  
  p <- ggplot(trend, aes(x = period, y = value)) +
    geom_line(colour = "#54A24B") +
    geom_point(colour = "#54A24B", size = 1) +
    facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    labs(title = "Review volume and sentiment over time",
         x = "Quarter", y = NULL) +
    theme_minimal()
  print(p)
  invisible(NULL)
}

train_baseline_model <- function(df, features, target = "sentiment_label") {
  stop("Not implemented: train_baseline_model")
}

evaluate_model <- function(model, df, features, target = "sentiment_label") {
  stop("Not implemented: evaluate_model")
}

run_pipeline <- function(path) {
  df <- load_data(path)
  df <- clean_data(df)
  df <- engineer_features(df)
  features <- select_features(df)
  plot_score_distribution(df)
  plot_review_length_distribution(df)
  plot_wordcloud_by_sentiment(df)
  plot_sentiment_over_time(df)
  invisible(df)
}