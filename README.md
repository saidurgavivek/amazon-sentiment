# Implement in R — Project 2: Amazon Fine Food Reviews Sentiment

1. Install packages: `install.packages(c("tidyverse","tidytext","textdata","wordcloud","tm"))`
2. Download the dataset (see the project's top-level README), subsample to ~5k-10k rows, and place `amazon_fine_food_reviews.csv` in this folder.
3. Open `analysis.R`. Every function except `load_data` and `run_pipeline` calls `stop("Not implemented: ...")` — fill in the body of each, following its roxygen comment. Do not change any function signature.
4. Run it: `source("analysis.R"); run_pipeline("amazon_fine_food_reviews.csv")`. It will stop at the first unimplemented function — that's expected. Keep filling functions in order (`clean_data` → `engineer_features` → `select_features` → the `plot_*` functions) until `run_pipeline` completes without error.
5. The optional `train_baseline_model` / `evaluate_model` stretch functions are not required for grading.
