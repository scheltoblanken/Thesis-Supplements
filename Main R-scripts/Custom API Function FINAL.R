#---------------------------------------------------------------------
## Chat GTP API Function
#---------------------------------------------------------------------
rm(list = ls())
cat("\f")
#libraries
library(tm)
library(httr)
library(textrank)
library(tidyverse)
library(textstem)
library(tidytext)
library(caret)
library(randomForest)
library(gbm)
library(e1071)

dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)
dirData <- file.path(dir, "Data")
dirRslt <- file.path(dir, "Results")
my_API <- "#########" # this is deleted due to privacy reasons


FUNDA_RURAL <- read.csv(paste0(dirData, "/AFTER CLEANING/Funda/FUNDA_RURAL_CLEANED (2).csv"))
FUNDA_URBAN <- read.csv(paste0(dirData, "/AFTER CLEANING/Funda/FUNDA_URBAN_CLEANED (2).csv"))
data <- rbind(FUNDA_URBAN, FUNDA_RURAL)

#Custom CHAT GPT FUNCTION
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      temperature = 1,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(httr::content(chat_GPT_answer)$choices[[1]]$message$content)
}

#Function to detect if a description is in Dutch
is_dutch <- function(description) {
  english_words <- c("english translation", "English translation", "the", "THE")
  !any(grepl(paste(english_words, collapse = "|"), description))
}


dutch_subset <- subset(data, sapply(data$description, is_dutch))

# Sample 500 descriptions,
sample_descriptions <- head(dutch_subset$description, 500)
Energylabel <- head(dutch_subset$energy_label, 500)
sample_data <- data.frame(description = sample_descriptions, Energylabel = Energylabel, gptef = character(length(sample_descriptions)), sentiment = character(length(sample_descriptions)), features = character(length(sample_descriptions)), stringsAsFactors = FALSE)
sample_data <- read.csv("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/500obs.csv")
# Function to preprocess text
preprocess_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[[:punct:]]", " ", text)
  text <- gsub("\\d+", "", text)
  text <- removeWords(text, stopwords("dutch"))
  text <- stripWhitespace(text)
  return(text)
}
sample_descriptions <- sapply(sample_descriptions, preprocess_text)

# Hypothesis 3: Energy Efficiency Rating
for (i in seq_along(sample_descriptions)) {
  print(i)
  question <- "On a scale of 0 to 5, with 0 indicating inability to provide a score, and 1 to 5
                representing low to high energy efficiency, please rate the energy efficiency of
                the following property. Be strict, look for words that say something about the
                energy efficiency of the house. If a high energy label (A or A>) is mentioned
                give a score of 5. Look for words like energy label and efficiency. The response
                should start with a number. Here is the description:"
  text <- sample_descriptions[i]
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while (length(result) == 0) {
    result <- hey_chatGPT(concat)
  }
  sample_data$gptef[i] <- result
}

# Take only the first string from gpt and convert to a numeric
sample_data$gptef <- as.numeric(substr(sample_data$gptef, 1, 1))


# Create a document-term matrix
corpus <- Corpus(VectorSource(sample_descriptions))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("dutch"))
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm_with_scores <- as.data.frame(as.matrix(dtm))
dtm_with_scores$gptef <- sample_data$gptef
# Remove rows where gptef is 0 and unnecessary column
dtm_with_scores <- dtm_with_scores[dtm_with_scores$gptef != 0, ]
dtm_with_scores$X <- NULL
colnames(dtm_with_scores) <- make.names(colnames(dtm_with_scores))

# Train-test split
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(dtm_with_scores), 0.8 * nrow(dtm_with_scores))
train_data <- dtm_with_scores[train_indices, ]
test_data <- dtm_with_scores[-train_indices, ]

assess_performance <- function(predictions, actuals) {
  mae <- mean(abs(predictions - actuals))
  rmse <- sqrt(mean((predictions - actuals)^2))
  list(mae = mae, rmse = rmse)
}
calculate_accuracy <- function(predictions, actuals) {
  valid_indices <- !is.na(actuals)
  predictions <- predictions[valid_indices]
  actuals <- actuals[valid_indices]
  accuracy <- mean(predictions == actuals)
  
  return(accuracy)
}

calculate_r_squared <- function(predictions, actuals) {
  valid_indices <- !is.na(actuals)
  predictions <- predictions[valid_indices]
  actuals <- actuals[valid_indices]
  
  ss_total <- sum((actuals - mean(actuals))^2)
  ss_residual <- sum((actuals - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  return(r_squared)
}

test_data <- test_data[!is.na(test_data$gptef), ]

train_data <- na.omit(train_data)
# 1. Random Forest
rf_model <- randomForest(gptef ~ ., data = train_data)
rf_predictions <- predict(rf_model, newdata = test_data)
rf_performance <- assess_performance(rf_predictions, test_data$gptef)
rf_accuracy <- calculate_accuracy(rf_predictions, test_data$gptef)
rf_r_squared <- calculate_r_squared(rf_predictions, test_data$gptef)

# 2. Linear Regression
lm_model <- lm(gptef ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_predictions <- round(lm_predictions)  # Round to nearest integer for comparison
lm_performance <- assess_performance(lm_predictions, test_data$gptef)
lm_accuracy <- calculate_accuracy(lm_predictions, test_data$gptef)
lm_r_squared <- calculate_r_squared(lm_predictions, test_data$gptef)
# 3. Gradient Boosting Machines (GBM)
gbm_model <- gbm(gptef ~ ., data = train_data, distribution = "gaussian", n.trees = 100)
gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = 100)
gbm_predictions <- round(gbm_predictions)  # Round to nearest integer for comparison
gbm_performance <- assess_performance(gbm_predictions, test_data$gptef)
gbm_accuracy <- calculate_accuracy(gbm_predictions, test_data$gptef)
gbm_r_squared <- calculate_r_squared(gbm_predictions, test_data$gptef)

# Compare model performance
model_performance <- data.frame(
  Model = c("Random Forest", "Linear Regression", "GBM"),
  MAE = c(rf_performance$mae, lm_performance$mae, gbm_performance$mae),
  RMSE = c(rf_performance$rmse, lm_performance$rmse, gbm_performance$rmse),
  Accuracy = c(rf_accuracy, lm_accuracy, gbm_accuracy),
  R_Squared = c(rf_r_squared, lm_r_squared, gbm_r_squared)
)

print("Model Performance Comparison")
print(model_performance)
dirRslt <- file.path(dir, "Results")

# Plot and save Random Forest predictions
pdf(file.path(dirRslt, "rf_predictions_vs_actual.pdf"))
plot(rf_predictions, test_data$gptef, xlab = "Predicted", ylab = "Actual", main = "RF: Predicted vs. Actual", col = "blue")
abline(0, 1, col = "red")
dev.off()

# Plot and save Linear Regression predictions
pdf(file.path(dirRslt, "lm_predictions_vs_actual.pdf"))
plot(lm_predictions, test_data$gptef, xlab = "Predicted", ylab = "Actual", main = "LM: Predicted vs. Actual", col = "blue")
abline(0, 1, col = "red")
dev.off()

# Plot and save GBM predictions
pdf(file.path(dirRslt, "gbm_predictions_vs_actual.pdf"))
plot(gbm_predictions, test_data$gptef, xlab = "Predicted", ylab = "Actual", main = "GBM: Predicted vs. Actual", col = "blue")
abline(0, 1, col = "red")
dev.off()
