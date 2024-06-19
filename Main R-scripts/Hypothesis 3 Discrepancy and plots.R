#-------------------------------------------------------------------------
## Hypothesis 3
#-------------------------------------------------------------------------

rm(list = ls())
cat("\f")

library(tm)
library(lme4)
library(httr)
library(textrank)
library(tidyverse)
library(textstem)
library(tidytext)
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(MuMIn)
library(ggplot2)
library(reshape2)
library(latticeExtra)

dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)
dirData <- file.path(dir, "Data")
dirRslt <- file.path(dir, "Results")

MERGEDDATA_B <- read.csv(paste0(dirData,"/finaldataformmultiB.csv"))
MERGEDDATA_B$house_type_binary <- ifelse(MERGEDDATA_B$house_type == "huis", 1, 0)
MERGEDDATA_B$energy_labelAA <- MERGEDDATA_B$energy_label_.A.
data<-MERGEDDATA_B


data <- data %>%na.omit()
sample_data <- read.csv("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/500obs.csv")
sample_data$gptef <- as.numeric(substr(sample_data$gptef, 1, 1))

ratings_df <- data.frame(rating = sample_data$gptef)

# Plot the distribution of the ratings
ggplot2::ggplot(ratings_df, aes(x = rating)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", boundary = 0.5)+
  labs(x = "Rating",
       y = "Frequency") +
  theme_minimal()

merged_data <- merge(data, sample_data, by = "description", all = TRUE)
merged_data_clean <- merged_data[!is.na(merged_data$gptef) & merged_data$gptef != 0, ]
merged_data_clean <- merged_data_clean[merged_data_clean$energy_label != "na", ]
merged_data_clean <- merged_data_clean[!is.na(merged_data_clean$energy_label), ]

#-------------------------------------------------------------------------
#Discrepancy Calculation
#-------------------------------------------------------------------------

# Define a function to convert energy labels to numeric values from 1 to 5
convert_label_to_numeric <- function(label) {
  if (label == "A" || label == ">A+") {
    return(5)
  } else if (label == "B") {
    return(4)
  } else if (label == "C") {
    return(3)
  } else if (label == "D") {
    return(2)
  } else if (label == "E" || label == "F" || label == "G") {
    return(1)
  } else {
    return(NA)
  }
}

merged_data_clean$energy_label_numeric <- sapply(merged_data_clean$energy_label, convert_label_to_numeric)

merged_data_clean$gptef <- as.numeric(merged_data_clean$gptef)
merged_data_clean$gptef <- pmax(pmin(merged_data_clean$gptef, 5), 1)

threshold <- 2
merged_data_clean$discrepancy <- abs(merged_data_clean$energy_label_numeric - merged_data_clean$gptef) >= threshold
merged_data_clean$ln_Pi<-log(merged_data_clean$price)

merged_data_clean$discrepancy_factor <- factor(merged_data_clean$discrepancy, 
                                               labels = c("No Discrepancy", "Discrepancy"))

no_discrepancy_data <- merged_data_clean[merged_data_clean$discrepancy == FALSE, ]
expected_price_model <- lm(ln_Pi ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area +
                             house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation +
                             Double_Glazing + energy_labelAA + energy_label_A + energy_label_B +
                             energy_label_C + energy_label_E + energy_label_F + energy_label_G +
                             sted + logwoz + Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde +
                             Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km +
                             Voortgezet.onderwijs...aantal.binnen.3.km, data = no_discrepancy_data)
summary(expected_price_model)


# Perform a t-test to compare house prices between properties with and without discrepancies
t_test_price <- t.test(ln_Pi ~ discrepancy, data = merged_data_clean)
t_test_price



#-------------------------------------------------------------------------
## Matching dataset
#-------------------------------------------------------------------------
library(MatchIt)

# Define the formula for the propensity score model
psm_formula <- discrepancy ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area +
  house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation +
  Double_Glazing + energy_labelAA + energy_label_A + energy_label_B +
  energy_label_C + energy_label_E + energy_label_F + energy_label_G +
  sted + logwoz + Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde +
  Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km +
  Voortgezet.onderwijs...aantal.binnen.3.km

matched_data <- matchit(psm_formula, data = merged_data_clean, method = "nearest", ratio = 1)
summary(matched_data)

matched_data_df <- match.data(matched_data)

# Perform a t-test on the matched data to compare house prices
t_test_matched_price <- t.test(ln_Pi ~ discrepancy, data = matched_data_df)
t_test_matched_price

#-------------------------------------------------------------------------
##Plots
#-------------------------------------------------------------------------
# Histogram of propensity scores before and after matching
pdf(file.path(dirRslt, "Propensity_Score_Distribution.pdf"))
par(mfrow = c(1, 2))
plot(density(matched_data$distance), main = "Before Matching", xlab = "Propensity Score")
plot(density(matched_data_df$distance), main = "After Matching", xlab = "Propensity Score")
dev.off()
# Bar plot of discrepancies with legend
pdf(file.path(dirRslt, "Bar_plot_of_discrepancies_with_legend.pdf"))
ggplot(merged_data_clean, aes(x = discrepancy_factor, fill = discrepancy_factor)) +
  geom_bar(color = "black") +
  labs(x = "Discrepancy",
       y = "Count",
       fill = "Discrepancy") +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme_minimal()
dev.off()


pdf(file.path(dirRslt, "House_Prices_Discrepancy_Matched.pdf"))
ggplot(matched_data_df, aes(x = discrepancy_factor, y = ln_Pi, fill = discrepancy_factor)) +
  geom_boxplot(color = "black") +
  labs(x = "Discrepancy",
       y = "House Price",
       fill = "Discrepancy") +
  scale_fill_manual(values = c("lightblue", "darkblue")) +
  theme_minimal()
dev.off()

pdf(file.path(dirRslt, "Propensity_Score_Distribution.pdf"))
par(mfrow = c(1, 2))
plot(density(matched_data$distance), main = "Before Matching", xlab = "Propensity Score")
plot(density(matched_data_df$distance), main = "After Matching", xlab = "Propensity Score")
dev.off()

