#-------------------------------------------------------------------------
## Hypothesis 2
#-------------------------------------------------------------------------

library(psych)
library(car)
library(dplyr)
library(tidyr)
library(MatchIt)
library(ggplot2)
library(optmatch)

remove(list=ls())
cat("\f")

dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)

dirData <- file.path(dir, "Data")
dirProg <- file.path(dir, "Programs")
dirRslt <- file.path(dir, "Results")

MERGEDDATA_B <- read.csv(file.path(dirData, "finaldataformmultiB.csv"))
MERGEDDATA_B$house_type_binary <- ifelse(MERGEDDATA_B$house_type == "huis", 1, 0)
MERGEDDATA_B$energy_labelAA <- MERGEDDATA_B$energy_label_.A.

MERGEDDATA_B$urban <- ifelse(MERGEDDATA_B$sted %in% c(1), 1, 0)
MERGEDDATA_B <- MERGEDDATA_B[complete.cases(MERGEDDATA_B), ]

MERGEDDATA_B$Log_price <- log(MERGEDDATA_B$price)

good_labels <- c("A", ">A+","B")
bad_labels <- c("C", "D", "E", "F", "G")
MERGEDDATA_B$label_category <- ifelse(MERGEDDATA_B$energy_label %in% good_labels, 1, 0)

urban_data <- subset(MERGEDDATA_B, urban == 1)
rural_data <- subset(MERGEDDATA_B, urban == 0)

perform_matching <- function(data, match_vars, treatment_var) {
  formula <- as.formula(paste(treatment_var, "~", paste(match_vars, collapse = " + ")))
  match <- matchit(formula, data = data, method = "full")
  matched_data <- match.data(match)
  return(matched_data)
}


match_vars <- c("Log_price","house_type_binary", "bedroom", "bathroom", "living_area", "house_age", 
                "garage_binary", "Fully_insulated", "Roof_Wall_Insulation", "Double_Glazing", 
                "logwoz", "Treinstation...dichtstbijzijnde", "Overstapstation.trein...dichtstbijzijnde", 
                "Oprit...dichtstbijzijnde", "totalegroenenstemmen", "Cafe...aantal.binnen.3.km", 
                "Warenhuis..aantal..binnen.5.km", "Hotel..aantal.binnen.5.km", "Voortgezet.onderwijs...aantal.binnen.3.km")

matched_urban_data <- perform_matching(urban_data, match_vars, "label_category")
matched_rural_data <- perform_matching(rural_data, match_vars, "label_category")

model_urban_matched <- lm(Log_price ~ house_type_binary + bedroom + bathroom + living_area + house_age + 
                            Fully_insulated + Roof_Wall_Insulation + Double_Glazing + label_category + logwoz + Treinstation...dichtstbijzijnde + 
                            Overstapstation.trein...dichtstbijzijnde + Oprit...dichtstbijzijnde + 
                            totalegroenenstemmen + Cafe...aantal.binnen.3.km + Warenhuis..aantal..binnen.5.km + 
                            Hotel..aantal.binnen.5.km + Voortgezet.onderwijs...aantal.binnen.3.km, 
                          data = matched_urban_data)

model_rural_matched <- lm(Log_price ~ house_type_binary + bedroom + bathroom + living_area + house_age + 
                            Fully_insulated + Roof_Wall_Insulation + Double_Glazing +label_category+ logwoz + Treinstation...dichtstbijzijnde + 
                            Overstapstation.trein...dichtstbijzijnde + Oprit...dichtstbijzijnde + 
                            totalegroenenstemmen + Cafe...aantal.binnen.3.km + Warenhuis..aantal..binnen.5.km + 
                            Hotel..aantal.binnen.5.km + Voortgezet.onderwijs...aantal.binnen.3.km, 
                          data = matched_rural_data)

# Display summary of the models
summary(model_urban_matched)
summary(model_rural_matched)
model_urban_matched$coefficients["label_category"]
model_rural_matched$coefficients["label_category"]

coef_urban <- summary(model_urban_matched)$coefficients["label_category", "Estimate"]
se_urban <- summary(model_urban_matched)$coefficients["label_category", "Std. Error"]

coef_rural <- summary(model_rural_matched)$coefficients["label_category", "Estimate"]
se_rural <- summary(model_rural_matched)$coefficients["label_category", "Std. Error"]

#statistical test to determine if the difference is significant
diff_coef <- coef_urban - coef_rural
se_diff <- sqrt(se_urban^2 + se_rural^2)
z_score <- diff_coef / se_diff
z_score
p_value <- 2 * (1 - pnorm(abs(z_score)))
p_value

## interaction model
# Combine urban and rural datasets
combined_data <- rbind(matched_urban_data, matched_rural_data)
combined_data$urban <- ifelse(combined_data$urban == 1, "Urban", "Rural")

# Run a combined regression model with interaction term
interaction_model <- lm(Log_price ~ label_category * urban + house_type_binary + bedroom + bathroom + living_area + 
                          house_age + Fully_insulated + Roof_Wall_Insulation + Double_Glazing + logwoz + 
                          Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde + 
                          Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + 
                          Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km + 
                          Voortgezet.onderwijs...aantal.binnen.3.km, data = combined_data)

summary(interaction_model)

# Ensure that label_category is a factor
matched_urban_data$label_category <- as.factor(matched_urban_data$label_category)
matched_rural_data$label_category <- as.factor(matched_rural_data$label_category)

## Balance plots
urban_save_dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Results/Balance plots/Urban"
rural_save_dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Results/Balance plots/Rural"
# Function to plot density plots for each covariate with customization
plot_density_custom <- function(data, covariate, treatment, title, save_path) {
  p <- ggplot(data, aes_string(x = covariate, fill = treatment)) +
    geom_density(alpha = 0.5) +
    labs(title = title, x = covariate, fill = treatment) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  ggsave(filename = save_path, plot = p, width = 8, height = 6)
  print(p)
}

# Create density plots for all covariates in urban matched data
for (var in match_vars) {
  plot_density_custom(matched_urban_data, var, "label_category", paste("Density plot of", var, "in Urban Data"),
                      file.path(urban_save_dir, paste0("density_plot_", var, "_Urban.png")))
}

# Create density plots for all covariates in rural matched data
for (var in match_vars) {
  plot_density_custom(matched_rural_data, var, "label_category", paste("Density plot of", var, "in Rural Data"),
                      file.path(rural_save_dir, paste0("density_plot_", var, "_Rural.png")))
}

