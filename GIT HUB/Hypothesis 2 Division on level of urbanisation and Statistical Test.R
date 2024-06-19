#-------------------------------------------------------------------------
## Hypothesis 2
#-------------------------------------------------------------------------

library(psych)
library(car)
library(dplyr)
library(tidyr)
library(MatchIt)
library(ggplot2)

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
  match <- matchit(formula, data = data, method = "nearest")
  matched_data <- match.data(match)
  return(matched_data)
}


match_vars <- c("house_type_binary", "bedroom", "bathroom", "living_area", "house_age", 
                "garage_binary", "Fully_insulated", "Roof_Wall_Insulation", "Double_Glazing", 
                "logwoz", "Treinstation...dichtstbijzijnde", "Overstapstation.trein...dichtstbijzijnde", 
                "Oprit...dichtstbijzijnde", "totalegroenenstemmen", "Cafe...aantal.binnen.3.km", 
                "Warenhuis..aantal..binnen.5.km", "Hotel..aantal.binnen.5.km", "Voortgezet.onderwijs...aantal.binnen.3.km")

# Perform matching for urban and rural areas
matched_urban_data <- perform_matching(urban_data, match_vars, "label_category")
matched_rural_data <- perform_matching(rural_data, match_vars, "label_category")

# Run regression models on matched data
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

# Calculate the z-value for the difference in coefficients
z_value <- (coef_urban - coef_rural) / sqrt(se_urban^2 + se_rural^2)

# Calculate the p-value
p_value <- 2 * (1 - pnorm(abs(z_value)))
z_value
p_value
# Run separate regression models
model_urban <- lm(Log_price ~ house_type_binary + bedroom + bathroom + living_area + house_age + 
                    Fully_insulated + Roof_Wall_Insulation + Double_Glazing + label_category + logwoz + 
                    Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde + 
                    Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + 
                    Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km + Voortgezet.onderwijs...aantal.binnen.3.km, 
                  data = urban_data)

model_rural <- lm(Log_price ~ house_type_binary + bedroom + bathroom + living_area + house_age + 
                    Fully_insulated + Roof_Wall_Insulation + Double_Glazing + label_category + logwoz + 
                    Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde + 
                    Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + 
                    Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km + Voortgezet.onderwijs...aantal.binnen.3.km, 
                  data = rural_data)

coef_urban_matched <- summary(model_urban_matched)$coefficients["label_category", "Estimate"]
se_urban_matched <- summary(model_urban_matched)$coefficients["label_category", "Std. Error"]
coef_rural_matched <- summary(model_rural_matched)$coefficients["label_category", "Estimate"]
se_rural_matched <- summary(model_rural_matched)$coefficients["label_category", "Std. Error"]

z_value_matched <- (coef_urban_matched - coef_rural_matched) / sqrt(se_urban_matched^2 + se_rural_matched^2)
p_value_matched <- 2 * (1 - pnorm(abs(z_value_matched)))

coef_urban_unmatched <- summary(model_urban)$coefficients["label_category", "Estimate"]
se_urban_unmatched <- summary(model_urban)$coefficients["label_category", "Std. Error"]
coef_rural_unmatched <- summary(model_rural)$coefficients["label_category", "Estimate"]
se_rural_unmatched <- summary(model_rural)$coefficients["label_category", "Std. Error"]

z_value_unmatched <- (coef_urban_unmatched - coef_rural_unmatched) / sqrt(se_urban_unmatched^2 + se_rural_unmatched^2)
p_value_unmatched <- 2 * (1 - pnorm(abs(z_value_unmatched)))

# Print the results for unmatched data
z_value_unmatched
p_value_unmatched
# Print the results for matched data
z_value_matched
p_value_matched
