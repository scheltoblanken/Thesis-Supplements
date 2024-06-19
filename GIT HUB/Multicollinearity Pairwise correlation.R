
#--------------------------------------------------------
## Multicollinearity 
#--------------------------------------------------------
library(psych)
library(car)
library(lme4)
library(dplyr)
library(tidyr)
library(xtable)
library(randomForest)
library(MuMIn)
library(ggplot2)
library(reshape2)
library(latticeExtra)

remove(list=ls())
cat("\f")
dir             <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)
dirData         <- file.path(dir, "Data")
dirProg         <- file.path(dir, "Programs")
dirRslt         <- file.path(dir, "Results")
#--------------------------------------------------------
## Alle relevante data sets
#--------------------------------------------------------
#fundasets
FUNDA_RURAL     <-  read.csv(paste0(dirData,"/AFTER CLEANING/Funda/FUNDA_RURAL_CLEANED (3).csv"))
FUNDA_URBAN     <-  read.csv(paste0(dirData,"/AFTER CLEANING/Funda/FUNDA_URBAN_CLEANED (3).csv"))
mergeddata <- rbind(FUNDA_RURAL,FUNDA_URBAN)

mergeddata <- mergeddata %>%na.omit()

#Garage
score_garage <- function(data) {
  ifelse(grepl("garage", tolower(data)), 1, 0)}

# Apply the function to create the new binary column
mergeddata$garage_binary <- score_garage(mergeddata$garage)

unique(mergeddata$garage_binary)
mergeddata$House_Age <- 2025-mergeddata$year_built
mergeddata$House_Age[mergeddata$House_Age == 2025] <- NA
mergeddata$bedroom[mergeddata$bedroom == 0] <- NA
mergeddata$bathroom[mergeddata$bathroom == 0] <- NA

mergeddata <- mergeddata %>%na.omit()
#--------------------------------------------------------
## Descriptive Analytics of the FUNDA data
#--------------------------------------------------------
numerical_vars <- c("price", "price_m2", "room", "bedroom", "bathroom", "living_area",
                    "House_Age", "size", "Log_price")

# Extract categorical variables
categorical_vars <- c("house_type", "building_type", "garage_binary",
                      "energy_label_.A.", "energy_label_A", "energy_label_B", 
                      "energy_label_C", "energy_label_D", "energy_label_E", 
                      "energy_label_F", "energy_label_G", "Roof_Wall_Insulation", 
                      "Floor_Insulation", "Double_Glazing", "Fully_insulated", 
                      "is_huis")

#--------------------------------------------------------
## Simple regression model A1
#--------------------------------------------------------
mergeddata$ln_Pi <- log(mergeddata$price)
simple_modelA1 <- lm(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
                     + bathroom + living_area + size + house_age + garage_binary +
                       Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                       energy_label_.A.+ energy_label_A + energy_label_B +
                       energy_label_C + energy_label_E +
                       energy_label_F+ energy_label_G, data = mergeddata)
summary(simple_modelA1)


#--------------------------------------------------------
## Multi level regression model A2
#--------------------------------------------------------
zip_modelA2 <- lmer(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
                    + bathroom + living_area + size + house_age + garage_binary +
                      Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                      energy_label_.A.+ energy_label_A + energy_label_B +
                      energy_label_C + energy_label_E +
                      energy_label_F+ energy_label_G+ (1 | new_zip_code), data = mergeddata)


#--------------------------------------------------------
## Multi level regression model A2
#--------------------------------------------------------
zip_modelA3 <- lmer(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
                    + bathroom + living_area + size + house_age + garage_binary +
                      Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                      energy_label_.A.+ energy_label_A + energy_label_B +
                      energy_label_C + energy_label_E +
                      energy_label_F+ energy_label_G+ (1 | city), data = mergeddata)



## MODELS 6.3 MODEL B123
dir             <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)
dirData         <- file.path(dir, "Data")
dirProg         <- file.path(dir, "Programs")
dirRslt         <- file.path(dir, "Results")
#--------------------------------------------------------
## Alle relevante data sets
#--------------------------------------------------------
CBSRURAL    <-  read.csv(paste0(dirData,"/AFTER CLEANING/CBS GROEN STEMMERS/CBSGREEN_RURAL_CLEANED (3).csv"))
CBSURBAN     <-  read.csv(paste0(dirData,"/AFTER CLEANING/CBS GROEN STEMMERS/CBSGREEN_URBAN_CLEANED (3).csv"))
CBSRURAL$sted <- CBSRURAL$...130

x <- colnames(CBSRURAL)
y <- colnames(CBSURBAN)
missing_from_rural <- setdiff(colnames(CBSURBAN), colnames(CBSRURAL))
missing_from_urban <- setdiff(colnames(CBSRURAL), colnames(CBSURBAN))
CBSRURAL_clean <- CBSRURAL[, !colnames(CBSRURAL) %in% missing_from_rural]
CBSURBAN_clean <- CBSURBAN[, !colnames(CBSURBAN) %in% missing_from_urban]
CBSRURAL_clean <- CBSRURAL_clean[, !colnames(CBSRURAL_clean) %in% c("clean_postal_code.x", "clean_postal_code.y", "...130")]
CBSURBAN_clean <- CBSURBAN_clean[, !colnames(CBSURBAN_clean) %in% "clean_postal_code"]
mergeddata <- rbind(CBSURBAN_clean, CBSRURAL_clean)
score_garage <- function(data) {
  ifelse(grepl("garage", tolower(data)), 1, 0)}
mergeddata$garage_binary <- score_garage(mergeddata$garage)

unique(mergeddata$garage_binary)
mergeddata$House_Age <- 2025-mergeddata$year_built
mergeddata$House_Age[mergeddata$House_Age == 2025] <- NA
mergeddata$bedroom[mergeddata$bedroom == 0] <- NA
mergeddata$bathroom[mergeddata$bathroom == 0] <- NA
mergeddata$omgevingsadressendichtheid<- mergeddata$...129
mergeddata <- mergeddata %>%na.omit()
mergeddata$logwoz<-log(mergeddata$WOZ.waarde.woning)
mergeddata$loginkom <-log(mergeddata$Gemiddeld..huishoudinkomen)
#--------------------------------------------------------
## Descriptive Analytics of the FUNDA data
#--------------------------------------------------------
numerical_vars <- c("price", "price_m2", "room", "bedroom", "bathroom", "living_area",
                    "House_Age", "size", "Log_price",
                    "sted", "omgevingsadressendichtheid", "loginkom", 
                    "logwoz", "Treinstation...dichtstbijzijnde", 
                    "Overstapstation.trein...dichtstbijzijnde", "Oprit...dichtstbijzijnde", 
                    "totalegroenenstemmen", "Huisartspraktijk...aantal.binnen.3.km", 
                    "Supermarkt..aantal.binnen.3.km", "Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km", 
                    "Dagelijkse.levensmiddelen..aantal.binnen.3.km", "Cafe...aantal.binnen.3.km", 
                    "Warenhuis..aantal..binnen.5.km", "Cafetaria...aantal.binnen.3.km", 
                    "Restaurant...aantal.binnen.3.km", "Hotel..aantal.binnen.5.km", 
                    "Kinderdagverblijf...aantal.binnen.3.km", "Basis.onderwijs...aantal.binnen.3.km", 
                    "Voortgezet.onderwijs...aantal.binnen.3.km", "Musea...aantal.binnen.5.km")

mergeddata[numerical_vars] <- lapply(mergeddata[numerical_vars], function(x) {
  x[x == -99997] <- NA
  return(x)
})
cleaned_data <- mergeddata[complete.cases(mergeddata[numerical_vars]), ]
mergeddata <- cleaned_data

mergeddata$ln_Pi<-mergeddata$Log_price


# Extract categorical variables
categorical_vars <- c("house_type", "building_type", "garage_binary",
                      "energy_label_.A.", "energy_label_A", "energy_label_B", 
                      "energy_label_C", "energy_label_D", "energy_label_E", 
                      "energy_label_F", "energy_label_G", "Roof_Wall_Insulation", 
                      "Floor_Insulation", "Double_Glazing", "Fully_insulated", 
                      "is_huis")
#-------------------------------------------------------------------
## Model B 1
#-------------------------------------------------------------------

modelB1 <- lm(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
              + bathroom + living_area + size + house_age + garage_binary +
                Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                energy_label_.A.+ energy_label_A + energy_label_B +
                energy_label_C + energy_label_E + energy_label_F+ energy_label_G
              + sted+ omgevingsadressendichtheid+ loginkom+ 
                logwoz+ Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde
              + Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Huisartspraktijk...aantal.binnen.3.km+ 
                Supermarkt..aantal.binnen.3.km+ Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km+ 
                Dagelijkse.levensmiddelen..aantal.binnen.3.km+ Cafe...aantal.binnen.3.km+ 
                Warenhuis..aantal..binnen.5.km+ Cafetaria...aantal.binnen.3.km+ 
                Restaurant...aantal.binnen.3.km+ Hotel..aantal.binnen.5.km+ 
                Kinderdagverblijf...aantal.binnen.3.km+ Basis.onderwijs...aantal.binnen.3.km+ 
                Voortgezet.onderwijs...aantal.binnen.3.km+ Musea...aantal.binnen.5.km, data = mergeddata)
summary(modelB1)

summary_modelB1 <- summary(modelB1)
xtable_modelB1 <- xtable(summary_modelB1)
print(xtable_modelB1, type = "latex")



#-------------------------------------------------------------------
## Model B 2 Zipcode
#-------------------------------------------------------------------

modelB2 <- lmer(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
                + bathroom + living_area + size + house_age + garage_binary +
                  Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                  energy_label_.A.+ energy_label_A + energy_label_B +
                  energy_label_C + energy_label_E + energy_label_F+ energy_label_G
                + sted+ omgevingsadressendichtheid+ loginkom+ 
                  logwoz+ Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde
                + Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Huisartspraktijk...aantal.binnen.3.km+ 
                  Supermarkt..aantal.binnen.3.km+ Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km+ 
                  Dagelijkse.levensmiddelen..aantal.binnen.3.km+ Cafe...aantal.binnen.3.km+ 
                  Warenhuis..aantal..binnen.5.km+ Cafetaria...aantal.binnen.3.km+ 
                  Restaurant...aantal.binnen.3.km+ Hotel..aantal.binnen.5.km+ 
                  Kinderdagverblijf...aantal.binnen.3.km+ Basis.onderwijs...aantal.binnen.3.km+ 
                  Voortgezet.onderwijs...aantal.binnen.3.km+ Musea...aantal.binnen.5.km+ (1 | new_zip_code)
                ,data = mergeddata)
summary(modelB2)
#-------------------------------------------------------------------
## Model B 2 Zipcode
#-------------------------------------------------------------------

modelB3 <- lmer(ln_Pi ~ house_type + building_type_binary+ room + bedroom 
                + bathroom + living_area + size + house_age + garage_binary +
                  Fully_insulated + Roof_Wall_Insulation+ Double_Glazing+
                  energy_label_.A.+ energy_label_A + energy_label_B +
                  energy_label_C + energy_label_E + energy_label_F+ energy_label_G
                + sted+ omgevingsadressendichtheid+ loginkom+ 
                  logwoz+ Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde
                + Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Huisartspraktijk...aantal.binnen.3.km+ 
                  Supermarkt..aantal.binnen.3.km+ Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km+ 
                  Dagelijkse.levensmiddelen..aantal.binnen.3.km+ Cafe...aantal.binnen.3.km+ 
                  Warenhuis..aantal..binnen.5.km+ Cafetaria...aantal.binnen.3.km+ 
                  Restaurant...aantal.binnen.3.km+ Hotel..aantal.binnen.5.km+ 
                  Kinderdagverblijf...aantal.binnen.3.km+ Basis.onderwijs...aantal.binnen.3.km+ 
                  Voortgezet.onderwijs...aantal.binnen.3.km+ Musea...aantal.binnen.5.km+ (1 | city)
                ,data = mergeddata)

# Summary of the zip code model
summary(modelB3)

# Define the predictors and outcome variable
predictors <- c("house_type", "building_type_binary", "room", "bedroom", 
                "bathroom", "living_area", "size", "house_age", "garage_binary",
                "Fully_insulated", "Roof_Wall_Insulation", "Double_Glazing",
                "energy_label_.A.", "energy_label_A", "energy_label_B",
                "energy_label_C", "energy_label_E", "energy_label_F", "energy_label_G",
                "sted", "omgevingsadressendichtheid", "loginkom", "logwoz",
                "Treinstation...dichtstbijzijnde", "Overstapstation.trein...dichtstbijzijnde",
                "Oprit...dichtstbijzijnde", "totalegroenenstemmen",
                "Huisartspraktijk...aantal.binnen.3.km", "Supermarkt..aantal.binnen.3.km",
                "Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km",
                "Dagelijkse.levensmiddelen..aantal.binnen.3.km", "Cafe...aantal.binnen.3.km",
                "Warenhuis..aantal..binnen.5.km", "Cafetaria...aantal.binnen.3.km",
                "Restaurant...aantal.binnen.3.km", "Hotel..aantal.binnen.5.km",
                "Kinderdagverblijf...aantal.binnen.3.km", "Basis.onderwijs...aantal.binnen.3.km",
                "Voortgezet.onderwijs...aantal.binnen.3.km", "Musea...aantal.binnen.5.km")

outcome <- "ln_Pi"


#---------------------------------------------------
# Correlation Matrix
#---------------------------------------------------
library(corrplot)
mergeddata$house_type
mergeddata$house_type_binary <- ifelse(mergeddata$house_type == "huis", 1, 0)

correlation_matrix_A <- cor(mergeddata[c("ln_Pi", "house_type_binary","building_type_binary", "room", "bedroom", 
                                       "bathroom", "living_area", "size", "house_age", "garage_binary",
                                       "Fully_insulated", "Roof_Wall_Insulation", "Double_Glazing",
                                       "energy_label_.A.", "energy_label_A", "energy_label_B",
                                       "energy_label_C", "energy_label_E",
                                       "energy_label_F", "energy_label_G")])

corr_plot <- corrplot(correlation_matrix_A, type = "lower", method = "circle",
                      tl.col = "black", # Set text color to black
                      tl.srt = 45,      # Rotate text by 15 degrees
                      tl.cex = 0.5)     # Reduce text size
pdf(file = file.path(dirRslt, "correlation_plot_ModelA.pdf"))
corrplot(correlation_matrix_A, type = "lower", method = "circle",
         tl.col = "black", # Set text color to black
         tl.srt = 45,      # Rotate text by 45 degrees
         tl.cex = 0.5)     # Reduce text size
dev.off()
# Define the variables used in the B models
variables_B_models <- c("house_type","house_type_binary", "building_type_binary", "room", "bedroom", 
                        "bathroom", "living_area", "size", "house_age", "garage_binary",
                        "Fully_insulated", "Roof_Wall_Insulation", "Double_Glazing",
                        "energy_label_.A.", "energy_label_A", "energy_label_B",
                        "energy_label_C", "energy_label_E", "energy_label_F", "energy_label_G",
                        "sted", "omgevingsadressendichtheid", "loginkom", "logwoz",
                        "Treinstation...dichtstbijzijnde", "Overstapstation.trein...dichtstbijzijnde",
                        "Oprit...dichtstbijzijnde", "totalegroenenstemmen",
                        "Huisartspraktijk...aantal.binnen.3.km", "Supermarkt..aantal.binnen.3.km",
                        "Ziekenhuis.inclusief.polikliniek...aantal..binnen.5.km",
                        "Dagelijkse.levensmiddelen..aantal.binnen.3.km", "Cafe...aantal.binnen.3.km",
                        "Warenhuis..aantal..binnen.5.km", "Cafetaria...aantal.binnen.3.km",
                        "Restaurant...aantal.binnen.3.km", "Hotel..aantal.binnen.5.km",
                        "Kinderdagverblijf...aantal.binnen.3.km", "Basis.onderwijs...aantal.binnen.3.km",
                        "Voortgezet.onderwijs...aantal.binnen.3.km", "Musea...aantal.binnen.5.km")

numeric_data_B <- mergeddata %>% select(all_of(variables_B_models)) %>% select_if(is.numeric)
correlation_matrix_B <- cor(numeric_data_B, use = "complete.obs")
CorModelB<- corrplot(correlation_matrix_B, type = "lower", method = "circle",
                     tl.col = "black", # Set text color to black
                     tl.srt = 45,      # Rotate text by 45 degrees
                     tl.cex = 0.5)     # Reduce text size

pdf(file = file.path(dirRslt, "correlation_plot_B_models.pdf"))
corrplot(correlation_matrix_B, type = "lower", method = "circle",
         tl.col = "black", # Set text color to black
         tl.srt = 45,      # Rotate text by 45 degrees
         tl.cex = 0.5)     # Reduce text size
dev.off()

get_var_pairs <- function(correlation_matrix, threshold) {
  high_corr_pairs <- which(abs(correlation_matrix) >= threshold & abs(correlation_matrix) < 1, arr.ind = TRUE)
  # Ensure i < j to avoid duplicate pairs
  high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
  pairs <- data.frame(var1 = rownames(correlation_matrix)[high_corr_pairs[, 1]],
                      var2 = colnames(correlation_matrix)[high_corr_pairs[, 2]])
  return(pairs)
}

threshold <- 0.8  # Define your threshold for high correlation
high_corr_pairs_A <- get_var_pairs(correlation_matrix_A, threshold)
high_corr_pairs_B <- get_var_pairs(correlation_matrix_B, threshold)

# Print the results
cat("High correlation pairs in Model A:\n")
print(unique(high_corr_pairs_A))

cat("\nHigh correlation pairs in Model B:\n")
print(unique(high_corr_pairs_B))

# Function to convert data frame to LaTeX table
to_latex <- function(df, caption) {
  library(xtable)
  latex_table <- xtable(df, caption = caption)
  print(latex_table, type = "latex")
}

# Export to LaTeX
cat("\n\\subsection{High correlation pairs in Model A}\n")
cat(to_latex(unique(high_corr_pairs_A), "High correlation pairs in Model A"))

cat("\n\\subsection{High correlation pairs in Model B}\n")
cat(to_latex(unique(high_corr_pairs_B), "High correlation pairs in Model B"))

# Ensure the car package is loaded
library(car)

# Calculate VIF for simple linear regression model A1
vif_A1 <- vif(simple_modelA1)
cat("VIF for Model A1:\n")
print(vif_A1)
top_vif_A1 <- sort(vif_A1, decreasing = TRUE)[1:5]
cat("Top 5 VIF values for Model A1:\n")
print(top_vif_A1)
# Calculate VIF for simple linear regression model B1
vif_B1 <- vif(modelB1)
cat("VIF for Model B1:\n")
print(vif_B1)
top_vif_B1 <- sort(vif_B1, decreasing = TRUE)[1:5]
cat("Top 5 VIF values for Model B1:\n")
print(top_vif_B1)

