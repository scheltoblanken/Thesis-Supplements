#----------------------------------------------------------------------
## Regression models after Multicollinearity
#----------------------------------------------------------------------

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
library(lmerTest)

remove(list=ls())
cat("\f")
dir             <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)
dirData         <- file.path(dir, "Data")
dirProg         <- file.path(dir, "Programs")
dirRslt         <- file.path(dir, "Results")

MERGEDDATA_A <- read.csv(paste0(dirData,"/finaldataformmultiA.csv"))
MERGEDDATA_B <- read.csv(paste0(dirData,"/finaldataformmultiB.csv"))
MERGEDDATA_B$house_type_binary <- ifelse(MERGEDDATA_B$house_type == "huis", 1, 0)
MERGEDDATA_A$energy_labelAA <-MERGEDDATA_A$energy_label_.A.
MERGEDDATA_B$energy_labelAA <-MERGEDDATA_B$energy_label_.A.
MERGEDDATA_B$house_type
# Model A
model_A1 <- lm(ln_Pi ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area + 
                house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation + 
                Double_Glazing + energy_labelAA + energy_label_A + energy_label_B + 
                energy_label_C + energy_label_E + energy_label_F + energy_label_G, data = MERGEDDATA_A)

model_A2 <- lmer(ln_Pi ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area + 
                   house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation + 
                   Double_Glazing + energy_labelAA + energy_label_A + energy_label_B + 
                   energy_label_C + energy_label_E + energy_label_F + energy_label_G +(1 | new_zip_code), data = MERGEDDATA_A)

model_A3 <- lmer(ln_Pi ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area + 
                   house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation + 
                   Double_Glazing + energy_labelAA + energy_label_A + energy_label_B + 
                   energy_label_C + energy_label_E + energy_label_F + energy_label_G +(1 | city), data = MERGEDDATA_A)
# Model B
model_B1 <- lm(ln_Pi ~ house_type_binary+ building_type_binary+ bedroom+ bathroom+ living_area+
               house_age+ garage_binary+ Fully_insulated+ Roof_Wall_Insulation+
               Double_Glazing+ energy_labelAA+ energy_label_A+ energy_label_B+
               energy_label_C+ energy_label_E+ energy_label_F+ energy_label_G+
               sted+ logwoz+Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde+
               Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Cafe...aantal.binnen.3.km+Warenhuis..aantal..binnen.5.km+ Hotel..aantal.binnen.5.km+
               Voortgezet.onderwijs...aantal.binnen.3.km, data = MERGEDDATA_B)

model_B2 <- lmerTest::lmer(ln_Pi ~ house_type_binary+ building_type_binary+ bedroom+ bathroom+ living_area+
                 house_age+ garage_binary+ Fully_insulated+ Roof_Wall_Insulation+
                 Double_Glazing+ energy_labelAA+ energy_label_A+ energy_label_B+
                 energy_label_C+ energy_label_E+ energy_label_F+ energy_label_G+
                 sted+ logwoz+Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde+
                 Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Cafe...aantal.binnen.3.km+Warenhuis..aantal..binnen.5.km+ Hotel..aantal.binnen.5.km+
                 Voortgezet.onderwijs...aantal.binnen.3.km+(1 | new_zip_code), data = MERGEDDATA_B)

model_B3 <- lmer(ln_Pi ~ house_type_binary+ building_type_binary+ bedroom+ bathroom+ living_area+
                 house_age+ garage_binary+ Fully_insulated+ Roof_Wall_Insulation+
                 Double_Glazing+ energy_labelAA+ energy_label_A+ energy_label_B+
                 energy_label_C+ energy_label_E+ energy_label_F+ energy_label_G+
                 sted+ logwoz+Treinstation...dichtstbijzijnde+ Overstapstation.trein...dichtstbijzijnde+
                 Oprit...dichtstbijzijnde+ totalegroenenstemmen+ Cafe...aantal.binnen.3.km+Warenhuis..aantal..binnen.5.km+ Hotel..aantal.binnen.5.km+
                 Voortgezet.onderwijs...aantal.binnen.3.km+(1 | city), data = MERGEDDATA_B)


# Determining the best models
library(lme4)
library(MuMIn)

aic_bic_table <- data.frame(
  Model = c("model_A1", "model_A2", "model_A3", "model_B1", "model_B2", "model_B3"),
  AIC = c(AIC(model_A1), AIC(model_A2), AIC(model_A3), AIC(model_B1), AIC(model_B2), AIC(model_B3)),
  BIC = c(BIC(model_A1), BIC(model_A2), BIC(model_A3), BIC(model_B1), BIC(model_B2), BIC(model_B3))
)

r_squared_table <- data.frame(
  Model = c("model_A1", "model_B1"),
  R_squared = c(summary(model_A1)$r.squared, summary(model_B1)$r.squared)
)

mixed_r_squared_table <- data.frame(
  Model = c("model_A2", "model_A3", "model_B2", "model_B3"),
  Marginal_R_squared = c(r.squaredGLMM(model_A2)[1], r.squaredGLMM(model_A3)[1], r.squaredGLMM(model_B2)[1], r.squaredGLMM(model_B3)[1]),
  Conditional_R_squared = c(r.squaredGLMM(model_A2)[2], r.squaredGLMM(model_A3)[2], r.squaredGLMM(model_B2)[2], r.squaredGLMM(model_B3)[2])
)

print("AIC and BIC Comparison:")
print(aic_bic_table)

print("R-squared for Linear Models:")
print(r_squared_table)

print("Marginal and Conditional R-squared for Mixed Models:")
print(mixed_r_squared_table)

# Visualize AIC and BIC
ggplot(aic_bic_table, aes(x = Model)) +
  geom_bar(aes(y = AIC), stat = "identity", fill = "blue", alpha = 0.7) +
  geom_bar(aes(y = BIC), stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "AIC and BIC Comparison", y = "Value") +
  theme_minimal()

# Visualize R-squared
ggplot(r_squared_table, aes(x = Model, y = R_squared)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "R-squared for Linear Models", y = "R-squared") +
  theme_minimal()

# Visualize Mixed Model R-squared
mixed_r_squared_long <- melt(mixed_r_squared_table, id = "Model")
ggplot(mixed_r_squared_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Marginal and Conditional R-squared for Mixed Models", y = "R-squared") +
  theme_minimal()

