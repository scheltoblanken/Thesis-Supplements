#----------------------------------------------------------------------
#VIF
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


# Models
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

vif_B1 <- vif(model_B1)
tolerance_B1 <- 1 / vif_B11
vif_B1
tolerance_B1

# For mixed model (model_B2),fixed effects only
vif_B2 <- vif(lm(ln_Pi ~ house_type_binary + building_type_binary + bedroom + bathroom + living_area +
                   house_age + garage_binary + Fully_insulated + Roof_Wall_Insulation +
                   Double_Glazing + energy_labelAA + energy_label_A + energy_label_B +
                   energy_label_C + energy_label_E + energy_label_F + energy_label_G +
                   sted + logwoz + Treinstation...dichtstbijzijnde + Overstapstation.trein...dichtstbijzijnde +
                   Oprit...dichtstbijzijnde + totalegroenenstemmen + Cafe...aantal.binnen.3.km + Warenhuis..aantal..binnen.5.km + Hotel..aantal.binnen.5.km +
                   Voortgezet.onderwijs...aantal.binnen.3.km, data = MERGEDDATA_B))

tolerance_B2 <- 1 / vif_B2
vif_B2
tolerance_B2