#--------------------------------------------------------
## SETUP R-CODE
#--------------------------------------------------------
remove(list=ls())
cat("\f")
dir <- "C:/Users/schel/Documents/Master thesis/Huizen markt 2"
dirData <- file.path(dir, "Data")
dirProg <- file.path(dir, "Programs")
dirRslt <- file.path(dir, "Results")

#--------------------------------------------------------
## SETUP Libraries
#--------------------------------------------------------
library(dplyr)
library(lmtest)
library(car)
library(ggplot2)
library(foreign)
library(readxl)
library(stringr)
#--------------------------------------------------------
## SETUP Data sets
#--------------------------------------------------------

Allcities <- read.csv("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/Funda/RURAL_FUNDA_FULL (2).csv")

#--------------------------------------------------------
## DATA Kerncijfers wijk en buurt KWB 2022
#--------------------------------------------------------
dbf_file2 <- file.path(dirData, "pc5_2020_vol.xlsx")
KWBata <- read_xlsx("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/pc5_2020_vol.xlsx", skip = 8)



Allcities$new_zip_code
Allcities$new_zip_code <- tolower(Allcities$new_zip_code)
Allcities$new_zip_code <- gsub("[[:punct:]]", "", Allcities$new_zip_code)
Allcities$new_zip_code <- trimws(Allcities$new_zip_code)
Allcities$new_zip_code <- gsub("\\s+(\\w).*", "\\1", Allcities$new_zip_code)
Allcities$new_zip_code <- str_extract(Allcities$new_zip_code, "\\d{4}[A-Za-z]?")

KWBata$`Postcode-5`
KWBata$`Postcode-5` <- tolower(KWBata$`Postcode-5`)
KWBata$`Postcode-5` <- gsub("[[:punct:]]", "", KWBata$`Postcode-5`)
KWBata$`Postcode-5` <- trimws(KWBata$`Postcode-5`)
KWBata$`Postcode-5` <- gsub("\\s+(\\w).*", "\\1", KWBata$`Postcode-5`)
KWBata$`Postcode-5` <- str_extract(KWBata$`Postcode-5`, "\\d{4}[A-Za-z]?")


merged_data <- merge(Allcities, KWBata_unique, by.x = "new_zip_code", by.y = "Postcode-5", all.x = TRUE)

dirToSave <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/CBS/"
merged_data <- merged_data %>%
  rename(sted = ...130)
write.csv(merged_data, file = paste0(dirToSave, "RURAL_CBS_FULL POSTCODES (2).csv"), row.names = FALSE)





 





