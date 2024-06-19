#--------------------------------------------------------
## SETUP R-CODE
#--------------------------------------------------------

# Clear workspace and console
remove(list=ls())
cat("\f")

# Set working directory
dir <- "C:/Users/schel/Documents/Master thesis/Huizen markt"
setwd(dir)

# Define subdirectories
dirData <- file.path(dir, "Data")
dirProg <- file.path(dir, "Programs")
dirRslt <- file.path(dir, "Results")

#--------------------------------------------------------
## SETUP Libraries
#--------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(lmtest)
library(car)
library(ggplot2)
library(foreign)
library(readxl)

#--------------------------------------------------------
## SETUP Data sets
#--------------------------------------------------------

Allcities <- read.csv("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/Funda/RURAL_FUNDA_FULL.csv")

#--------------------------------------------------------
## DATA Kerncijfers wijk en buurt KWB 2022
#--------------------------------------------------------

# Read KWB data
dbf_file2 <- file.path(dirData, "kwb-2022.xlsx")
KWBata <- read_xlsx(dbf_file2)
colnames(KWBata)


# Clean neighborhood names in Allcities dataframe
Allcities$neighborhood_name <- tolower(Allcities$neighborhood_name)
Allcities$neighborhood_name <- gsub("[[:punct:]]", "", Allcities$neighborhood_name)
Allcities$neighborhood_name <- trimws(Allcities$neighborhood_name)


# Clean City names in Allcities dataframe
Allcities$city <- tolower(Allcities$city)
Allcities$city <- gsub("[[:punct:]]", "", Allcities$city)
Allcities$city <- trimws(Allcities$city)

# Clean neighborhood names in KWBata dataframe
KWBata$regio <- tolower(KWBata$regio)
KWBata$regio <- gsub("[[:punct:]]", "", KWBata$regio)
KWBata$regio <- trimws(KWBata$regio)

# Match neighborhoods from Allcities with KWBata
unique_neighborhoods <- unique(Allcities$neighborhood_name)
Allcities$regio <- Allcities$neighborhood_name
matchingKWBata <- KWBata[KWBata$regio %in% unique_neighborhoods, ]

# Filter the rows with "Buurt"
KWBata_buurt <- KWBata %>% filter(recs == "Buurt")
KWBata_buurt$regio

#merge data
merged_data <- merge(Allcities, KWBata_buurt, by.x = "regio", by.y = "regio", all.x = FALSE)
Finaldata <- unique(merged_data)


# Clean GM names in merged_data dataframe
merged_data$gm_naam <- ifelse(merged_data$gm_naam == "'s-Gravenhage", "Den Haag", merged_data$gm_naam)
unique(merged_data$gm_naam)
unique(merged_data$city)
merged_data$gm_naam  <- tolower(merged_data$gm_naam )
merged_data$gm_naam  <- gsub("[[:punct:]]", "", merged_data$gm_naam )
merged_data$gm_naam  <- trimws(merged_data$gm_naam )

#delete all the wrongly merged This ensures that only rows with matching municipality names are retained in filtered_data.
filtered_data <- merged_data %>%
  filter(city == gm_naam)
write.csv(filtered_data,file = "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/CBS/RURAL_CBS_FULL.csv", row.names = FALSE)

