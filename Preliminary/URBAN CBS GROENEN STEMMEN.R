# Clear workspace and console
remove(list=ls())
cat("\f")

# Set working directory
dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)

# Define subdirectories
dirData <- file.path(dir, "Data")
dirProg <- file.path(dir, "Programs")
dirRslt <- file.path(dir, "Results")



#Verkiezingen matching
fulldata <- read.csv("C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/CBS/RURAL_CBS_FULL.csv")
verkiezingsdata <- read.csv("C:/Users/schel/Documents/Master thesis/Huizen markt/Data/Verkiezingsgegevens.csv")

# Clean neighborhood names in ALLDATA_clean dataframe
fulldata$city <- gsub("[[:punct:]]", "", fulldata$city)
fulldata$city <- trimws(fulldata$city)
verkiezingsdata$gemeente
# Clean neighborhood names in dbf_data dataframe
verkiezingsdata$gemeente <- gsub("[[:punct:]]", "",verkiezingsdata$gemeente)
verkiezingsdata$gemeente <- trimws(verkiezingsdata$gemeente)
verkiezingsdata$gemeente <- tolower(verkiezingsdata$gemeente)

# Extract unique gemeente from fulldata and verkiezingsdata
unique_gemeente_fulldata <- unique(fulldata$city)
unique_gemeente_verkiezingsdata <- unique(verkiezingsdata$gemeente)


# Replace "den-haag" with "Den Haag" in the fulldata$city column
fulldata$city <- gsub("den-haag", "den haag", fulldata$city, ignore.case = TRUE)
verkiezingsdata$gemeente
# Check the unique values in fulldata$city to confirm the change
unique(fulldata$city)
gemeente_not_in_verkiezingsdata <- setdiff(unique_gemeente_fulldata, unique_gemeente_verkiezingsdata)


merged_data2 <- merge(fulldata, verkiezingsdata, by.x = "city", by.y = "gemeente", all.x = FALSE)
mergedVerkiezing<- unique(merged_data2)
# Exporteer het dataframe als een CSV-bestand
write.csv(mergedVerkiezing, file = "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/CBS GROEN STEMMERS/RURAL CBS GROEN.csv", row.names = FALSE)
