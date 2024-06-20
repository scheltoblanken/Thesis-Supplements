#--------------------------------------------------------
##SETUP R-CODE
#--------------------------------------------------------

remove(list=ls())
cat("\f")
dir<- "C:/Users/schel/Documents/Master thesis/Huizen markt"
setwd("C:/Users/schel/Documents/Master thesis/Huizen markt")

dirData <- paste0(dir, "/Data/")
dirProg <- paste0(dir, "/Programs/")
dirRslt <- paste0(dir, "/Results/")

#--------------------------------------------------------
##SETUP Libraries
#--------------------------------------------------------

library(dplyr)  
library(lmtest) 
library(car)    
library(ggplot2)

#--------------------------------------------------------
##SETUP Data sets
#--------------------------------------------------------

dirDataBuyClean <- paste0(dirData, "/BUY/CLEANED/")
AlmereClB       <- read.csv(paste0(dirDataBuyClean, "Almere 405 dwellings CLEANED BUY data.csv"))
AmsterdamClB    <- read.csv(paste0(dirDataBuyClean, "Amsterdam 405 dwellings CLEANED BUY data.csv"))
BredaClB        <- read.csv(paste0(dirDataBuyClean, "Breda 405 dwellings CLEANED BUY data.csv"))
DenHaagClB      <- read.csv(paste0(dirDataBuyClean, "Den Haag 405 dwellings CLEANED BUY data.csv"))
EindhovenClB    <- read.csv(paste0(dirDataBuyClean, "Eindhoven 405 dwellings CLEANED BUY data.csv"))
GroningenClB    <- read.csv(paste0(dirDataBuyClean, "Groningen 405 dwellings CLEANED BUY data.csv"))
NijmegenClB     <- read.csv(paste0(dirDataBuyClean, "Nijmegen 405 dwellings CLEANED BUY data.csv"))
RotterdamClB    <- read.csv(paste0(dirDataBuyClean, "Rotterdam 405 dwellings CLEANED BUY data.csv"))
TilburgClB      <- read.csv(paste0(dirDataBuyClean, "Tilburg 405 dwellings CLEANED BUY data.csv"))
UtrechtClB      <- read.csv(paste0(dirDataBuyClean, "Utrecht 405 dwellings CLEANED BUY data.csv"))

dirDataBuyRAW <- paste0(dirData, "/BUY/RAW/")
AlmereRWB       <- read.csv(paste0(dirDataBuyRAW, "Almere 405 dwellings RAW BUY data.csv"))
AmsterdamRWB    <- read.csv(paste0(dirDataBuyRAW, "Amsterdam 405 dwellings RAW BUY data.csv"))
BredaRWB        <- read.csv(paste0(dirDataBuyRAW, "Breda 405 dwellings RAW BUY data.csv"))
DenHaagRWB      <- read.csv(paste0(dirDataBuyRAW, "Den Haag 405 dwellings RAW BUY data.csv"))
EindhovenRWB    <- read.csv(paste0(dirDataBuyRAW, "Eindhoven 405 dwellings RAW BUY data.csv"))
GroningenRWB    <- read.csv(paste0(dirDataBuyRAW, "Groningen 405 dwellings RAW BUY data.csv"))
NijmegenRWB     <- read.csv(paste0(dirDataBuyRAW, "Nijmegen 405 dwellings RAW BUY data.csv"))
RotterdamRWB    <- read.csv(paste0(dirDataBuyRAW, "Rotterdam 405 dwellings RAW BUY data.csv"))
TilburgRWB      <- read.csv(paste0(dirDataBuyRAW, "Tilburg 405 dwellings RAW BUY data.csv"))
UtrechtRWB      <- read.csv(paste0(dirDataBuyRAW, "Utrecht 405 dwellings RAW BUY data.csv"))

AllcitiesCLB       <- rbind(AlmereClB,AmsterdamClB,BredaClB,DenHaagClB,EindhovenClB,GroningenClB,NijmegenClB,RotterdamClB,TilburgClB,UtrechtClB)
AllcitiesRWB      <- rbind(AlmereRWB, AmsterdamRWB, BredaRWB, DenHaagRWB, EindhovenRWB, GroningenRWB, NijmegenRWB, RotterdamRWB, TilburgRWB, UtrechtRWB)
AllcitiesRWB$new_zip_code <- sub("^(\\d{4}\\s*[A-Z]{2}).*$", "\\1", AllcitiesRWB$zip_code)


cities <- c("Almere", "Amsterdam", "Breda", "Den Haag", "Eindhoven", "Groningen", "Nijmegen", "Rotterdam", "Tilburg", "Utrecht")


dirDataSave <- "C:/Users/schel/Documents/Master thesis/Huizen markt/Data/Full data/"
colnames(AllcitiesCLB)
colnames(AllcitiesRWB)


for(city in cities) {
  
  ClB_data <- read.csv(paste0(dirDataBuyClean, city, " 405 dwellings CLEANED BUY data.csv"))
  RWB_data <- read.csv(paste0(dirDataBuyRAW, city, " 405 dwellings RAW BUY data.csv"))
  merged_data <- merge(ClB_data, RWB_data, by = "address", all.x = TRUE)
  write.csv(merged_data, file = paste0(dirDataSave, city, "_merged_data2.csv"), row.names = FALSE)
}


cities <- c("Almere", "Amsterdam", "Breda", "Den Haag", "Eindhoven", "Groningen", "Nijmegen", "Rotterdam", "Tilburg", "Utrecht")

dirDataSave <- "C:/Users/schel/Documents/Master thesis/Huizen markt/Data/Full data/"
combined_data <- do.call(rbind, lapply(cities, function(city) {
  file_path <- paste0(dirDataSave, city, "_merged_data2.csv")
  if (file.exists(file_path)) {
    read.csv(file_path)
  } else {
    stop(paste("File not found:", file_path))
  }
}))

colnames(combined_data)
selected_columns <- c("address", "house_id", "city.x", "house_type", "building_type.x", 
                      "price.x", "price_m2", "room", "bedroom", "bathroom", 
                      "living_area.x", "energy_label.x", "zip_code", "year_built", 
                      "house_age", "descrip.x", "size", "year", "neighborhood_name", "city.y", "insulation","parking" )

new_dataframe <- combined_data[, selected_columns]
colnames(new_dataframe) <- c("address", "house_id", "city", "house_type", "building_type", 
                             "price", "price_m2", "room", "bedroom", "bathroom", 
                             "living_area", "energy_label", "zip_code", "year_built", 
                             "house_age", "description", "size", "year", "neighborhood_name", "city_raw", "Insulation","garage")

new_dataframe$new_zip_code <- sub("^(\\d{4}\\s*[A-Z]{2}).*$", "\\1", new_dataframe$zip_code)
print(new_dataframe)


dirToSave <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/Funda/"
write.csv(new_dataframe, file = paste0(dirToSave, "Allcities_merged_data2.csv"), row.names = FALSE)

 