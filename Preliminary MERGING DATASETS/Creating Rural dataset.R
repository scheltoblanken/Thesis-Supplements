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
library(readxl)
#--------------------------------------------------------
##SETUP Data sets
#--------------------------------------------------------

dirDataBuyClean <- paste0(dirData, "data RURAL/CLEAN/")
UrkCLB          <- read.csv(paste0(dirDataBuyClean, "Urk dwellings CLEANED BUY DATA.csv"))
UdenCLB         <- read.csv(paste0(dirDataBuyClean, "Uden dwellings CLEANED BUY DATA.csv"))
SwifterbantCLB  <- read.csv(paste0(dirDataBuyClean, "Swifterbant dwellings CLEANED BUY DATA.csv"))
PuttenCLB       <- read.csv(paste0(dirDataBuyClean, "Putten dwellings CLEANED BUY DATA.csv"))
OisterwijkCLB   <- read.csv(paste0(dirDataBuyClean, "Oisterwijk dwellings CLEANED BUY DATA.csv"))
OirschotCLB     <- read.csv(paste0(dirDataBuyClean, "Oirschot dwellings CLEANED BUY DATA.csv"))
ZeewoldeCLB     <- read.csv(paste0(dirDataBuyClean, "Zeewolde dwellings CLEANED BUY DATA.csv"))
HarderwijkCLB   <- read.csv(paste0(dirDataBuyClean, "Harderwijk dwellings CLEANED BUY DATA.csv"))
ElbrugCLB       <- read.csv(paste0(dirDataBuyClean, "Elbrug dwellings CLEANED BUY DATA.csv"))
EdeCLB          <- read.csv(paste0(dirDataBuyClean, "Ede dwellings CLEANED BUY DATA.csv"))
BoxtelCLB       <- read.csv(paste0(dirDataBuyClean, "Boxtel dwellings CLEANED BUY DATA.csv"))
BoxmeerCLB      <- read.csv(paste0(dirDataBuyClean, "Boxmeer dwellings CLEANED BUY DATA.csv"))
BiddinghuizenCLB<- read.csv(paste0(dirDataBuyClean, "Biddinghuizen dwellings CLEANED BUY DATA.csv"))


dirDataBuyRAW  <- paste0(dirData, "data RURAL/RAW/")
UrkRB          <- read.csv(paste0(dirDataBuyRAW, "Urk dwellings RAW BUY DATA.csv"))
UdenRB         <- read.csv(paste0(dirDataBuyRAW, "Uden dwellings RAW BUY DATA.csv"))
SwifterbantRB  <- read.csv(paste0(dirDataBuyRAW, "Swifterbant dwellings RAW BUY DATA.csv"))
PuttenRB       <- read.csv(paste0(dirDataBuyRAW, "Putten dwellings RAW BUY DATA.csv"))
OisterwijkRB   <- read.csv(paste0(dirDataBuyRAW, "Oisterwijk dwellings RAW BUY DATA.csv"))
OirschotRB     <- read.csv(paste0(dirDataBuyRAW, "Oirschot dwellings RAW BUY DATA.csv"))
ZeewoldeRB     <- read.csv(paste0(dirDataBuyRAW, "Zeewolde dwellings RAW BUY DATA.csv"))
HarderwijkRB   <- read.csv(paste0(dirDataBuyRAW, "Harderwijk dwellings RAW BUY DATA.csv"))
ElbrugRB       <- read.csv(paste0(dirDataBuyRAW, "Elbrug dwellings RAW BUY DATA.csv"))
EdeRB          <- read.csv(paste0(dirDataBuyRAW, "Ede dwellings RAW BUY DATA.csv"))
BoxtelRB       <- read.csv(paste0(dirDataBuyRAW, "Boxtel dwellings RAW BUY DATA.csv"))
BoxmeerRB      <- read.csv(paste0(dirDataBuyRAW, "Boxmeer dwellings RAW BUY DATA.csv"))
BiddinghuizenRB<- read.csv(paste0(dirDataBuyRAW, "Biddinghuizen dwellings RAW BUY DATA.csv"))

dirDataSave <- "C:/Users/schel/Documents/Master thesis/Huizen markt/Data/Full data/"
rural <- c("Urk", "Uden", "Swifterbant", "Putten", "Oisterwijk", "Oirschot", "Zeewolde", "Harderwijk", "Elbrug", "Ede", "Boxtel", "Boxmeer", "Biddinghuizen")
AllruralCLB <- rbind(UrkCLB, UdenCLB, SwifterbantCLB, PuttenCLB, OisterwijkCLB, OirschotCLB, ZeewoldeCLB, HarderwijkCLB, ElbrugCLB, EdeCLB, BoxtelCLB, BoxtelCLB, BoxmeerCLB, BiddinghuizenCLB)
AllruralRWB <- rbind(UrkRB, UdenRB, SwifterbantRB, PuttenRB, OisterwijkRB, OirschotRB, ZeewoldeRB, HarderwijkRB, ElbrugRB, EdeRB, BoxtelRB, BoxtelRB, BoxmeerRB, BiddinghuizenRB)

merged_data <- merge(AllruralCLB, AllruralRWB, by = "address", all.x = FALSE)


merged_data$new_zip_code <- sub("^(\\d{4}\\s*[A-Z]{2}).*$", "\\1", merged_data$zip_code)

dirDataSave <- "C:/Users/schel/Documents/Master thesis/Huizen markt/Data/Full data/"
colnames(AllruralCLB)
colnames(AllruralRWB)


colnames(combined_data)
selected_columns <- c("address", "house_id", "city.x", "house_type", "building_type.x", 
                      "price.x", "price_m2", "room", "bedroom", "bathroom", 
                      "living_area.x", "energy_label.x", "zip_code", "year_built", 
                      "house_age", "descrip.x", "size", "year", "neighborhood_name", "city.y", "insulation","parking" )

new_dataframe <- merged_data[, selected_columns]
colnames(new_dataframe) <- c("address", "house_id", "city", "house_type", "building_type", 
                             "price", "price_m2", "room", "bedroom", "bathroom", 
                             "living_area", "energy_label", "zip_code", "year_built", 
                             "house_age", "description", "size", "year", "neighborhood_name", "city_raw", "Insulation","garage")

new_dataframe$new_zip_code <- sub("^(\\d{4}\\s*[A-Z]{2}).*$", "\\1", new_dataframe$zip_code)
print(new_dataframe)


dirToSave <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/Funda/"
write.csv(new_dataframe, file = paste0(dirToSave, "Allrural_merged_data2.csv"), row.names = FALSE)

