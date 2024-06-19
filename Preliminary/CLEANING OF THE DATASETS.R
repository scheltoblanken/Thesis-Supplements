# Clear workspace and console
remove(list=ls())
cat("\f")

# Set working directory
dir             <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2"
setwd(dir)

# Define subdirectories
dirData         <- file.path(dir, "Data")
dirProg         <- file.path(dir, "Programs")
dirRslt         <- file.path(dir, "Results")
#--------------------------------------------------------
## Alle relevante data sets
#--------------------------------------------------------
#fundasets
FUNDA_RURAL     <-  read.csv(paste0(dirData,"/Funda/RURAL_FUNDA_FULL (2).csv"))
FUNDA_URBAN     <-  read.csv(paste0(dirData,"/Funda/URBAN_FUNDA_FULL (2).csv"))
#CBS datasets
CBS_RURAL       <-  read.csv(paste0(dirData,"/CBS/RURAL_CBS_FULL POSTCODES (2).csv"))
CBS_URBAN       <-  read.csv(paste0(dirData,"/CBS/URBAN_CBS_FULL POSTCODES (2).csv"))
#CBS + GREEN VOTERS
CBSGREEN_RURAL  <-  read.csv(paste0(dirData,"/CBS GROEN STEMMERS/RURAL CBS GROEN (2).csv"))
CBSGREEN_URBAN  <-  read.csv(paste0(dirData,"/CBS GROEN STEMMERS/URBAN CBS GROEN (2).csv"))

#--------------------------------------------------------
## Preliminary steps DUMMY VARIABLE FOR THE ENERGY LABELS
#--------------------------------------------------------
# List of datasets and their names
datasets <- list(FUNDA_RURAL, FUNDA_URBAN, CBS_RURAL, CBS_URBAN, CBSGREEN_RURAL, CBSGREEN_URBAN)
dataset_names <- c("FUNDA_RURAL", "FUNDA_URBAN", "CBS_RURAL", "CBS_URBAN", "CBSGREEN_RURAL", "CBSGREEN_URBAN")

# Create dummy variables for energy labels
energy_labels <- c(">A+", "A", "B", "C", "D", "E", "F", "G")
for (i in seq_along(datasets)) {
  dataset <- datasets[[i]]
  for (label in energy_labels) {
    dummy_var <- ifelse(dataset$energy_label == label, 1, 0)
    col_name <- paste0("energy_label_", label)
    dataset[[col_name]] <- dummy_var
  }
  datasets[[i]] <- dataset  # Update the dataset in the list
}

# Assign updated datasets back to their original variable names
for (i in seq_along(datasets)) {
  assign(dataset_names[i], datasets[[i]])
}

# List of Funda datasets
datasets <- list(FUNDA_RURAL, FUNDA_URBAN)

# Loop over each Funda dataset
for (i in seq_along(datasets)) {
  dataset <- datasets[[i]]
  dataset$huisleeftijd <- 2024 - dataset$year_built
  dataset$size <- as.numeric(gsub(" m²", "", dataset$size))
  dataset$size <- as.numeric(gsub(",", ".", dataset$size))
  datasets[[i]] <- dataset  # Update the dataset in the list
}

# Assign updated Funda datasets back to their original variable names
assign("FUNDA_RURAL", datasets[[1]])
assign("FUNDA_URBAN", datasets[[2]])

# List of CBS datasets
datasets <- list(CBS_RURAL, CBS_URBAN, CBSGREEN_RURAL, CBSGREEN_URBAN)

# Loop over each CBS dataset
for (i in seq_along(datasets)) {
  dataset <- datasets[[i]]
  if (nrow(dataset) > 0) {
    dataset$huisleeftijd <- 2024 - dataset$year_built
    dataset$size <- as.numeric(gsub(" m²", "", dataset$size))
    dataset$size <- as.numeric(gsub(",", ".", dataset$size))
  }
  datasets[[i]] <- dataset  # Update the dataset in the list
}
# Assign updated CBS datasets back to their original variable names
assign("CBS_RURAL", datasets[[1]])
assign("CBS_URBAN", datasets[[2]])
assign("CBSGREEN_RURAL", datasets[[3]])
assign("CBSGREEN_URBAN", datasets[[4]])

# Set the directory path for saving cleaned datasets
output_dir <- "C:/Users/schel/Documents/Master thesis/Huizenmarkt 2/Data/AFTER CLEANING"

# Save each dataset
write.csv(FUNDA_RURAL, file.path(output_dir, "FUNDA_RURAL_CLEANED (2).csv"), row.names = FALSE)
write.csv(FUNDA_URBAN, file.path(output_dir, "FUNDA_URBAN_CLEANED (2).csv"), row.names = FALSE)
write.csv(CBS_RURAL, file.path(output_dir, "CBS_RURAL_CLEANED (2).csv"), row.names = FALSE)
write.csv(CBS_URBAN, file.path(output_dir, "CBS_URBAN_CLEANED (2).csv"), row.names = FALSE)
write.csv(CBSGREEN_RURAL, file.path(output_dir, "CBSGREEN_RURAL_CLEANED (2).csv"), row.names = FALSE)
write.csv(CBSGREEN_URBAN, file.path(output_dir, "CBSGREEN_URBAN_CLEANED (2).csv"), row.names = FALSE)