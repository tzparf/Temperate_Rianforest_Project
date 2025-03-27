########################## Vapour Pressure Deficit ###############################

# 4a. Calculate VPD

# VPD=SVP−AVP
# SVP is the Saturation Vapour Pressure, which is the maximum amount of moisture the air can hold at a given temperature.
# AVP is the Actual Vapour Pressure, which is the current amount of moisture in the air.
  #  SVP=0.6108×exp(17.27×T/(T+237.3)
  #  AVP=SVPxRH/100
  #  VPD=SVP-AVP
# measured in kPa

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

#load the raster packages
library(raster)
library(sp)
library(terra)
library(sf)
library(gdalUtilities)

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create Function to calculate SVP
calculate_svp <- function(temp) {
  0.6108 * exp((17.27 * temp) / (temp + 237.3))
}

########## BASELINE
# Define output paths
Base_VPD_path <- "Rasters/VPD/Baseline/"

# Define file paths
Humidity <- "Rasters/R_Humidity/Baseline/"
July <- "Rasters/July_Temp/Baseline/"

# Function to create new rasters based on criteria
create_VPD_rasters <- function(index) {
  July_RH <- raster(paste0(Humidity, "E", index, "_bRHumidity.tif"))
  July_Temp <- raster(paste0(July, "E", index, "_bJuly_Temp.tif"))
  
  # Calculate SVP, AVP, and VPD
  SVP <- calc(July_Temp, fun = calculate_svp)
  AVP <- (July_RH/100) * SVP
  VPD <- SVP - AVP
  
  # Export oceanicity rasters
  VPD_filename <- paste0(Base_VPD_path, "E", index, "_bVPD.tif")
  writeRaster(VPD, VPD_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(VPD_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_VPD_rasters(index)
}


########## FUTURE
# Define output paths
Future_VPD_path <- "Rasters/VPD/Future/"

# Define file paths
Humidity <- "Rasters/R_Humidity/Future/"
July <- "Rasters/July_Temp/Future/"

# Function to create new rasters based on criteria
create_VPD_rasters <- function(index) {
  July_RH <- raster(paste0(Humidity, "E", index, "_fRHumidity.tif"))
  July_Temp <- raster(paste0(July, "E", index, "_fJuly_Temp.tif"))
  
  # Calculate SVP, AVP, and VPD
  SVP <- calc(July_Temp, fun = calculate_svp)
  AVP <- (July_RH/100) * SVP
  VPD <- SVP - AVP
  
  # Export oceanicity rasters
  VPD_filename <- paste0(Future_VPD_path, "E", index, "_fVPD.tif")
  writeRaster(VPD, VPD_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(VPD_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_VPD_rasters(index)
}

# Checks
Checks <- raster("Rasters/VPD/Future/E10_fVPD.tif") 
plot(Checks)
plot(st_geometry(Basemap), add = TRUE)

############### END ########################
