##################### Oceanicity ########################

# 3a. Calculate Oceanicity and Hygrothermy

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

#BASLEINE
# Define output paths
O_path <- "Rasters/Oceanicity/Baseline/"
H_path <- "Rasters/Hygrothermy/Baseline/"

# Define file paths
Annual <- "Rasters/Annual_Pr/Baseline/"
ATemp <- "Rasters/AvTemp/Baseline/"
Range <- "Rasters/Temp_Range/Baseline/"

# Function to create new rasters based on criteria
create_new_rasters <- function(index) {
  Annual_Pr <- raster(paste0(Annual, "E", index, "_bAnnual_Pr.tif"))
  Annual_tas <- raster(paste0(ATemp, "E", index, "_bAvTemp.tif"))
  TempRange <- raster(paste0(Range, "E", index, "_bTemp_Range.tif"))
  
  # Apply criteria to form new rasters
  oceanicity <- Annual_Pr / TempRange
  Hygrothermy <- oceanicity * Annual_tas
  
  # Export oceanicity rasters
  o_filename <- paste0(O_path, "E", index, "_bOceanicity.tif")
  writeRaster(oceanicity, o_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(o_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  
  # Export hygrothermy rasters
  h_filename <- paste0(H_path, "E", index, "_bHygro.tif")
  writeRaster(Hygrothermy, h_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(h_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_new_rasters(index)
}


#FUTURE
# Define output paths
O_path <- "Rasters/Oceanicity/Future/"
H_path <- "Rasters/Hygrothermy/Future/"

# Define file paths
Annual <- "Rasters/Annual_Pr/Future/"
ATemp <- "Rasters/AvTemp/Future/"
Range <- "Rasters/Temp_Range/Future/"

# Function to create new rasters based on criteria
create_new_rasters <- function(index) {
  Annual_Pr <- raster(paste0(Annual, "E", index, "_fAnnual_Pr.tif"))
  Annual_tas <- raster(paste0(ATemp, "E", index, "_fAvTemp.tif"))
  TempRange <- raster(paste0(Range, "E", index, "_fTemp_Range.tif"))
  
  # Apply criteria to form new rasters
  oceanicity <- Annual_Pr / TempRange
  Hygrothermy <- oceanicity * Annual_tas
  
  # Export oceanicity rasters
  o_filename <- paste0(O_path, "E", index, "_fOceanicity.tif")
  writeRaster(oceanicity, o_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(o_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  
  # Export hygrothermy rasters
  h_filename <- paste0(H_path, "E", index, "_fHygro.tif")
  writeRaster(Hygrothermy, h_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(h_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_new_rasters(index)
}

# Did it run
Checks <- raster("Rasters/Hygrothermy/Baseline/E10_bHygro.tif") 
plot(Checks)
plot(st_geometry(Basemap), add = TRUE)

##################### END ############################
