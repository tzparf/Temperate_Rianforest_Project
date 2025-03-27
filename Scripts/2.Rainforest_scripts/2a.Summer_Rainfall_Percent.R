###################### Rainforest scripts (Alaback method) #########################

# 2a. Summer Rainfall %

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

### Load packages
library(raster)
library(gdalUtilities)
library(sp)
library(terra)
library(sf)

####This script uses the summer rainfall rasters created in script 1a and annual rainfall rasters created in script 1b to get average summer rainfall as a %
############## BASELINE
# Define the output path
output_path <- "Rasters/Percent_Summer_Pr/Baseline/"

### Define path for annual and summer rainfall
Annual_location <- "Rasters/Annual_Pr/Baseline"
Summer_location <- "Rasters/Summer_Pr/Baseline"

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create raster lists
annuals <- file.path(Annual_location, paste0("E", indices, "_bAnnual_Pr.tif"))
summers <- file.path(Summer_location, paste0("E", indices, "_bSummer_Pr.tif"))

# Loop through each index and perform calculation
for (i in seq_along(indices)) {
  # Load rasters
  annual_raster <- raster(annuals[i])
  summer_raster <- raster(summers[i])
  
  # Get summer as percentage of total
  result <- 100 * summer_raster / annual_raster
  
  # Define the output filename
  output_filename <- file.path(output_path, paste0("E", indices[i], "_bPercent_Summer_Pr.tif"))
  
  # Save the result as a new raster file
  writeRaster(result, filename = output_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file 
  gdal_addo(output_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}


############## FUTURE
# Define the output path
output_path <- "Rasters/Percent_Summer_Pr/Future/"

### Define path for annual and summer rainfall
Annual_location <- "Rasters/Annual_Pr/Future"
Summer_location <- "Rasters/Summer_Pr/Future"

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create raster lists
annuals <- file.path(Annual_location, paste0("E", indices, "_fAnnual_Pr.tif"))
summers <- file.path(Summer_location, paste0("E", indices, "_fSummer_Pr.tif"))

# Loop through each index and perform calculation
for (i in seq_along(indices)) {
  # Load rasters
  annual_raster <- raster(annuals[i])
  summer_raster <- raster(summers[i])
  
  # Get summer as percentage of total
  result <- 100 * summer_raster / annual_raster
  
  # Define the output filename
  output_filename <- file.path(output_path, paste0("E", indices[i], "_fPercent_Summer_Pr.tif"))
  
  # Save the result as a new raster file
  writeRaster(result, filename = output_filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file 
  gdal_addo(output_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

##############END##############
