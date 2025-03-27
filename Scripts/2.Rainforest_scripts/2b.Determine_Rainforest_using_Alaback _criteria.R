###################### Rainforest scripts (Alaback method) #########################

# 2b. Determine Rainforest using Alaback criteria

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

############# Baseline ###############
# Define the output path
output_path <- "Rasters/Rainforest/Baseline/"

### Define path for all the factors
Annual <- "Rasters/Annual_Pr/Baseline/"
Summer <- "Rasters/Percent_Summer_Pr/Baseline/"
July <- "Rasters/July_Temp/Baseline/"

# Function to create new rasters based on criteria
create_new_rasters <- function(index) {
  Annual_Pr <- raster(paste0(Annual, "E", index, "_bAnnual_Pr.tif"))
  summer_Pr <- raster(paste0(Summer, "E", index, "_bPercent_Summer_Pr.tif"))
  July_Temp <- raster(paste0(July, "E", index, "_bJuly_Temp.tif"))
  
  # Apply criteria to form new rainforest or not raster 
  new_raster <- (Annual_Pr >= 140) & (summer_Pr >= 10) & (July_Temp <= 16)
  new_raster <- as.integer(new_raster)
  
  # Export rainforest raster
  output_filename <- paste0(output_path, "E", index, "_bRainforest.tif")
  writeRaster(new_raster, output_filename, , format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(output_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_new_rasters(index)
}


############# Future ###############
# Define the output path
output_path <- "Rasters/Rainforest/Future/"

### Define path for all the factors
Annual <- "Rasters/Annual_Pr/Future/"
Summer <- "Rasters/Percent_Summer_Pr/Future/"
July <- "Rasters/July_Temp/Future/"


# Function to create new rasters based on criteria
create_new_rasters <- function(index) {
  Annual_Pr <- raster(paste0(Annual, "E", index, "_fAnnual_Pr.tif"))
  summer_Pr <- raster(paste0(Summer, "E", index, "_fPercent_Summer_Pr.tif"))
  July_Temp <- raster(paste0(July, "E", index, "_fJuly_Temp.tif"))
  
  # Apply criteria to form new rainforest or not raster 
  new_raster <- (Annual_Pr >= 140) & (summer_Pr >= 10) & (July_Temp <= 16)
  new_raster <- as.integer(new_raster)
  
  # Export rainforest raster
  output_filename <- paste0(output_path, "E", index, "_fRainforest.tif")
  writeRaster(new_raster, output_filename, , format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file
  gdal_addo(output_filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Loop through indices and create new rasters
for (index in indices) {
  create_new_rasters(index)
}

##### TEST
Checks <- raster("Rasters/Rainforest/Future/E15_fRainforest.tif") 
plot(Checks)
plot(st_geometry(Basemap), add = TRUE)

############ END ##########
