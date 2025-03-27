############## net CDF to Raster ##########

# 1a. Summer Precipitation

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

### Load packages
library(raster)
library(ncdf4)
library(gdalUtilities)
library(sp)
library(terra)
library(sf)

###### Summer Rainfall ##########

# Define the output paths
Baseline_output_path <- "Rasters/Summer_Pr/Baseline/"
Future_output_path <- "Rasters/Summer_Pr/Future/"

#BASELINE
# Open the netCDF file
nc_file1 <- nc_open("Inputs/NetCDFs/1981-2000_Summer_Pr.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file1, "projection_x_coordinate")
lat <- ncvar_get(nc_file1, "projection_y_coordinate")

# Get the precipitation variable data
variable_data <- ncvar_get(nc_file1, "pr")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file1, "ensemble_member")

# Calculate the mean for each ensemble member across the 20 years
mean_values <- apply(variable_data, c(1, 2, 4), mean, na.rm = TRUE)

# Create a list to store the raster objects
raster_list <- list()

for (i in 1:dim(mean_values)[3]) {
  # Extract the mean values for the current ensemble member
  mean_raster <- mean_values[,,i]
  
  # Rotate the values 90 degrees anticlockwise (see notes)
  mean_raster <- apply(t(mean_raster), 2, rev)
  
  # Multiply by 9.2 For conversion total Summer rainfall and to cm from mm/day (see notes script in Admin folder for detail)
  mean_raster <- mean_raster * 9.2
  
  # Create a raster object
  r <- raster(mean_raster)
  
  # Define the coordinate reference system (CRS)
  crs(r) <- CRS("EPSG:27700")  # BNG (OSGB 1936)
  
  # Define the extent using the projection coordinates
  extent(r) <- extent(min(lon), max(lon), min(lat), max(lat))
  
  # Store the raster object in the list
  raster_list[[i]] <- r
}

# Write the raster objects to files
for (i in 1:length(raster_list)) {
  # Get the raster object
  r <- raster_list[[i]]
  
  # Define the filename using the ensemble member name
  filename <- paste0(Baseline_output_path, "E", ensemble_members[i], "_bSummer_Pr.tif")
  
  # Save the raster to a BigTIFF file and generate a .tfw file
  writeRaster(r, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file 
  gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Close the netCDF file
nc_close(nc_file1)


#FUTURE (Repeat of above code with 2061-2080 data)
# Open the netCDF file
nc_file2 <- nc_open("Inputs/NetCDFs/2061-2080_Summer_Pr.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file2, "projection_x_coordinate")
lat <- ncvar_get(nc_file2, "projection_y_coordinate")

# Get the precipitation variable data
variable_data <- ncvar_get(nc_file2, "pr")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file2, "ensemble_member")

# Calculate the mean for each ensemble member across the 20 years
mean_values <- apply(variable_data, c(1, 2, 4), mean, na.rm = TRUE)

# Create a list to store the raster objects
raster_list <- list()

for (i in 1:dim(mean_values)[3]) {
  # Extract the mean values for the current ensemble member
  mean_raster <- mean_values[,,i]
  
  # Rotate the values 90 degrees anticlockwise
  mean_raster <- apply(t(mean_raster), 2, rev)
  
  # Multiply the raster values by 9.2 (see notes script in Admin folder for detail)
  mean_raster <- mean_raster * 9.2
  
  # Create a raster object
  r <- raster(mean_raster)
  
  # Define the coordinate reference system (CRS)
  crs(r) <- CRS("EPSG:27700")  # BNG (OSGB 1936)
  
  # Define the extent using the projection coordinates
  extent(r) <- extent(min(lon), max(lon), min(lat), max(lat))
  
  # Store the raster object in the list
  raster_list[[i]] <- r
}

# Write the raster objects to files
for (i in 1:length(raster_list)) {
  # Get the raster object
  r <- raster_list[[i]]
  
  # Define the filename using the ensemble member name
  filename <- paste0(Future_output_path, "E", ensemble_members[i], "_fSummer_Pr.tif")
  
  # Save the raster to a BigTIFF file and generate a .tfw file
  writeRaster(r, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file 
  gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Close the netCDF file
nc_close(nc_file2)

###TRIAL
DIDITWORK <- raster("Rasters/Summer_Pr/Baseline/E1_bSummer_Pr.tif") 
Basemap <- st_read("Inputs/BASEMAPS/GBR_Country.shp")
Basemap <- st_transform(Basemap, crs = 27700)
plot(DIDITWORK)
plot(st_geometry(Basemap), add = TRUE)
print(DIDITWORK)

#### END ##############
