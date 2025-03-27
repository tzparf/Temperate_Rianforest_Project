############## net CDF to Raster ##########

# 1d. Mean Annual Temperautre

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

###### Annual Temperature ##########

# Define the output paths
Baseline_output_path <- "Rasters/AvTemp/Baseline/"
Future_output_path <- "Rasters/AvTemp/Future/"

#BASELINE
# Open the netCDF file
nc_file7 <- nc_open("Inputs/NetCDFs/1981-2000_AvTemp.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file7, "projection_x_coordinate")
lat <- ncvar_get(nc_file7, "projection_y_coordinate")

# Get the variable data
variable_data <- ncvar_get(nc_file7, "tas")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file7, "ensemble_member")

# Calculate the mean for each ensemble member across the 20 years
mean_values <- apply(variable_data, c(1, 2, 4), mean, na.rm = TRUE)

# Create a list to store the raster objects
raster_list <- list()

for (i in 1:dim(mean_values)[3]) {
  # Extract the mean values for the current ensemble member
  mean_raster <- mean_values[,,i]
  
  # Rotate the values 90 degrees anticlockwise
  mean_raster <- apply(t(mean_raster), 2, rev)
  
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
  filename <- paste0(Baseline_output_path, "E", ensemble_members[i], "_bAvTemp.tif")
  
  # Save the raster to a BigTIFF file and generate a .tfw file
  writeRaster(r, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file using system call
  gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Close the netCDF file
nc_close(nc_file7)


#FUTURE
# Open the netCDF file
nc_file8 <- nc_open("Inputs/NetCDFs/2061-2080_AvTemp.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file8, "projection_x_coordinate")
lat <- ncvar_get(nc_file8, "projection_y_coordinate")

# Get the variable data
variable_data <- ncvar_get(nc_file8, "tas")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file8, "ensemble_member")

# Calculate the mean for each ensemble member across the 20 years
mean_values <- apply(variable_data, c(1, 2, 4), mean, na.rm = TRUE)

# Create a list to store the raster objects
raster_list <- list()

for (i in 1:dim(mean_values)[3]) {
  # Extract the mean values for the current ensemble member
  mean_raster <- mean_values[,,i]
  
  # Rotate the values 90 degrees anticlockwise
  mean_raster <- apply(t(mean_raster), 2, rev)
  
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
  filename <- paste0(Future_output_path, "E", ensemble_members[i], "_fAvTemp.tif")
  
  # Save the raster to a BigTIFF file and generate a .tfw file
  writeRaster(r, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
  
  # Generate pyramid (.ovr) file using system call
  gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
}

# Close the netCDF file
nc_close(nc_file8)

###TRIAL
DIDITWORK <- raster("Rasters/AvTemp/Baseline/E13_bAvTemp.tif") 
plot(DIDITWORK)
plot(st_geometry(Basemap), add = TRUE)
print(DIDITWORK)

########## END ##############
