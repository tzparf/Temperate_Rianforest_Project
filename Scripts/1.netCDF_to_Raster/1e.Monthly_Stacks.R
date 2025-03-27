############## net CDF to Raster ##########

# 1e. Mean Monthly Temperature

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

###### Monthly Temperature ##########

# BASELINE
# Open the netCDF file
nc_file9 <- nc_open("Inputs/NetCDFs/1981-2000_Monthly_Temp.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file9, "projection_x_coordinate")
lat <- ncvar_get(nc_file9, "projection_y_coordinate")

# Get the time variable
time <- ncvar_get(nc_file9, "time")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file9, "ensemble_member")

# Create an empty list to store the raster stacks for each ensemble member
all_ensemble_timeslices <- list()

# Loop through each ensemble member
for (ensemble_member_name in ensemble_members) {
  # Get the index of the current ensemble member
  ensemble_member_index <- which(ensemble_members == ensemble_member_name)
  
  # Get the tas variable for the current ensemble member
  tas <- ncvar_get(nc_file9, "tas")[,,,ensemble_member_index]
  
  # Initialize an empty list to store the rasters for the current ensemble member
  timeslice <- list()
  
  # Loop through each time period and store the raster in the list
  for (i in 1:dim(tas)[3]) {
    # Create a raster for the current time period
    r <- raster(tas[,,i], xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
    crs(r) <- CRS("+init=epsg:27700")  # BNG (OSGB 1936)
    
    # Rotate the values 90 degrees anticlockwise using apply
    rotated_values <- apply(t(as.matrix(r)), 2, rev)
    r <- raster(rotated_values, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
    crs(r) <- CRS("+init=epsg:27700")
    
    # Add the raster to the list
    timeslice[[i]] <- r
  }
  
  # Stack the rasters for the current ensemble member
  all_ensemble_timeslices[[ensemble_member_name]] <- stack(timeslice)
}

# Close the netCDF file
nc_close(nc_file9)

# Function to calculate monthly means
calculate_monthly_means <- function(timeslices_stack) {
  monthly_means <- list()
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  for (month_index in 1:12) {
    indices <- seq(month_index, by = 12, length.out = floor(length(timeslices_stack) / 12))
    # Ensure indices remain those for ensmble members
    valid_indices <- indices[indices <= nlayers(timeslices_stack)]
    if (length(valid_indices) > 0) {
      monthly_rasters <- timeslices_stack[[valid_indices]]
      monthly_mean <- calc(monthly_rasters, mean)
      monthly_means[[months[month_index]]] <- monthly_mean
    }
  }
  
  return(stack(monthly_means))
}

# Calculate monthly means for each ensemble member
all_monthly_means <- lapply(all_ensemble_timeslices, function(x) {
  if (!is.null(x)) {
    calculate_monthly_means(x)
  } else {
    NULL
  }
})



########################################################################################################### EXPORT JULY RASTERS ######################################

# Output Folder
output_dir <- "Rasters/July_Temp/Baseline/"

# Export each ensemble member's monthly means
for (i in seq_along(all_monthly_means)) {
  if (!is.null(all_monthly_means[[i]])) {
    # Extract the July layer
    July_raster <- all_monthly_means[[i]][["Jul"]]
    
    # Define the output file path
    filename <- file.path(output_dir, paste0("E", i, "_bJuly_Temp.tif"))
    
    # Save the raster to a BigTIFF file and generate a .tfw file
    writeRaster(July_raster, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
    
    # Generate pyramid (.ovr) file
    gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  }
}


######################## Export Diurnal Temperature Range ##################

# Output Folder
output_dir2 <- "Rasters/Temp_Range/Baseline/"

# Create a list to store the range for each ensemble member
all_monthly_ranges <- list()

# Calcualte each ensemble member's monthly means
for (i in seq_along(all_monthly_means)) {
  if (!is.null(all_monthly_means[[i]])) {
    # Calculate the maximum value across all monthly mean rasters
    max_raster <- calc(all_monthly_means[[i]], max, na.rm = TRUE)
    
    # Calculate the minimum value across all monthly mean rasters
    min_raster <- calc(all_monthly_means[[i]], min, na.rm = TRUE)
    
    # Calculate the range
    range_raster <- max_raster - min_raster
    
    # Store the range raster in the list
    all_monthly_ranges[[i]] <- range_raster
  }
}

# Export each range raster and save it to a file
for (i in seq_along(all_monthly_ranges)) {
  if (!is.null(all_monthly_ranges[[i]])) {
    # Define the output file path
    filename <- file.path(output_dir2, paste0("E", i, "_bTemp_Range.tif"))
    
    # Save the range raster to a file
    writeRaster(all_monthly_ranges[[i]], filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
    
    # Generate pyramid (.ovr) file
    gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  }
}

################# SIDE QUEST: 
##### REPEAT BUT GET JULY STANDARD DEVIATION INSTEAD OF MEAN ###################
# Function to calculate monthly means
calculate_monthly_sds <- function(timeslices_stack) {
  monthly_means <- list()
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  for (month_index in 1:12) {
    indices <- seq(month_index, by = 12, length.out = floor(length(timeslices_stack) / 12))
    # Ensure indices remain those for ensmble members
    valid_indices <- indices[indices <= nlayers(timeslices_stack)]
    if (length(valid_indices) > 0) {
      monthly_rasters <- timeslices_stack[[valid_indices]]
      monthly_mean <- calc(monthly_rasters, sd) ####CHANGE TO SD
      monthly_means[[months[month_index]]] <- monthly_mean
    }
  }
  
  return(stack(monthly_means))
}

# Calculate monthly means for each ensemble member
all_monthly_sds <- lapply(all_ensemble_timeslices, function(x) {
  if (!is.null(x)) {
    calculate_monthly_sds(x)
  } else {
    NULL
  }
})

## Now we need July, lets export actual rasters to July raster/baseline_sd
# Output Folder
output_dir <- "Rasters/July_Temp/Baseline_sd/"

# Export each ensemble member's monthly means
for (i in seq_along(all_monthly_sds)) {
  if (!is.null(all_monthly_sds[[i]])) {
    # Extract the July layer
    July_raster <- all_monthly_sds[[i]][["Jul"]]
    
    # Define the output file path
    filename <- file.path(output_dir, paste0("E", i, "_bsd.tif"))
    
    # Save the raster to a BigTIFF file and generate a .tfw file
    writeRaster(July_raster, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
    
    # Generate pyramid (.ovr) file
    gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  }
}

###############################################################################
#FUTURE
# Open the netCDF file
nc_file10 <- nc_open("Inputs/NetCDFs/2061-2080_Monthly_Temp.nc")

# Get the projection coordinates
lon <- ncvar_get(nc_file10, "projection_x_coordinate")
lat <- ncvar_get(nc_file10, "projection_y_coordinate")

# Get the time variable
time <- ncvar_get(nc_file10, "time")

# Get the ensemble member names
ensemble_members <- ncvar_get(nc_file10, "ensemble_member")

# Create an empty list to store the raster stacks for each ensemble member
all_ensemble_timeslices <- list()

# Loop through each ensemble member
for (ensemble_member_name in ensemble_members) {
  # Get the index of the current ensemble member
  ensemble_member_index <- which(ensemble_members == ensemble_member_name)
  
  # Get the tas variable for the current ensemble member
  tas <- ncvar_get(nc_file10, "tas")[,,,ensemble_member_index]
  
  # Initialize an empty list to store the rasters for the current ensemble member
  timeslice <- list()
  
  # Loop through each time period and store the raster in the list
  for (i in 1:dim(tas)[3]) {
    # Create a raster for the current time period
    r <- raster(tas[,,i], xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
    crs(r) <- CRS("+init=epsg:27700")  # BNG (OSGB 1936)
    
    # Rotate the values 90 degrees anticlockwise using apply
    rotated_values <- apply(t(as.matrix(r)), 2, rev)
    r <- raster(rotated_values, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat))
    crs(r) <- CRS("+init=epsg:27700")
    
    # Add the raster to the list
    timeslice[[i]] <- r
  }
  
  # Stack the rasters for the current ensemble member
  all_ensemble_timeslices[[ensemble_member_name]] <- stack(timeslice)
}

# Close the netCDF file
nc_close(nc_file10)

# Function to calculate monthly means
calculate_monthly_means <- function(timeslices_stack) {
  monthly_means <- list()
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  for (month_index in 1:12) {
    indices <- seq(month_index, by = 12, length.out = floor(length(timeslices_stack) / 12))
    # Ensure indices remain those for ensmble members
    valid_indices <- indices[indices <= nlayers(timeslices_stack)]
    if (length(valid_indices) > 0) {
      monthly_rasters <- timeslices_stack[[valid_indices]]
      monthly_mean <- calc(monthly_rasters, mean)
      monthly_means[[months[month_index]]] <- monthly_mean
    }
  }
  
  return(stack(monthly_means))
}

# Calculate monthly means for each ensemble member
all_monthly_means <- lapply(all_ensemble_timeslices, function(x) {
  if (!is.null(x)) {
    calculate_monthly_means(x)
  } else {
    NULL
  }
})

########################################################################################################### EXPORT JULY RASTERS ######################################

# Output Folder
output_dir <- "Rasters/July_Temp/Future/"

# Export each ensemble member's monthly means
for (i in seq_along(all_monthly_means)) {
  if (!is.null(all_monthly_means[[i]])) {
    # Extract the July layer
    July_raster <- all_monthly_means[[i]][["Jul"]]
    
    # Define the output file path
    filename <- file.path(output_dir, paste0("E", i, "_fJuly_Temp.tif"))
    
    # Save the raster to a BigTIFF file and generate a .tfw file
    writeRaster(July_raster, filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
    
    # Generate pyramid (.ovr) file
    gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  }
}


######################## Export Diurnal Temperature Range ##################

# Output Folder
output_dir2 <- "Rasters/Temp_Range/Future/"

# Create a list to store the range for each ensemble member
all_monthly_ranges <- list()

# Calcualte each ensemble member's monthly means
for (i in seq_along(all_monthly_means)) {
  if (!is.null(all_monthly_means[[i]])) {
    # Calculate the maximum value across all monthly mean rasters
    max_raster <- calc(all_monthly_means[[i]], max, na.rm = TRUE)
    
    # Calculate the minimum value across all monthly mean rasters
    min_raster <- calc(all_monthly_means[[i]], min, na.rm = TRUE)
    
    # Calculate the range
    range_raster <- max_raster - min_raster
    
    # Store the range raster in the list
    all_monthly_ranges[[i]] <- range_raster
  }
}

# Export each range raster and save it to a file
for (i in seq_along(all_monthly_ranges)) {
  if (!is.null(all_monthly_ranges[[i]])) {
    # Define the output file path
    filename <- file.path(output_dir2, paste0("E", i, "_fTemp_Range.tif"))
    
    # Save the range raster to a file
    writeRaster(all_monthly_ranges[[i]], filename = filename, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)
    
    # Generate pyramid (.ovr) file
    gdal_addo(filename, r = "average", overviews = c(2, 4, 8, 16), method = "average")
  }
}

###TRIAL
DIDITWORK <- raster("Rasters/Temp_Range/Future/E10_fTemp_Range.tif") 
plot(DIDITWORK)
plot(st_geometry(Basemap), add = TRUE) #Load via Script 1a
print(DIDITWORK)

########### END ##############
