#################### Rainforest scripts (Alaback method) ########################

# 2f. Rainforest Loss. Part 3

# This script calculates standard deviation of the July temperature, to calculate the areas that move 3 standard deviations away from their baseline in the future.

##################### OK, we need to get standard deviation of July ##############
###############################################################################
# GT SDs From Rasters/July_Temp/Baseline_sd folder
SD_path <- "Rasters/July_Temp/Baseline_sd/"
July_path <- "Rasters/July_Temp/Future/"

# Function to create new rasters based on criteria
JT_change_function <- function(indices, mean_July_Temp) {
  # Initialize a list to store the results
  JT_change_list <- list()
  
  # Loop through the specified indices
  for (index in indices) {
    # Read the rasters
    SD <- raster(paste0(SD_path, "E", index, "_bsd.tif"))
    FutureJ <- raster(paste0(July_path, "E", index, "_fJuly_Temp.tif"))
    
    # Apply criteria to form new rasters
    JT_change <- FutureJ - ((SD * 3) + Mean_July_Temp)
    
    # Store the result in the list
    JT_change_list[[index]] <- JT_change
  }
  
  # Return the list of new rasters
  return(JT_change_list)
}

# run function
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)
JT_change <- JT_change_function(indices, mean_July_Temp)

# Function to rename rasters in the list
rename_rasters <- function(raster_list, new_names) {
  # Check if the length of new names matches the length of the raster list
  if (length(new_names) != length(raster_list)) {
    stop("The length of new names must match the length of the raster list.")
  }
  
  # Assign new names to the rasters in the list
  names(raster_list) <- new_names
  
  return(raster_list)
}

# Example usage
SD_raster_names <- c("E1_3SD", "E2_3SD", "E3_3SD","E4_3SD", "E5_3SD", "E6_3SD", "E7_3SD", "E8_3SD", "E9_3SD", "E10_3SD", "E11_3SD", "E12_3SD", "E13_3SD", "E14_3SD", "E15_3SD")
JT_change <- rename_rasters(JT_change, SD_raster_names)

# Function to remove blank rasters from the list
remove_blank_rasters <- function(raster_list, blank_indices) {
  # Remove the blank rasters based on the specified indices
  raster_list <- raster_list[!names(raster_list) %in% blank_indices]
  
  return(raster_list)
}

# Run function
blank_indices <- c("E2_3SD", "E3_3SD", "E14_3SD")
JT_change <- remove_blank_rasters(JT_change, blank_indices)
SD_raster_names <- c("E1_3SD", "E4_3SD", "E5_3SD", "E6_3SD", "E7_3SD", "E8_3SD", "E9_3SD", "E10_3SD", "E11_3SD", "E12_3SD", "E13_3SD", "E15_3SD")

# Load base mean rainforest extent shapefile
UK_rf_area <- st_read("Inputs/BASEMAPS/mean_rf_area.shp")
UK_rf_area <- st_transform(UK_rf_area, crs = 27700) 

# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- JT_change[[raster_name]]
  raster_clipped <- mask(raster, UK_rf_area)  # Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x >= 0] <- NA; return(x) })  # Filter out rainforest values
  
  # Check if the filtered raster has any non-NA values
  if (sum(!is.na(values(raster_filtered))) == 0) {
    return(NULL)  # Return NULL if there are no values to be filtered out
  }
  
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(SD_raster_names == raster_name)]
  
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
July_change <- lapply(SD_raster_names, process_raster)
July_change <- July_change[!sapply(July_change, is.null)]

# xtract raster data frames and cell counts
cell_counts <- sapply(July_change, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- SD_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_July_change <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_July_change)
# Remember there were 3 blank ones removed!!!
##### END
