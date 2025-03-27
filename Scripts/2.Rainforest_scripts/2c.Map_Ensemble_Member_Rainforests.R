##################### Rainforest scripts (Alaback method) ########################

# 2c. Map All 12 Ensemble Member Rainforests

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

#load the raster packages
library(raster)
library(sp)
library(terra)
library(sf)
library(gdalUtilities)
library(colorspace)
library(dplyr)
library(ggplot2)
library(rlang)

######### Plot output forlder
plot_Output <- "Outputs/All_ensemble_member_maps/"

####### BASELINE 
########### GET THE RASTERS WE WANT #######
# Raster Path
base_path <- "Rasters/Rainforest/Baseline"

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create list to store the raster objects
Baseline_rainforest_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Baseline_rainforest_path <- file.path(base_path, paste0("E", i, "_bRainforest.tif"))
  Baseline_rainforest_list[[paste0("E", i, "bR")]] <- raster(Baseline_rainforest_path)
}

########## PLOT MAPS #########
### Load basemaps
GBR_Country <- st_read("Inputs/BASEMAPS/GBR_Country.shp")
uk_grid_5km <- st_read("Inputs/BASEMAPS/uk_grid_5km.shp")

### Transform to British National Grid (BNG)
GBR_Country <- st_transform(GBR_Country, crs = 27700)
uk_grid_5km <- st_transform(uk_grid_5km, crs = 27700)

## Load the rasters
# List of raster names 
Base_raster_names <- c("E1bR", "E4bR", "E5bR", "E6bR", "E7bR", "E8bR", "E9bR", "E10bR", "E11bR", "E12bR", "E13bR", "E15bR")

# New titles for the rasters for use in plot
new_titles <- c("01", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "15")

# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Baseline_rainforest_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x != 1] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_raster_names == raster_name)]
  
  return(raster_df)
}

###### Process all rasters
base_rainfroest_rasters_df <- do.call(rbind, lapply(Base_raster_names, process_raster))

# Ensure the rasters appear in the specified order
base_rainfroest_rasters_df$plot_title <- factor(base_rainfroest_rasters_df$plot_title, levels = new_titles)

# Open an A4 PDF
pdf("Outputs/All_ensemble_member_maps/baseline_rainforest_1981_2000.pdf", width = 8.27, height = 11.69)

# Create the plot
ggplot(base_rainfroest_rasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y), fill = "#228B22") + 
  labs(title = "Baseline Rainforest, 1981 - 2000") + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()


####### FUTURE 
########### GET THE RASTERS WE WANT #######
# Raster Path
Future_path <- "Rasters/Rainforest/Future"

# Create list to store the raster objects
Future_rainforest_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Future_rainforest_path <- file.path(Future_path, paste0("E", i, "_fRainforest.tif"))
  Future_rainforest_list[[paste0("E", i, "fR")]] <- raster(Future_rainforest_path)
}

########## PLOT MAPS #########
## Load the rasters
# List of raster names 
Future_raster_names <- c("E1fR", "E4fR", "E5fR", "E6fR", "E7fR", "E8fR", "E9fR", "E10fR", "E11fR", "E12fR", "E13fR", "E15fR")

# New titles for the rasters for use in plot
new_titles <- c("01", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "15")

# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Future_rainforest_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x != 1] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_raster_names == raster_name)]
  
  return(raster_df)
}

###### Process all rasters
Future_rainfroest_rasters_df <- do.call(rbind, lapply(Future_raster_names, process_raster))

# Ensure the rasters appear in the specified order
Future_rainfroest_rasters_df$plot_title <- factor(Future_rainfroest_rasters_df$plot_title, levels = new_titles)

# Open an A4 PDF 
pdf("Outputs/All_ensemble_member_maps/future_rainforest_2061_2080.pdf", width = 8.27, height = 11.69)

# Plot using ggplot2
ggplot(Future_rainfroest_rasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y), fill = "#228B22") + 
  labs(title = "Future Rainforest, 2060 - 2080") + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.position = "none"  # Remove legend
  ) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()
########### END #######################
