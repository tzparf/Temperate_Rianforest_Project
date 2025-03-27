##################### Oceanicity ########################

# 3b. Map Oceanicty for all 12 Ensemble Members 

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
base_path <- "Rasters/Oceanicity/Baseline"

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create list to store the raster objects
Baseline_oceanicity_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Baseline_oceanicty_path <- file.path(base_path, paste0("E", i, "_bOceanicity.tif"))
  Baseline_oceanicity_list[[paste0("E", i, "bO")]] <- raster(Baseline_oceanicty_path)
}

# List of raster names 
Base_raster_names <- c("E1bO", "E4bO", "E5bO", "E6bO", "E7bO", "E8bO", "E9bO", "E10bO", "E11bO", "E12bO", "E13bO", "E15bO")

# New titles for the rasters for use in plot
new_titles <- c("01", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "15")

########## PLOT MAPS #########
### Load basemaps
GBR_Country <- st_read("Inputs/BASEMAPS/GBR_Country.shp")
uk_grid_5km <- st_read("Inputs/BASEMAPS/uk_grid_5km.shp")

### Transform to British National Grid (BNG)
GBR_Country <- st_transform(GBR_Country, crs = 27700)
uk_grid_5km <- st_transform(uk_grid_5km, crs = 27700)

# Create colour palette
Yellow_blue <- c("#FFFFD9", "#C7E9B4", "#41B6C4", "#225EA8", "#081D58")

# Define the breaks and labels for colour palette
scale_fill_custom <- scale_fill_manual(
  values = Yellow_blue,
  labels = c("5-10 ", "10-15 ", "15-20 ", "20-25 ", ">25 "),
  na.value = "transparent",  
  drop = TRUE
)

# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Baseline_oceanicity_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ##clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 5] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_raster_names == raster_name)]
  
  
  # Add colour band column
  raster_df$color_band <- cut(
    raster_df$value,
    breaks = c(5, 10, 15, 20, 25, Inf),
    labels = c("5-10", "10-15", "15-20", "20-25", ">25")
  )
  
  return(raster_df)
}

###### Process all rasters
Base_Orasters_df <- do.call(rbind, lapply(Base_raster_names, process_raster))


# Ensure the rasters appear in the specified order
Base_Orasters_df$plot_title <- factor(Base_Orasters_df$plot_title, levels = new_titles)

# Open an A4 PDF 
pdf("Outputs/All_ensemble_member_maps/baseline_oceanicity_1981_2000.pdf", width = 8.27, height = 11.69)

# Plot using ggplot2
ggplot(Base_Orasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = color_band)) + 
  scale_fill_custom +
  labs(title = "Baseline Oceanicity, 1981 - 2000", fill = "Oceanicity\n(Av. Annual Precipitation (mm)/\nTemperature Range (°C))") + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.key.size = unit(0.5, "cm"),  
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)  # Set the size for the main title
  ) +
  guides(fill = guide_legend(title = expression(atop("Oceanicity", 
                                                     atop(italic("(Av. Annual Precipitation (mm)/"), 
                                                          italic("Temperature Range(°C))")))))) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()


####### FUTURE 
########### GET THE RASTERS WE WANT #######
# Raster Path
future_path <- "Rasters/Oceanicity/Future"

# Create list to store the raster objects
Future_oceanicity_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Baseline_oceanicty_path <- file.path(future_path, paste0("E", i, "_fOceanicity.tif"))
  Future_oceanicity_list[[paste0("E", i, "fO")]] <- raster(Baseline_oceanicty_path)
}

# List of raster names 
Future_raster_names <- c("E1fO", "E4fO", "E5fO", "E6fO", "E7fO", "E8fO", "E9fO", "E10fO", "E11fO", "E12fO", "E13fO", "E15fO")

########## PLOT MAPS #########
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Future_oceanicity_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ##clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 5] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_raster_names == raster_name)]
  
  
  # Add colour band column
  raster_df$color_band <- cut(
    raster_df$value,
    breaks = c(5, 10, 15, 20, 25, Inf),
    labels = c("5-10", "10-15", "15-20", "20-25", "25+")
  )
  
  return(raster_df)
}

###### Process all rasters
Future_Orasters_df <- do.call(rbind, lapply(Future_raster_names, process_raster))

# Ensure the rasters appear in the specified order
Future_Orasters_df$plot_title <- factor(Future_Orasters_df$plot_title, levels = new_titles)

# Open an A4 PDF 
pdf("Outputs/All_ensemble_member_maps/Future_oceanicity_2061_2080.pdf", width = 8.27, height = 11.69)

# Plot using ggplot2
ggplot(Future_Orasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = color_band)) + 
  scale_fill_custom +
  labs(title = "Future Oceanicity, 2061 - 2080", fill = "Oceanicity\n(Av. Annual Rain/\nTemperature Range)") + 
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.key.size = unit(0.5, "cm"),  
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)  # Set the size for the main title
  ) +
  guides(fill = guide_legend(title = expression(atop("Oceanicity", 
                                                     atop(italic("(Av. Annual Rain/"), 
                                                          italic("Temperature Range)")))))) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()

################### END #################
