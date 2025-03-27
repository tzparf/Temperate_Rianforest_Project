##################### Oceanicity ########################

# 3d. Map Hygrothermy for all 12 Ensemble Members 

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

######### Plot output forlder
plot_Output <- "Outputs/All_ensemble_member_maps/"

####### BASELINE 
########### GET THE RASTERS WE WANT #######
# Raster Path
base_path <- "Rasters/Hygrothermy/Baseline"

# List of raster indices
indices <- c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15)

# Create list to store the raster objects
Baseline_Hygrothermy_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Baseline_Hygrothermy_path <- file.path(base_path, paste0("E", i, "_bHygro.tif"))
  Baseline_Hygrothermy_list[[paste0("E", i, "bH")]] <- raster(Baseline_Hygrothermy_path)
}

# List of raster names 
Base_Hygro_names <- c("E1bH", "E4bH", "E5bH", "E6bH", "E7bH", "E8bH", "E9bH", "E10bH", "E11bH", "E12bH", "E13bH", "E15bH")

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
cyan_to_purple <- c("#07F5F8", "#658FF8", "#5804F5", "#BA0EF4", "#FC20F5")

# Define the breaks and labels for colour palette
scale_fill_custom2 <- scale_fill_manual(
  values = cyan_to_purple,
  labels = c("100-125 ", "125-150 ", "150-175 ", "175-200 ", ">200 "),
  na.value = "transparent"
)

# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Baseline_Hygrothermy_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ##clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 100] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_Hygro_names == raster_name)]
  
  # Add colour band column
  raster_df$color_band <- cut(
    raster_df$value,
    breaks = c(100, 125, 150, 175, 200, Inf),
    labels = c("100-125", "125-150", "150-175", "175-200", "200+")
  )
  
  return(raster_df)
}

###### Process all rasters
Base_Hrasters_df <- do.call(rbind, lapply(Base_Hygro_names, process_raster))


# Ensure the rasters appear in the specified order
Base_Hrasters_df$plot_title <- factor(Base_Hrasters_df$plot_title, levels = new_titles)

# Open an A4 PDF 
pdf("Outputs/All_ensemble_member_maps/baseline_Hygrothermy_1981_2000.pdf", width = 8.27, height = 11.69)

# Plot using ggplot2
ggplot(Base_Hrasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = color_band)) + 
  scale_fill_custom2 +
  labs(title = "Baseline Index of Hygrothermy, 1981 - 2000", fill = "Amann's Hygrothermy\nIndex") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.key.size = unit(0.5, "cm"),  
    legend.text = element_text(size = 6)  
  ) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()

####### FUTURE 
########### GET THE RASTERS WE WANT #######
# Raster Path
Future_path <- "Rasters/Hygrothermy/Future"

# Create list to store the raster objects
Future_Hygrothermy_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  Future_Hygrothermy_path <- file.path(Future_path, paste0("E", i, "_fHygro.tif"))
  Future_Hygrothermy_list[[paste0("E", i, "fH")]] <- raster(Future_Hygrothermy_path)
}

# List of raster names 
Future_Hygro_names <- c("E1fH", "E4fH", "E5fH", "E6fH", "E7fH", "E8fH", "E9fH", "E10fH", "E11fH", "E12fH", "E13fH", "E15fH")

# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Future_Hygrothermy_list[[raster_name]]
  raster_clipped <- mask(raster, uk_grid_5km) ##clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 100] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_Hygro_names == raster_name)]
  
  # Add colour band column
  raster_df$color_band <- cut(
    raster_df$value,
    breaks = c(100, 125, 150, 175, 200, Inf),
    labels = c("100-125", "125-150", "150-175", "175-200", "200+")
  )
  
  return(raster_df)
}

###### Process all rasters
Future_Hrasters_df <- do.call(rbind, lapply(Future_Hygro_names, process_raster))

# Ensure the rasters appear in the specified order
Future_Hrasters_df$plot_title <- factor(Future_Hrasters_df$plot_title, levels = new_titles)

# Open an A4 PDF 
pdf("Outputs/All_ensemble_member_maps/Future_Hygrothermy_2061_2080.pdf", width = 8.27, height = 11.69)

# Plot using ggplot2
ggplot(Future_Hrasters_df) +
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = color_band)) + 
  scale_fill_custom2 +
  labs(title = "Future Index of Hygrothermy, 2061 - 2080", fill = "Amann's Hygrothermy\nIndex") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),  # Remove grid lines
    legend.key.size = unit(0.5, "cm"),  
    legend.text = element_text(size = 6)  
  ) +
  facet_wrap(~plot_title, ncol = 4) +
  coord_sf()

# Close the PDF device
dev.off()

######################## END ##################################
