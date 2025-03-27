##################### Rainforest scripts (Alaback method) ########################

# 2d. Rainforest Loss. Part 1

# In this script cell counts are used to determine an area of the UK that is rainforest (in both baseline and future) and order Ensemble Members by Rainforest Extent to determine range of projections

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

#load the raster packages
library(raster)
library(sp)
library(terra)
library(sf)
library(gdalUtilities)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rlang)
library(colorspace)
library(purrr)
library(gridExtra)
library(grid)

#####  ORDER RASTERS BY EXTENT/ CELL COUNT TO FIND MEDIAN, 10th AND 90th PERCENTILE OF RAINFOREST IN THE MODEL #####
### Load basemaps
Britain_area <- st_read("Inputs/BASEMAPS/Britain_area.shp")
UK_area <- st_read("Inputs/BASEMAPS/UK_area.shp")

### Transform to British National Grid (BNG)
Britain_area <- st_transform(Britain_area, crs = 27700)
UK_area <- st_transform(UK_area, crs = 27700)

# BASELINE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Baseline_rainforest_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x != 1] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
BaseUK_rainforests_extent <- lapply(Base_raster_names, process_raster)

# Extract raster data frames and cell counts
cell_counts <- sapply(BaseUK_rainforests_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Base_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_BaseUK_rainforests_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_BaseUK_rainforests_extent)

######### FUTURE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Future_rainforest_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x != 1] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
FutureUK_rainforests_extent <- lapply(Future_raster_names, process_raster)

# Extract raster data frames and cell counts
cell_counts <- sapply(FutureUK_rainforests_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Future_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_FutureUK_rainforests_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_FutureUK_rainforests_extent)


##################### Calculate a mean Baseline Rainforest #####################
# Define names 
patternA <- "E.*_bAnnual_Pr\\.tif$"
patternS <- "E.*_bPercent_Summer_Pr\\.tif$"
patternJ <- "E.*_bJuly_Temp\\.tif$"

### Define path for all the factors
Annual <- "Rasters/Annual_Pr/Baseline/"
Summer <- "Rasters/Percent_Summer_Pr/Baseline/"
July <- "Rasters/July_Temp/Baseline/"

# List, load, and stack the rasters in one step
AP <- list.files(Annual, pattern = patternA, full.names = TRUE)
AnnualPr <- stack(lapply(AP, raster))

SP <- list.files(Summer, pattern = patternS, full.names = TRUE)
SummerPr <- stack(lapply(SP, raster))

JT <- list.files(July, pattern = patternJ, full.names = TRUE)
JulyTemp <- stack(lapply(JT, raster))

# Get means  
mean_AnnualPr <- calc(AnnualPr, fun = mean, na.rm = TRUE)
mean_Summer_Pr <- calc(SummerPr, fun = mean, na.rm = TRUE)
Mean_July_Temp <- calc(JulyTemp, fun = mean, na.rm = TRUE)

# Get rainforest mean
BASE_MEAN_RAINFOREST <- (mean_AnnualPr >= 140) & (mean_Summer_Pr >= 10) & (Mean_July_Temp <= 16)
BASE_MEAN_RAINFOREST <- as.integer(BASE_MEAN_RAINFOREST)

# Define the output path and filename
output_path <- "Rasters/Rainforest/Baseline/"

# Export base mean raster
BASE_MEAN_FILE <- paste0(output_path, "MEAN_bRainforest.tif")
writeRaster(BASE_MEAN_RAINFOREST, BASE_MEAN_FILE, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)

# Generate pyramid (.ovr) file
gdal_addo(BASE_MEAN_FILE, r = "average", overviews = c(2, 4, 8, 16), method = "average")

# Cell count of mean rainforest extent UK
BMR_filtered <- overlay(BASE_MEAN_RAINFOREST, fun = function(x) {
  x[x != 1] <- NA
  return(x)
})
BMR_UK <- mask(BMR_filtered, UK_area) ## Clip raster to UK
BMR_UKcells <- sum(!is.na(values(BMR_UK)))
print(BMR_UKcells)

## And for Britain only
BMR_B <- mask(BMR_filtered, Britain_area)
BMR_Bcells <- sum(!is.na(values(BMR_B)))
print(BMR_Bcells)


################ CREATE A MAP OF CHANGE IN RAINFOREST AREA.
# Get the three baseline and future rasters with 2nd, 7th and 11th biggest extent, representing 10th and 90th percentiles and median.
print(ordered_BaseUK_rainforests_extent) 
print(ordered_FutureUK_rainforests_extent) # run lists to return order of raster by cell count

# Manually load appropriate rasters
BaseRF_2nd_Lowest <- raster("Rasters/Rainforest/Baseline/E10_bRainforest.tif")
BaseRF_Median <- raster("Rasters/Rainforest/Baseline/E11_bRainforest.tif")
BaseRF_2nd_Highest <- raster("Rasters/Rainforest/Baseline/E1_bRainforest.tif")

# Manually load appropriate rasters
FutureRF_2nd_Lowest <- raster("Rasters/Rainforest/Future/E11_fRainforest.tif")
FutureRF_Median <- raster("Rasters/Rainforest/Future/E1_fRainforest.tif")
FutureRF_2nd_Highest <- raster("Rasters/Rainforest/Future/E7_fRainforest.tif")

# Get change in Future against mean (climate normal). With loss and gain of rainforest area added
RF2L <- overlay(FutureRF_2nd_Lowest, BASE_MEAN_RAINFOREST, fun = function(x, y) {
  result <- ifelse(x == 0 & y == 0, 0,
                   ifelse(x == 1 & y == 1, 1,
                          ifelse(x == 0 & y == 1, 2, 3)))
  return(result)
})
RFMed <- overlay(FutureRF_Median, BASE_MEAN_RAINFOREST, fun = function(x, y) {
  result <- ifelse(x == 0 & y == 0, 0,
                   ifelse(x == 1 & y == 1, 1,
                          ifelse(x == 0 & y == 1, 2, 3)))
  return(result)
})
RF2H <- overlay(FutureRF_2nd_Highest, BASE_MEAN_RAINFOREST, fun = function(x, y) {
  result <- ifelse(x == 0 & y == 0, 0,
                   ifelse(x == 1 & y == 1, 1,
                          ifelse(x == 0 & y == 1, 2, 3)))
  return(result)
})

### Create plot
# List of rasters and their corresponding names
rainforest_maps <- list(BaseRF_2nd_Lowest, BASE_MEAN_RAINFOREST, BaseRF_2nd_Highest, FutureRF_2nd_Lowest, FutureRF_Median, FutureRF_2nd_Highest, RF2L, RFMed, RF2H)
order_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") 

# Define the function to mask and convert rasters to data frames
raster_to_df <- function(r, name) {
  masked_raster <- mask(r, UK_area) #clip to UK
  filtered_raster <- calc(masked_raster, function(x) ifelse(x %in% c(1, 2, 3), x, NA)) # Filter raster values to exclude non rainforest
  df <- as.data.frame(rasterToPoints(filtered_raster), stringsAsFactors = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$plot_title <- name
  return(df)
}

# Convert each raster to a data frame and combine them
combined_df <- do.call(rbind, lapply(seq_along(rainforest_maps), function(i) {
  raster_to_df(rainforest_maps[[i]], order_names[i])
}))

# Titles for the plots
titles <- c("2nd Lowest\nProjected Extent", "Mean Projected\nExtent (Climate Normal)", "2nd Highest\nProjected Extent", "2nd Lowest\nProjected Extent", "Central Projected\nExtent", "2nd Highest\nProjected Extent", "Difference between\n2nd Lowest Future Extent\nand Climate Normal", "Difference between\nCentral Future Extent\nand Climate Normal", "Difference between\n2nd Highest Future Extent\nand Climate Normal")

######## CREATE A LEGEND SEPERATELY ################
# Function to extract the legend from a ggplot
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Create a separate plot just for the legend
legend_plot <- ggplot(combined_df %>% filter(plot_title == "8")) + 
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = factor(value))) + 
  scale_fill_manual(values = c("1" = "#228B22", "2" = "grey", "3" = "red"), 
                    labels = c("1" = "Rainforest Zone  ", 
                               "2" = "Loss of Rainforest Zone  ", 
                               "3" = "New Rainforest Zone")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(title = NULL, keyheight = unit(0.5, "lines"), keywidth = unit(0.3, "lines")))
# Extract the legend
legend <- get_legend(legend_plot)

##### CREATE CHANGE MAPS
# ggplot2
plots <- lapply(1:9, function(i) {
  if (i <= 6) {
    color_scheme <- scale_fill_manual(values = c("1" = "#228B22"))
  } else {
    color_scheme <- scale_fill_manual(values = c("1" = "#228B22", "2" = "grey", "3" = "red"))
  }
  ggplot(combined_df %>% filter(plot_title == as.character(i))) + 
    geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
    geom_raster(aes(x = x, y = y, fill = factor(value))) + 
    color_scheme +
    annotation_custom(
      grob = textGrob(titles[i], x = unit(0.05, "npc"), y = unit(0.91, "npc"), 
                      just = "left", gp = gpar(fontsize = 8))
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),  # Remove grid lines
      legend.position = "none",  # Remove legend from individual plots
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),  # Add border around each plot
      plot.margin = unit(c(0.1, 0.1, 0.3, 0.1), "cm")  # Reduce space between plots
    ) +
    coord_sf()
})

# Create row titles
row_titles <- c("  Baseline Rainforest Zone (1981 - 2000)", "  Future Rainforest Zone (2061 - 2080)", "  Change in Rainforest Zone")
row_title_grobs <- lapply(row_titles, function(title) {
  textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 18))
})

# Combine the plots into a single gtable with fixed widths and heights
plot_gtable <- arrangeGrob(
  arrangeGrob(grobs = plots[1:3], ncol = 3, top = row_title_grobs[[1]], widths = unit(c(0.25, 0.25, 0.25), "npc")),
  arrangeGrob(grobs = plots[4:6], ncol = 3, top = row_title_grobs[[2]], widths = unit(c(0.25, 0.25, 0.25), "npc")),
  arrangeGrob(grobs = plots[7:9], ncol = 3, top = row_title_grobs[[3]], widths = unit(c(0.25, 0.25, 0.25), "npc")),
  ncol = 1,
  heights = unit(rep(1, 3), "null")
)

# Create text at bottom to explain maps
bottom_text <- textGrob("          Rainforest zone calcualted using the ALaback criteria. The 2nd Lowest, Central and 2 highest projections represent the 2nd, 7th and 11th Lowest projections out of the 12 local simulation ensemble members when ordered by extent of modelled rainforest zone.  The top row of maps show the baseline projections, the middle row\n          shows the future projections and the bottom row shows the difference between the future projections and the baseline climate normal. In this plot, the baseline climate normal is represented by the mean of the 12 ensemble members (top middle). The change in rainforest zone maps are then as follows; bottom left =\n          climate normal – 2nd Lowest Future Extent (middle left), bottom middle = climate normal – Central Future Extent (middle middle), bottom right = climate normal – 2nd Highest Future Extent (middle right).", x = unit(0, "npc"),  just = "left", gp = gpar(fontsize = 4))

# Export an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_UK_rainforest.pdf", width = 8.27, height = 11.69)

# Combine the plot gtable and the legend
combined_gtable <- arrangeGrob(
  plot_gtable,
  legend,
  bottom_text,
  ncol = 1,
  heights = unit.c(unit(0.95, "npc") - unit(2, "lines"), unit(2.5, "lines"), unit(0.3, "lines"))  # Adjust the space for the legend
)

# Add title to gtable
grid.arrange(
  combined_gtable,
  top = textGrob("Change in UK Alaback Rainforest Zone\n ", gp = gpar(fontsize = 30, fontface = "bold", lineheight = 0.1))
)

# Close the PDF device
dev.off()
############### END
