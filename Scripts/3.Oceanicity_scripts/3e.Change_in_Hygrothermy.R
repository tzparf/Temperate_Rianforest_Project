##################### Hygrothermy ########################

# 3e. Change in Hygrothermy 

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

#####  ORDER RASTERS BY EXTENT/ CELL COUNT TO FIND MEDIAN, 10th AND 90th PERCENTILE OF Hygrothermy >100 IN THE MODEL #####

### BASELINE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
Cell_Count_Function <- function(raster_name) {
  raster <- Baseline_Hygrothermy_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 100] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_Hygro_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
BaseUK_H_extent <- lapply(Base_Hygro_names, Cell_Count_Function)

# Extract raster data frames and cell counts
cell_counts <- sapply(BaseUK_H_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Base_Hygro_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_BaseUK_H_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_BaseUK_H_extent)

### FUTURE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
Cell_Count_Function <- function(raster_name) {
  raster <- Future_Hygrothermy_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 100] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_Hygro_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
FutureUK_H_extent <- lapply(Future_Hygro_names, Cell_Count_Function)

# Extract raster data frames and cell counts
cell_counts <- sapply(FutureUK_H_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Future_Hygro_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_FutureUK_H_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_FutureUK_H_extent)

## MAKE MEAN BASELINE
########## CREATE CHANGE IN Hygrothermy PLOTS
## Calculate a baseline mean
H_Base_stack <- stack(Baseline_Hygrothermy_list)

# Calculate the mean
MEAN_Hygrothermy <- calc(H_Base_stack, fun = mean, na.rm = TRUE)

# Cell count of mean Hygrothermy extent UK
Mean_Hfiltered <- overlay(MEAN_Hygrothermy, fun = function(x) {
  x[x < 100] <- NA
  return(x)
})
Mean_Hygrothermy_UK <- mask(Mean_Hfiltered, UK_area) ## Clip raster to UK
Mean_Hygrothermy_UKcells <- sum(!is.na(values(Mean_Hygrothermy_UK)))
print(Mean_Hygrothermy_UKcells)

# Mean extent for Britain
E8_Hygrothermy_Brit <- mask(Mean_Hfiltered, Britain_area) ## Clip raster to UK
E8_Hygrothermy_Britcells <- sum(!is.na(values(E8_Hygrothermy_Brit)))
print(E8_Hygrothermy_Britcells)

# Export base mean hygrothermy
# Define the output path and filename
base_path <- "Rasters/Hygrothermy/Baseline/"

# Export base mean raster
BASE_MEAN_FILE <- paste0(base_path, "MEAN_bHygro.tif")
writeRaster(MEAN_Hygrothermy, BASE_MEAN_FILE, format = "GTiff", options = c("COMPRESS=LZW", "TFW=YES", "BIGTIFF=YES"), overwrite = TRUE)

# Generate pyramid (.ovr) file
gdal_addo(BASE_MEAN_FILE, r = "average", overviews = c(2, 4, 8, 16), method = "average")

######### MAKE THE MAPS
# Get the three baseline and future rasters with 2nd, 7th and 11th biggest extent, representing 10th and 90th percentiles and median.
print(ordered_BaseUK_H_extent) 
print(ordered_FutureUK_H_extent) # run lists to return order of raster by cell count

# Manually load appropriate rasters
BaseH_2nd_Lowest <- raster("Rasters/Hygrothermy/Baseline/E9_bHygro.tif")
BaseH_Median <- raster("Rasters/Hygrothermy/Baseline/E7_bHygro.tif")
BaseH_2nd_Highest <- raster("Rasters/Hygrothermy/Baseline/E8_bHygro.tif")

# Manually load appropriate rasters
FutureH_2nd_Lowest <- raster("Rasters/Hygrothermy/Future/E12_fHygro.tif")
FutureH_Median <- raster("Rasters/Hygrothermy/Future/E1_fHygro.tif")
FutureH_2nd_Highest <- raster("Rasters/Hygrothermy/Future/E4_fHygro.tif")

# Get change in Future against mean 
# Mean that is clipped to wider rainforest area (Hygrothermy >100)
H2L <- FutureH_2nd_Lowest - BaseH_2nd_Highest 
HMed <- FutureH_Median - BaseH_2nd_Highest 
H2H <- FutureH_2nd_Highest - BaseH_2nd_Highest

# Create change in Oceanicity colour palette
PBGR <- c("red", "yellow", "green", "blue", "purple")
PBG <- c("yellow", "green", "blue", "purple")

# Define the breaks and labels for change in Oceancity colour palette
RF_Change_Hygrothermy_colours <- scale_fill_manual(
  values = PBGR,
  labels = c("<-5  ", "-5 to 5  ", "5 to 15  ", "15 to 25  ", ">25 ")
)

# Define the breaks and labels for change in Oceancity colour palette
RF_Change_Hygrothermy_colours2 <- scale_fill_manual(
  values = PBG,
  labels = c("-5 - 5 ", "5 - 15 ", "15 - 25 ", ">25 ")
)

### Create maps
# List of rasters and their corresponding names
H_maps <- list(BaseH_2nd_Lowest, BaseH_Median, BaseH_2nd_Highest, FutureH_2nd_Lowest, FutureH_Median, FutureH_2nd_Highest, H2L, HMed, H2H)
order_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") 

# Function to process rasters
H_raster_to_df <- function(r, name) {
  masked_raster <- mask(r, UK_area) # Clip to UK
  df <- as.data.frame(rasterToPoints(masked_raster), stringsAsFactors = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$plot_title <- name
  
  # Add colour band column
  df$colour_band <- cut(
    df$value,
    breaks = c(100, 125, 150, 175, 200, Inf),
    labels = c("100-125", "125-150", "150-175", "175-200", "200+")
  )
  
  # Add colour band2 column
  df$colour_band2 <- cut(
    df$value,
    breaks = c(Inf, 25, 15, 5, -5, -Inf),
    labels = c(">25", "25 to 15", "15 to 5", "5 to -5", "<-5")
  )
  
  return(df)
}

# Convert each raster to a data frame and combine them
H_combined_df <- do.call(rbind, lapply(seq_along(H_maps), function(i) {
  H_raster_to_df(H_maps[[i]], order_names[i])
}))

# Set the titles
titles <- c("2nd Lowest\nProjected Extent", "Central Projected\nExtent", "2nd Highest\nProjected Extent\n(Climate Normal)", "2nd Lowest\nProjected Extent", "Central Projected\nExtent", "2nd Highest\nProjected Extent", "Difference between 2nd\nLowest Future Projection\nand Climate Normal", "Difference between\nCentral Future Projection\nand Climate Normal", "Difference between 2nd\nHighest Future Projection\nand Climate Normal")

######## CREATE A LEGEND SEPERATELY ################
# Function to extract the legend from a ggplot
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Filter out NA values from the data
NA_Hless_df <- H_combined_df %>% filter(plot_title == "5" & !is.na(colour_band))

# Create a separate plot just for the top legend
top_legend_plot <- ggplot(NA_Hless_df) +  
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band)) + 
  scale_fill_custom2 +  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(
    title = "Index of\nHygrothermy",
  ))

# Extract the top legend
Top_Legend <- get_legend(top_legend_plot)

# Separate plot for bottom legend
bottom_legend_plot <- ggplot(H_combined_df %>% filter(plot_title == "8")) + 
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band2)) + 
  RF_Change_Hygrothermy_colours +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(
    title = "Change in Index of\nHygrothermy From\nClimate Normal", 
  ))
# Extract the bottom legend
Bottom_Legend <- get_legend(bottom_legend_plot)

######## MAKE PLOT
# ggplot2
plots <- lapply(1:9, function(i) {
  if (i <= 6) {
    colour_scheme <- scale_fill_custom2
    fill_aes <- aes(x = x, y = y, fill = colour_band)
  } else {
    if (i == 9) {
      colour_scheme <- RF_Change_Hygrothermy_colours2
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    } else {
      colour_scheme <- RF_Change_Hygrothermy_colours
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    }
  }
  ggplot(H_combined_df %>% filter(plot_title == as.character(i))) + 
    geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
    geom_raster(fill_aes) + 
    colour_scheme +
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
      panel.border = element_rect(color = "black", fill = NA, size = 0.4),  
      plot.margin = unit(c(0.1, 0.1, 0.4, 0.1), "cm") 
    ) +
    coord_sf()
})

# Create row titles
row_titles <- c("  Baseline Hygrothermy Index (1981 - 2000)", "  Future Hygrothermy Index (2061 - 2080)", "  Change in Hygrothermy Index")
row_title_grobs <- lapply(row_titles, function(title) {
  textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 18))
})

# Create a blank textGrob to add space
spacer <- textGrob(" \n \n ", gp = gpar(fontsize = 50))

# Combine the plots into a single gtable
plot_gtable <- arrangeGrob(
  arrangeGrob(grobs = list(plots[[1]], plots[[2]], plots[[3]],  spacer), ncol = 4, top = row_title_grobs[[1]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[4]], plots[[5]], plots[[6]], arrangeGrob( Top_Legend, spacer, ncol = 1)),  ncol = 4, top = row_title_grobs[[2]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[7]], plots[[8]], plots[[9]], Bottom_Legend), ncol = 4, top = row_title_grobs[[3]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  ncol = 1,
  heights = unit(rep(1, 3), "null")
)

# Create text at bottom to explain maps
bottom_text <- textGrob("          Rainforest zone defined as Hygrothermy Index >100. The 2nd Lowest, Central and 2 highest projections represent the 2nd, 7th and 11th Lowest projections out of the 12 local simulation ensemble members when ordered by extent of modelled rainforest zone. In this plot, the baseline climate normal is represented by the 2nd\n           Highest Projection of the 12 ensemble members (top right). The change in rainforest zone maps are then as follows; bottom left = climate normal – 2nd Lowest Future Extent (middle left), bottom middle = climate normal – Central Future Extent (middle middle), bottom right = climate normal – 2nd Highest Future Extent\n          (middle right).", x = unit(0, "npc"),  just = "left", gp = gpar(fontsize = 4))

#EXPORT
# Open an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_Hygrothermy.pdf", width = 8.27, height = 11.69)

# Combine the legends and the plot gtable
combined_gtable <- arrangeGrob(
  plot_gtable,
  bottom_text,
  ncol = 1,
  heights = unit.c(unit(1, "npc") - unit(3, "lines"), unit(0.3, "lines"))
)

# Add title to gtable
grid.arrange(
  combined_gtable,
  top = textGrob("Change in Hygrothermy\n ", gp = gpar(fontsize = 30, fontface = "bold", lineheight = 0.1))
)

# Close the PDF device
dev.off()
############### END
