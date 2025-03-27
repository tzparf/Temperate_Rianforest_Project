########################## Vapour Pressure Deficit ###############################

# 4b. Change in VPD

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

#####  ORDER RASTERS BY EXTENT/ CELL COUNT TO FIND MEDIAN, 10th AND 90th PERCENTILE OF VPD <=0.3 #####

### Load basemaps
UK_area <- st_read("Inputs/BASEMAPS/UK_area.shp")

### Transform to British National Grid (BNG)
UK_area <- st_transform(UK_area, crs = 27700)

################## GET EXTENT OF RASTERS WITH VPD <= 0.3
# BASELINE
# creat a list for VPD rasters
Baseline_VPD_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  BaseVPD_path <- file.path(Base_VPD_path, paste0("E", i, "_bVPD.tif"))
  Baseline_VPD_list[[paste0("E", i, "bVPD")]] <- raster(BaseVPD_path)
}

## Load the rasters
# List of raster names 
Base_raster_names <- c("E1bVPD", "E4bVPD", "E5bVPD", "E6bVPD", "E7bVPD", "E8bVPD", "E9bVPD", "E10bVPD", "E11bVPD", "E12bVPD", "E13bVPD", "E15bVPD")

# New titles for the rasters for use in list
new_titles <- c("01", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "15")

# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
clip_raster_extent <- function(raster_name) {
  raster <- Baseline_VPD_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x > 0.3] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
BaseUK_VPD_extent <- lapply(Base_raster_names, clip_raster_extent)

# Extract raster data frames and cell counts
cell_counts <- sapply(BaseUK_VPD_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Base_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_BaseUK_VPD_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_BaseUK_VPD_extent)

###### FUTURE
# creat a list for VPD rasters
Future_VPD_list <- list()

# Loop through indices to load the rasters
for (i in indices) {
  FutureVPD_path <- file.path(Future_VPD_path, paste0("E", i, "_fVPD.tif"))
  Future_VPD_list[[paste0("E", i, "fVPD")]] <- raster(FutureVPD_path)
}

## Load the rasters
# List of raster names 
Future_raster_names <- c("E1fVPD", "E4fVPD", "E5fVPD", "E6fVPD", "E7fVPD", "E8fVPD", "E9fVPD", "E10fVPD", "E11fVPD", "E12fVPD", "E13fVPD", "E15fVPD")

# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
clip_raster_extent <- function(raster_name) {
  raster <- Future_VPD_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x > 0.3] <- NA; return(x) }) ### Filter out non-rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
FutureUK_VPD_extent <- lapply(Future_raster_names, clip_raster_extent)

# Extract raster data frames and cell counts
cell_counts <- sapply(FutureUK_VPD_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Future_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_FutureUK_VPD_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_FutureUK_VPD_extent)


########## CREATE CHANGE IN VPD PLOTS
## Calculate a baseline mean
VPD_Base_stack <- stack(Baseline_VPD_list)

# Calculate the mean
MEAN_VPD <- calc(VPD_Base_stack, fun = mean, na.rm = TRUE)

# Cell count of mean VPD extent UK
MVPD_filtered <- overlay(MEAN_VPD, fun = function(x) {
  x[x > 0.3] <- NA
  return(x)
})
MVPD_UK <- mask(MVPD_filtered, UK_area) ## Clip raster to UK
MVPD_UKcells <- sum(!is.na(values(MVPD_UK)))
print(MVPD_UKcells)

# Get the three baseline and future rasters with 2nd, 7th and 11th biggest extent, representing 10th and 90th percentiles and median.
print(ordered_BaseUK_VPD_extent) 
print(ordered_FutureUK_VPD_extent) # run lists to return order of raster by cell count

# Manually load appropriate rasters
BaseVPD_2nd_Lowest <- raster("Rasters/VPD/Baseline/E8_bVPD.tif")
BaseVPD_Median <- raster("Rasters/VPD/Baseline/E7_bVPD.tif")
BaseVPD_2nd_Highest <- raster("Rasters/VPD/Baseline/E1_bVPD.tif")

# Manually load appropriate rasters
FutureVPD_2nd_Lowest <- raster("Rasters/VPD/Future/E10_fVPD.tif")
FutureVPD_Median <- raster("Rasters/VPD/Future/E9_fVPD.tif")
FutureVPD_2nd_Highest <- raster("Rasters/VPD/Future/E5_fVPD.tif")

############### GET A MEAN OUTLINE FOR <0.4
MVPD_4 <- overlay(BaseVPD_Median, fun = function(x) {
  x[x > 0.4] <- NA
  return(x)
})
MVPD_4 <- mask(MVPD_4, UK_area)

# Get change in Future against mean 
# Mean that is clipped to wider rainforest area (VPD<0.4)
VPD2L <-  (FutureVPD_2nd_Lowest - MVPD_4) *100 / MVPD_4
VPDMed <- (FutureVPD_Median - MVPD_4) *100 / MVPD_4
VPD2H <- (FutureVPD_2nd_Highest - MVPD_4) *100 / MVPD_4

### CrVPD2L### Create plot
# List of rasters and their corresponding names
VPD_maps <- list(BaseVPD_2nd_Lowest, BaseVPD_Median, BaseVPD_2nd_Highest, FutureVPD_2nd_Lowest, FutureVPD_Median, FutureVPD_2nd_Highest, VPD2L, VPDMed, VPD2H)
order_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") 

# Function to process rasters
VPD_raster_to_df <- function(r, name) {
  masked_raster <- mask(r, UK_area) # Clip to UK
  df <- as.data.frame(rasterToPoints(masked_raster), stringsAsFactors = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$plot_title <- name
  
  # Add colour band column
  df$colour_band <- cut(
    df$value,
    breaks = c(-Inf, 0.2, 0.3, 0.4, 0.5, 0.6, Inf),
    labels = c("<0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6", ">0.6")
  )
  
  # Add colour band2 column
  df$colour_band2 <- cut(
    df$value,
    breaks = c(-Inf, 10, 25, 50, 100, Inf),
    labels = c("<10", "10 to 25", "25 to 50", "50 to 100", ">100")
  )
  
  return(df)
}

# Convert each raster to a data frame and combine them
VPD_combined_df <- do.call(rbind, lapply(seq_along(VPD_maps), function(i) {
  VPD_raster_to_df(VPD_maps[[i]], order_names[i])
}))

# Titles for the plots
titles <- c("2nd Lowest\nProjected Extent", "Central Projected\nExtent (Climate Normal)", "2nd Highest\nProjected Extent", "2nd Lowest\nProjected Extent", "Central Projected\nExtent", "2nd Highest\nProjected Extent", "Difference between 2nd\nLowest Future Projection\nand Climate Normal", "Difference between\nCentral Future Projection\nand Climate Normal", "Difference between 2nd\nHighest Future Projection\nand Climate Normal")

###### CREATE COLOUR PALETTES
# Create VPD colour palette
green_blue_purple <- c("#883689", "#765fb0", "#7998cc", "#8fd4cb", "#bee3b6", "#FFDAB9")

# Define the breaks and labels for VPD colour palette
scale_fill_custom3 <- scale_fill_manual(
  values = green_blue_purple,
  labels = c("<0.2 ", "0.2-0.3 ", "0.3-0.4 ", "0.4-0.5 ", "0.5-0.6 ", ">0.6 "),
  na.value = "transparent"
)

# Create change in VPD colour palette
Blue_Green_Red <- c("blue", "green", "yellow", "orange", "red")
Blue_Orange <- c("blue", "green", "yellow", "orange")

# Define the breaks and labels for VPD colour palette
RF_Change_VPD_colours <- scale_fill_manual(
  values = Blue_Green_Red,
  labels = c("<10% ", "10-25% ", "25-50% ", "50-100% ", ">100% "),
  na.value = "transparent"
)

# Adjust the color scheme for the 7th raster (VPD2L) as no values <0
RF_Change_VPD_colours_VPD2H <- scale_fill_manual(
  values = Blue_Orange,
  labels = c("<10% ", "10-25% ", "25-50% ", "50-100% "),
  na.value = "transparent"
)

######## CREATE A LEGEND SEPERATELY ################
# Function to extract the legend from a ggplot
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Create a separate plot just for the top legend
top_legend_plot <- ggplot(VPD_combined_df %>% filter(plot_title == "5")) + 
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band)) + 
  scale_fill_custom3 + 
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
    title = "Mean July VPD (kPa)", 
  ))

# Extract the top legend
Top_Legend <- get_legend(top_legend_plot)

# Separate plot for bottom legend
bottom_legend_plot <- ggplot(VPD_combined_df %>% filter(plot_title == "8")) + 
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band2)) + 
  RF_Change_VPD_colours +
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
    title = "Change in July VPD\nFrom Climate Normal", 
  ))

# Extract the bottom legend
Bottom_Legend <- get_legend(bottom_legend_plot)

######## MAKE PLOT
# ggplot2
plots <- lapply(1:9, function(i) {
  if (i <= 6) {
    colour_scheme <- scale_fill_custom3
    fill_aes <- aes(x = x, y = y, fill = colour_band)
  } else {
    if (i == 9) {
      colour_scheme <- RF_Change_VPD_colours_VPD2H
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    } else {
      colour_scheme <- RF_Change_VPD_colours
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    }
  }
  ggplot(VPD_combined_df %>% filter(plot_title == as.character(i))) + 
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
row_titles <- c("  Baseline VPD (1981 - 2000)", "  Future VPD (2061 - 2080)", "  Change in VPD")
row_title_grobs <- lapply(row_titles, function(title) {
  textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 18))
})

# Create a blank textGrob to add space
spacer <- textGrob(" \n \n \n ", gp = gpar(fontsize = 50))

# Combine the plots into a single gtable
plot_gtable <- arrangeGrob(
  arrangeGrob(grobs = list(plots[[1]], plots[[2]], plots[[3]],  spacer), ncol = 4, top = row_title_grobs[[1]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[4]], plots[[5]], plots[[6]], arrangeGrob( Top_Legend, spacer, ncol = 1)),  ncol = 4, top = row_title_grobs[[2]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[7]], plots[[8]], plots[[9]], Bottom_Legend), ncol = 4, top = row_title_grobs[[3]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  ncol = 1,
  heights = unit(rep(1, 3), "null")
)

# Create text at bottom to explain maps
bottom_text <- textGrob("          Rainforest zone defined as VPD <3. The 2nd Lowest, Central and 2 highest projections represent the 2nd, 7th and 11th Lowest projections out of the 12 local simulation ensemble members when ordered by extent of modelled rainforest zone. In this plot, the baseline climate normal is represented by the Central\n          Projection of the 12 ensemble members (top middle). The change in rainforest zone maps are then as follows; bottom left = climate normal – 2nd Lowest Future Extent (middle left), bottom middle = climate normal – Central Future Extent (middle middle), bottom right = climate normal – 2nd Highest Future Extent\n          (middle right). The change in VPD maps only look at the areas with VPD <4 in the Climate Normal", x = unit(0, "npc"),  just = "left", gp = gpar(fontsize = 4))

#EXPORT
# Open an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_VPD.pdf", width = 8.27, height = 11.69)

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
  top = textGrob("Change in Vapour Pressure Deficit\n ", gp = gpar(fontsize = 30, fontface = "bold", lineheight = 0.1))
)

# Close the PDF device
dev.off()
############### END
