##################### Oceanicity ########################

# 3c. Map change in Oceanicty

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

#####  ORDER RASTERS BY EXTENT/ CELL COUNT TO FIND MEDIAN, 10th AND 90th PERCENTILE OF Oceanicity >15 IN THE MODEL #####

### Load basemaps
Britain_area <- st_read("Inputs/BASEMAPS/Britain_area.shp")
UK_area <- st_read("Inputs/BASEMAPS/UK_area.shp")

### Transform to British National Grid (BNG)
Britain_area <- st_transform(Britain_area, crs = 27700)
UK_area <- st_transform(UK_area, crs = 27700)

### BASELINE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Baseline_oceanicity_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 15] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Base_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
BaseUK_oceanicity_extent <- lapply(Base_raster_names, process_raster)

# Extract raster data frames and cell counts
cell_counts <- sapply(BaseUK_oceanicity_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Base_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_BaseUK_oceanicity_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_BaseUK_oceanicity_extent)

### FUTURE
# Process rainforest rasters and return the number of cells in the filtered raster
# Function to process rasters
process_raster <- function(raster_name) {
  raster <- Future_oceanicity_list[[raster_name]]
  raster_clipped <- mask(raster, UK_area) ## Clip raster to UK
  raster_filtered <- calc(raster_clipped, fun = function(x) { x[x < 15] <- NA; return(x) })       ### Filter out non rainforest values
  raster_df <- as.data.frame(rasterToPoints(raster_filtered), stringsAsFactors = FALSE)
  colnames(raster_df) <- c("x", "y", "value")
  raster_df$plot_title <- new_titles[which(Future_raster_names == raster_name)]
  
  # Count the number of non-NA cells in the filtered raster
  non_na_cells <- sum(!is.na(values(raster_filtered))) 
  
  return(list(raster_df = raster_df, cell_count = non_na_cells))
}

# Process all rasters and get cell counts
FutureUK_oceanicity_extent <- lapply(Future_raster_names, process_raster)

# Extract raster data frames and cell counts
cell_counts <- sapply(FutureUK_oceanicity_extent, function(info) info$cell_count)

# Order raster names by the number of non-NA cells in the filtered raster
ordered_raster_names <- Future_raster_names[order(cell_counts)]

# Print the ordered raster names and their cell counts to find median, 10th and 90th percentiles
ordered_FutureUK_oceanicity_extent <- data.frame(
  raster_name = ordered_raster_names,
  cell_count = cell_counts[order(cell_counts)]
)
print(ordered_FutureUK_oceanicity_extent)

########## CREATE CHANGE IN OCEANICITY PLOTS
## Calculate a baseline mean
O_Base_stack <- stack(Baseline_oceanicity_list)

# Calculate the mean
MEAN_Oceanicity <- calc(O_Base_stack, fun = mean, na.rm = TRUE)

# Cell count of mean Oceanicity extent UK
Mean_Ofiltered <- overlay(MEAN_Oceanicity, fun = function(x) {
  x[x < 15] <- NA
  return(x)
})
Mean_Oceanicity_UK <- mask(Mean_Ofiltered, UK_area) ## Clip raster to UK
Mean_Oceanicity_UKcells <- sum(!is.na(values(Mean_Oceanicity_UK)))
print(Mean_Oceanicity_UKcells)

# Mean extent for Britain
Mean_Oceanicity_Brit <- mask(Mean_Ofiltered, Britain_area) ## Clip raster to UK
Mean_Oceanicity_Britcells <- sum(!is.na(values(Mean_Oceanicity_Brit)))
print(Mean_Oceanicity_Britcells)

# Get the three baseline and future rasters with 2nd, 7th and 11th biggest extent, representing 10th and 90th percentiles and median.
print(ordered_BaseUK_oceanicity_extent) 
print(ordered_FutureUK_oceanicity_extent) # run lists to return order of raster by cell count

# Manually load appropriate rasters
BaseO_2nd_Lowest <- raster("Rasters/Oceanicity/Baseline/E10_bOceanicity.tif")
BaseO_Median <- raster("Rasters/Oceanicity/Baseline/E13_bOceanicity.tif")
BaseO_2nd_Highest <- raster("Rasters/Oceanicity/Baseline/E7_bOceanicity.tif")

# Manually load appropriate rasters
FutureO_2nd_Lowest <- raster("Rasters/Oceanicity/Future/E9_fOceanicity.tif")
FutureO_Median <- raster("Rasters/Oceanicity/Future/E5_fOceanicity.tif")
FutureO_2nd_Highest <- raster("Rasters/Oceanicity/Future/E12_fOceanicity.tif")

# Mean oceanicity clipped to 10
Median_O_10 <- overlay(BaseO_Median, fun = function(x) {
  x[x < 10] <- NA
  return(x)
})

# Get change in Future against mean 
# Mean that is clipped to wider rainforest area (Oceanicity >15)
O2L <-  Median_O_10 - FutureO_2nd_Lowest
OMed <- Median_O_10 - FutureO_Median 
O2H <- Median_O_10 - FutureO_2nd_Highest 

# Create change in Oceanicity colour palette
Blue_Green_Red <- c("blue", "green", "yellow", "orange", "red")
Green_Red <- c("green", "yellow", "orange", "red")

# Define the breaks and labels for change in Oceancity colour palette
RF_Change_Oceanicity_colours <- scale_fill_manual(
  values = Blue_Green_Red,
  labels = c(">+1  ", "+1 to -1  ", "-1 to -2  ", "-2 to -3  ", "<-3  ")
)

# Adjust the color scheme for the 7th raster (VPD2L) as no values <-1
RF_Change_Oceanicity_colours_O2L <- scale_fill_manual(
  values = Green_Red,
  labels = c("-1 - 1  ", "1 - 2  ", "2 - 3  ", ">3  "),
  na.value = "transparent"
)

### Create plot
# List of rasters and their corresponding names
O_maps <- list(BaseO_2nd_Lowest, BaseO_Median, BaseO_2nd_Highest, FutureO_2nd_Lowest, FutureO_Median, FutureO_2nd_Highest, O2L, OMed, O2H)
order_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") 

# Function to process rasters
O_raster_to_df <- function(r, name) {
  masked_raster <- mask(r, UK_area) # Clip to UK
  df <- as.data.frame(rasterToPoints(masked_raster), stringsAsFactors = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$plot_title <- name
  
  # Add colour band column
  df$colour_band <- cut(
    df$value,
    breaks = c(5, 10, 15, 20, 25, Inf),
    labels = c("5-10", "10-15", "15-20", "20-25", "25+")
  )
  
  # Add colour band2 column
  df$colour_band2 <- cut(
    df$value,
    breaks = c(-Inf, -1, 1, 2, 3, Inf),
    labels = c("<-1", "-1 to 1", "1 to 2", "2 to 3", ">3")
  )
  
  return(df)
}

# Convert each raster to a data frame and combine them
O_combined_df <- do.call(rbind, lapply(seq_along(O_maps), function(i) {
  O_raster_to_df(O_maps[[i]], order_names[i])
}))

# Titles for the plots
titles <- c("2nd Lowest\nProjected Extent", "Central Projected\nExtent (Climate Normal)", "2nd Highest\nProjected Extent", "2nd Lowest\nProjected Extent", "Central Projected\nExtent", "2nd Highest\nProjected Extent", "Difference between 2nd\nLowest Future Projection\nand Climate Normal", "Difference between\nCentral Future Projection\nand Climate Normal", "Difference between 2nd\nHighest Future Projection\nand Climate Normal")

######## CREATE A LEGEND SEPERATELY ################
# Function to extract the legend from a ggplot
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Filter out NA values from the data
NA_less_df <- O_combined_df %>% filter(plot_title == "2" & !is.na(colour_band))

# Create a separate plot just for the top legend
top_legend_plot <- ggplot(NA_less_df) +  
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band)) + 
  scale_fill_custom +  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10)
  ) +
  guides(fill = guide_legend(title = expression(atop("Oceanicity Index", 
                                                     atop(italic("(Annual Precipitation (mm)/"), 
                                                          italic("Temperature Range(°C))")))),
                             ))

# Extract the top legend
Top_Legend <- get_legend(top_legend_plot)

# Separate plot for bottom legend
bottom_legend_plot <- ggplot(O_combined_df %>% filter(plot_title == "8")) + 
  geom_sf(data = GBR_Country, fill = NA, color = "grey", size = 0.5) + 
  geom_raster(aes(x = x, y = y, fill = colour_band2)) + 
  RF_Change_Oceanicity_colours +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8) 
  ) +
  guides(fill = guide_legend(
    title = "Change in Oceanicity\nIndex From\nClimate Normal", 
  ))

# Extract the bottom legend
Bottom_Legend <- get_legend(bottom_legend_plot)

######## MAKE PLOT
# ggplot2
plots <- lapply(1:9, function(i) {
  if (i <= 6) {
    colour_scheme <- scale_fill_custom
    fill_aes <- aes(x = x, y = y, fill = colour_band)
  } else {
    if (i == 7) {
      colour_scheme <- RF_Change_Oceanicity_colours_O2L
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    } else {
      colour_scheme <- RF_Change_Oceanicity_colours
      fill_aes <- aes(x = x, y = y, fill = colour_band2)
    }
  }
  ggplot(O_combined_df %>% filter(plot_title == as.character(i))) + 
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
row_titles <- c("  Baseline Oceanicity Index (1981 - 2000)", "  Future Oceanicity Index (2061 - 2080)", "  Change in Oceanicity Index")
row_title_grobs <- lapply(row_titles, function(title) {
  textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 18))
})

# Create a blank textGrob to add space
spacer <- textGrob(" \n ", gp = gpar(fontsize = 50))

# Combine the plots into a single gtable
plot_gtable <- arrangeGrob(
  arrangeGrob(grobs = list(plots[[1]], plots[[2]], plots[[3]],  spacer), ncol = 4, top = row_title_grobs[[1]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[4]], plots[[5]], plots[[6]], arrangeGrob( Top_Legend, spacer, ncol = 1)),  ncol = 4, top = row_title_grobs[[2]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[7]], plots[[8]], plots[[9]], Bottom_Legend), ncol = 4, top = row_title_grobs[[3]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  ncol = 1,
  heights = unit(rep(1, 3), "null")
)

# Create text at bottom to explain maps
bottom_text <- textGrob("          Rainforest zone defined as ocenicity Index >15. The 2nd Lowest, Central and 2 highest projections represent the 2nd, 7th and 11th Lowest projections out of the 12 local simulation ensemble members when ordered by extent of modelled rainforest zone. In this plot, the baseline climate normal is represented by the Central\n          Projection of the 12 ensemble members (top middle). The top row shows baseline projections, middle shows future and the bottom row shows the difference between the futre projections and the baseline cliamte normal. The change in rainforest zone maps are then as follows; bottom left = climate normal – 2nd Lowest\n           Future Extent (middle left), bottom middle = climate normal – Central Future Extent (middle middle), bottom right = climate normal – 2nd Highest Future Extent(middle right). The change in Oceanicity maps only look at areas with an Oceanicity Climate Normal >10", x = unit(0, "npc"),  just = "left", gp = gpar(fontsize = 4))

#EXPORT
# Open an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_Oceanicity.pdf", width = 8.27, height = 11.69)

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
  top = textGrob("Change in Oceanicity Index\n ", gp = gpar(fontsize = 30, fontface = "bold", lineheight = 0.1))
)

# Close the PDF device
dev.off()
############### END
