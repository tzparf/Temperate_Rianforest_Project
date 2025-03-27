#################### Rainforest scripts (Alaback method) ########################

# 2e. Rainforest Loss. Part 2

##### This script outputs the mean annual and summer precipitation and July Temperature change into one plot.  

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
library(ncdf4)


####### Calculate Future Means for Rainfall, Summer rainfall and July temp ######## ###################################################################################
# Define names 
patternA <- "E.*_fAnnual_Pr\\.tif$"
patternS <- "E.*_fPercent_Summer_Pr\\.tif$"
patternJ <- "E.*_fJuly_Temp\\.tif$"

### Define path for all the factors
Annual <- "Rasters/Annual_Pr/Future/"
Summer <- "Rasters/Percent_Summer_Pr/Future/"
July <- "Rasters/July_Temp/Future/"

# List, load, and stack the rasters in one step
AP <- list.files(Annual, pattern = patternA, full.names = TRUE)
AnnualPr <- stack(lapply(AP, raster))

SP <- list.files(Summer, pattern = patternS, full.names = TRUE)
SummerPr <- stack(lapply(SP, raster))

JT <- list.files(July, pattern = patternJ, full.names = TRUE)
JulyTemp <- stack(lapply(JT, raster))

# Get means  
Future_mean_AnnualPr <- calc(AnnualPr, fun = mean, na.rm = TRUE)
Future_mean_Summer_Pr <- calc(SummerPr, fun = mean, na.rm = TRUE)
Future_Mean_July_Temp <- calc(JulyTemp, fun = mean, na.rm = TRUE)

# Get precipitation into mm
Future_mean_AnnualPr <- Future_mean_AnnualPr * 10
mean_AnnualPr <- mean_AnnualPr * 10

#Get difference in means as %
APr <- (mean_AnnualPr - Future_mean_AnnualPr) * 100 / mean_AnnualPr 
SPr <- (mean_Summer_Pr - Future_mean_Summer_Pr) * 100 / mean_Summer_Pr # SWapped this so that it % change heads in same direction as july temp.
JTas <- (Future_Mean_July_Temp - Mean_July_Temp) * 100 / Mean_July_Temp

################### PLOT ALABACK CRITRIA ON SPRAT PLOTS ##############
# List of rasters and their corresponding names
rainforest_maps <- list(mean_AnnualPr, mean_Summer_Pr, Mean_July_Temp, Future_mean_AnnualPr, Future_mean_Summer_Pr, Future_Mean_July_Temp, APr, SPr, JTas)
order_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") 

### Get rasters into a data frame
# Function to process rasters
Triple_variable_change_df <- function(r, name) {
  masked_raster <- mask(r, UK_area) # Clip to UK
  df <- as.data.frame(rasterToPoints(masked_raster), stringsAsFactors = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$plot_title <- name
  
  # Add colour band column
  df$colour_band <- cut(
    df$value,
    breaks = c(-Inf, 1200, 1400, 1700, 2000, Inf),
    labels = c("<1200", "1200-1400", "1400-1700", "1700-2000", ">2000")
  )
  
  # Add colour band2 column
  df$colour_band2 <- cut(
    df$value,
    breaks = c(0, 10, 15, 20, Inf),
    labels = c("<10", "10 to 15", "15 to 20", ">20")
  )
  
  # Add colour band3 column
  df$colour_band3 <- cut(
    df$value,
    breaks = c(0, 14, 16, 18, 20, Inf),
    labels = c("<14", "14 to 16", "16 to 18", "18 to 20", ">20")
  )
  
  # Add colour band4 column
  df$colour_band4 <- cut(
    df$value,
    breaks = c(-Inf, -20, -10, 0, 10, 20, 30, 40, Inf),
    labels = c("<-20", "-20--10","-10-0", "0-10", "10-20", "20-30", "30-40", ">40")
  )
  
  return(df)
}

# Convert each raster to a data frame and combine them
Triple_df <- do.call(rbind, lapply(seq_along(rainforest_maps), function(i) {
  Triple_variable_change_df(rainforest_maps[[i]], order_names[i])
}))

####### Create colour palettes
# Create change in Oceanicity colour palette
PBGR <- c("purple", "blue", "lightblue", "green", "yellow", "orange", "red", "darkred")
PBGR2 <- c("green", "yellow", "orange", "red", "darkred")
PBGR3 <- c("orange", "red", "darkred")
Blue_palette <- c("#E0F7FA", "#B2EBF2", "#80DEEA", "#4DD0E1", "#26C6DA")
Purple_palette <- c("#E1BEE7", "#CE93D8", "#AB47BC", "#9C27B0")
Purple_palette2 <- c("#CE93D8", "#AB47BC", "#9C27B0")
Orange_palette <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A")

# Define the breaks and labels for change in Annual rain colour palette
AP_colours <- scale_fill_manual(
  values = Blue_palette,
  labels = c("<1200", "1200-1400", "1400-1700", "1700-2000", ">2000")
)

# Define the breaks and labels for change in Summer rain colour palette
SP_colours <- scale_fill_manual(
  values = Purple_palette,
  labels = c("<10", "10 to 15", "15 to 20", ">20"),
  na.value = "transparent"
)

# Define the breaks and labels for change in Summer rain colour palette
SP_colours2 <- scale_fill_manual(
  values = Purple_palette2,
  labels = c("10 to 15", "15 to 20", ">20"),
  na.value = "transparent"
)

# Define the breaks and labels for change in July colour palette
JT_colours <- scale_fill_manual(
  values = Orange_palette,
  labels = c("<14", "14 to 16", "16 to 18", "18 to 20", ">20"),
  na.value = "transparent"
)

# Define the breaks and labels for change in Change colour palette
Percent_change_colours <- scale_fill_manual(
  values = PBGR,
  labels = c(">20% (+ve)", "20% - 10%", "10% - 0%", "0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%", ">40% (-ve)"),
  na.value = "transparent"
)

# Define the breaks and labels for change in Change colour palette
Percent_change_colours2 <- scale_fill_manual(
  values = PBGR2,
  labels = c("0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%", ">40%"),
  na.value = "transparent"
)

# Define the breaks and labels for change in Change colour palette
Percent_change_colours3 <- scale_fill_manual(
  values = PBGR3,
  labels = c("20% - 30%", "30% - 40%", ">40%"),
  na.value = "transparent"
)

# Titles for the plots
titles <- c("Mean Annual \nPrecipitation", "Mean Summer \nPrecipitation", "Mean July \nTemperature", "Mean Annual \nPrecipitation", "Mean Summer \nPrecipitation", "Mean July \nTemperature", "Difference in\nAnnual Precipitation", "Difference in\nSummer Precipitation", "Difference in\nJuly Temperature")


### CREATE LEGENDS
# Create legends for each color palette
legend_AP <- ggplot(data.frame(x = 1, y = 1, fill = factor(c("<1200", "1200-1400", "1400-1700", "1700-2000", ">2000"))), aes(x, y, fill = fill)) +
  geom_tile() +
  scale_fill_manual(values = Blue_palette, labels = c("<1200", "1200-1400", "1400-1700", "1700-2000", ">2000")) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title = "Annual Precipitation (mm)"
  ))

legend_SP <- ggplot(data.frame(x = 1, y = 1, fill = factor(c("<10", "10 to 15", "15 to 20", ">20"))), aes(x, y, fill = fill)) +
  geom_tile() +
  scale_fill_manual(values = Purple_palette, labels = c("<10", "10 to 15", "15 to 20", ">20")) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title = "Summer Precipitation       \n(% of annual)"
  ))

legend_JT <- ggplot(data.frame(x = 1, y = 1, fill = factor(c("<14", "14 to 16", "16 to 18", "18 to 20", ">20"))), aes(x, y, fill = fill)) +
  geom_tile() +
  scale_fill_manual(values = Orange_palette, labels = c("<14", "14 to 16", "16 to 18", "18 to 20", ">20")) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title = "Mean July Temperature   \n(°C)"
  ))

##### Bottom legend
bottom_legend_plot <- ggplot(data.frame(x = 1, y = 1, fill = factor(c("<-20", "-20--10","-10-0", "0-10", "10-20", "20-30", "30-40", ">40"))), aes(x, y, fill = fill)) +
  geom_tile() +
  scale_fill_manual(values = PBGR, labels = c(">20% (+ve)", "20% - 10% ", "10% - 0% ", "0% - 10% ", "10% - 20% ", "20% - 30% ", "30% - 40% ", ">40% (-ve)")) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    title = "% Change in                    \nclimatic variable", 
  ))

# Extract the legends
Bottom_Legend <- ggplotGrob(bottom_legend_plot)$grobs[[which(sapply(ggplotGrob(legend_AP)$grobs, function(x) x$name) == "guide-box")]]
legend_grob_SP <- ggplotGrob(legend_AP)$grobs[[which(sapply(ggplotGrob(legend_AP)$grobs, function(x) x$name) == "guide-box")]]
legend_grob_SP <- ggplotGrob(legend_SP)$grobs[[which(sapply(ggplotGrob(legend_SP)$grobs, function(x) x$name) == "guide-box")]]
legend_grob_JT <- ggplotGrob(legend_JT)$grobs[[which(sapply(ggplotGrob(legend_JT)$grobs, function(x) x$name) == "guide-box")]]

# Combine the rainfall legends into a single column
legends_combined <- arrangeGrob(legend_grob_AP, legend_grob_SP, ncol = 1)

######## MAKE PLOT
# ggplot2
plots <- lapply(1:9, function(i) {
  if (i == 1 || i == 4) {
    colour_scheme <- AP_colours
    fill_aes <- aes(x = x, y = y, fill = colour_band)
  } else if (i == 5) {
    colour_scheme <- SP_colours
    fill_aes <- aes(x = x, y = y, fill = colour_band2)
  } else if (i == 2) {
    colour_scheme <- SP_colours2
    fill_aes <- aes(x = x, y = y, fill = colour_band2)
  } else if (i == 3 || i == 6) {
    colour_scheme <- JT_colours
    fill_aes <- aes(x = x, y = y, fill = colour_band3)
  } else if (i == 8) {
    colour_scheme <- Percent_change_colours2
    fill_aes <- aes(x = x, y = y, fill = colour_band4)
  } else if (i == 9) {
    colour_scheme <- Percent_change_colours3
    fill_aes <- aes(x = x, y = y, fill = colour_band4)
  } else {
    colour_scheme <- Percent_change_colours
    fill_aes <- aes(x = x, y = y, fill = colour_band4)
  }
  ggplot(Triple_df %>% filter(plot_title == as.character(i))) + 
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
row_titles <- c("  Baseline Climate (1981 - 2000)", "  Future Climate (2061 - 2080)", "  Percentage Change in Climate Variable")
row_title_grobs <- lapply(row_titles, function(title) {
  textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 18))
})

# Create a blank textGrob to add space
spacer <- textGrob(" ", gp = gpar(fontsize = 30))

# Combine the plots into a single gtable
plot_gtable <- arrangeGrob(
  arrangeGrob(grobs = list(plots[[1]], plots[[2]], plots[[3]],  legends_combined), ncol = 4, top = row_title_grobs[[1]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[4]], plots[[5]], plots[[6]], arrangeGrob( legend_grob_JT, spacer, ncol = 1)),  ncol = 4, top = row_title_grobs[[2]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  arrangeGrob(grobs = list(plots[[7]], plots[[8]], plots[[9]], Bottom_Legend), ncol = 4, top = row_title_grobs[[3]], widths = unit(c(0.25, 0.25, 0.25, 0.18), "npc")),
  ncol = 1,
  heights = unit(rep(1, 3), "null")
)

# Create text at bottom to explain maps
bottom_text <- textGrob("           The means of each climate variable are calculated from the 12 local simulation ensemble members. The percentage change in climate variable maps are the differece between \n           the baseline and future means for each variable, shown as a percentage change either positevely or negatively in respect to supporting rainforests", x = unit(0, "npc"),  just = "left", gp = gpar(fontsize = 6, hjust = 8))

#EXPORT
# Open an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_Rainforest_criteria.pdf", width = 8.27, height = 11.69)

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
  top = textGrob("Change in Alaback Rainforest Criteria\n ", gp = gpar(fontsize = 30, fontface = "bold", lineheight = 0.2))
)

# Close the PDF device
dev.off()
############### END
