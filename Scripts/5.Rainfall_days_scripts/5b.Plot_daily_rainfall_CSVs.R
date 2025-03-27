############################ Daily Rainfall ######################################

# 5b. Plot Daily rainfall CSVs

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

### Load packages
library(raster)
library(gdalUtilities)
library(sp)
library(terra)
library(sf)
library(ggplot2)

# List the Rainforests
Rainforestss <- c("Wistmans woods (WW)", "Yarner wood (YW)", "Horner wood (HW)", "Coedydd Nedd a mellte (NM)", "Cwm Elan (CE)", 
               "Coedydd Llawr y glyn (LG)", "Ceunant Llennyrch (CL)", "Naddle Forest (NF)", "Ullswater oakwoods (UO)", 
               "Borrowdale (BW)", "Banagher Glen (BG)", "Largalinny (LY)", "Taynish woods (TW)", "Glen Creran woods (GC)", 
               "Loch Ba (LB)", "Ariundle woods (AR)", "Coille Mhor (CM)", "Ardvar and Loch a Mhuilinn (AM)")

# Select columns 4 to 6 for plotting from Future_export_df
data_to_plot_Future <- Future_export_df[, 4:6]

# Convert the Future_export_df to a long format
data_long_Future <- data.frame(
  Row = rep(Rainforestss, each = ncol(data_to_plot_Future)),
  Value = as.vector(t(data_to_plot_Future)),
  Dataset = "Future (2061-2080)"
)

# Select columns 4 to 6 for plotting from Base_export_df
data_to_plot_base <- Base_export_df[, 4:6]

# Convert the Base_export_df to a long format
data_long_base <- data.frame(
  Row = rep(Rainforestss, each = ncol(data_to_plot_base)),
  Value = as.vector(t(data_to_plot_base)),
  Dataset = "Baseline (1981-2000)"
)

# Combine the two datasets
data_long <- rbind(data_long_Future, data_long_base)

# Ensure the Row factor levels are in the order of the dataframe
data_long$Row <- factor(data_long$Row, levels = Rainforestss)

# Identify the median value for each row and dataset
data_long <- data_long %>%
  group_by(Row, Dataset) %>%
  mutate(Key = ifelse(Value == median(Value), "Median", "90% Confidence limit"))

# Create a new column to define the groups
data_long <- data_long %>%
  mutate(Group = factor(case_when(
    Row %in% Rainforestss[1:3] ~ "SW England",
    Row %in% Rainforestss[4:7] ~ "Wales",
    Row %in% Rainforestss[8:10] ~ "NW England",
    Row %in% Rainforestss[11:12] ~ "N. Ireland",
    Row %in% Rainforestss[13:18] ~ "NW Scotland"
  ), levels = c("NW Scotland", "N. Ireland", "NW England", "Wales", "SW England")))

#EXPORT
# Open an A4 PDF 
pdf("Outputs/Comparison_maps/Change_in_Rainfall_Days.pdf", width = 8.27, height = 11.69)

# Create the plot
ggplot(data_long, aes(y = Row, x = Value, group = interaction(Row, Dataset), color = Dataset)) +  
  geom_point(aes(shape = Key), position = position_dodge(width = 0.75), size = 2) +
  geom_point(data = subset(data_long, Key == "90% Confidence limit"), aes(shape = Key), position = position_dodge(width = 0.75), size = 2) + 
  geom_line() +  
  scale_shape_manual(values = c("Median" = 16, "90% Confidence limit" = 124)) +  
  scale_color_manual(values = c("Future (2061-2080)" = "red", "Baseline (1981-2000)" = "blue")) +  
  scale_x_continuous(limits = c(120, 240), breaks = seq(120, 240, by = 20)) +  
  scale_y_discrete() +  
  labs(y = "UK SAC Rainforests", x = "Mean No. Rain days per year\n ", title = "Change in Rainforest Rainfall Days\n ") +  
  theme_minimal() +  
  facet_grid(Group ~ ., scales = "free_y", space = "free") +  
  theme(strip.text.y = element_text(angle = 90, hjust = 0.5, face = "bold.italic", size = 8),
axis.title.x = element_text(size = 18),  # Adjust text size for x-axis label
axis.title.y = element_text(size = 20),  # Adjust text size for y-axis label
plot.title = element_text(size = 30, face = "bold", hjust = 0.48)  # Adjust text size for plot title
)

# Close the PDF device
dev.off()
############### END
