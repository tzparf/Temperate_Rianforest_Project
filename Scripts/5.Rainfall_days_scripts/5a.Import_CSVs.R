############################ Daily Rainfall ######################################

# 5a. Daily rainfall CSVs

# This code gets the daily rainfall data from the CSVs within the input folder to an output CSV for each period with number of rainfall days for each woodland with 90% confidence limits. 

# Set working directory
setwd("D:/R_usb/Temperate_Rainforest_Project")

### Load packages
library(raster)
library(gdalUtilities)
library(sp)
library(terra)
library(sf)
library(tidyverse)

# Load up the CSVs
# Path to CSV
CSV_path <- "Inputs/CSVs"

# List the woodland name abbreviations
Woodland_names <- c("WW", "YW", "HW", "NM", "CE", "LG", "CL", "NF", "UO", "BW", "BG", "LY", "TW", "GC", "LB", "AR", "CM", "AM")

# List CSV names
CSV_names <- c("Wistmans_woods", "Yarner_wood", "Horner_wood", "Coedydd_Nedd_a_mellte", "Cwm_Elan", 
                    "Coedydd_Llawr_y_glyn", "Ceunant_Llennyrch", "Naddle_Forest", "Ullswater_oakwoods", 
                    "Borrowdale", "Banagher_Glen", "Largalinny", "Taynish_woods", "Glen_Creran_woods", 
                    "Loch_Ba", "Ariundle_woods", "Coille_Mhor", "Ardvar_and_Loch_a_Mhuilinn")

# Function to load multiple CSVs and change to woodland name and  the value of cell B2 (the coordinate value)
# Function to load multiple CSVs, extract cell B2 from unclipped data, and process clipped data
load_csvs <- function(csv_path, CSV_names, woodland_names) {
  csv_list <- lapply(seq_along(CSV_names), function(i) {
    # Load the original CSV to get cell B2
    original_df <- read.csv(file.path(csv_path, paste0(CSV_names[i], ".csv")), header = FALSE)
    b2_value <- as.character(original_df[2, 2])  # Get the value of cell B2 as a character string
    
    # Load the CSV again, skipping the first 12 rows and setting header = TRUE
    df <- read.csv(file.path(csv_path, paste0(CSV_names[i], ".csv")), header = TRUE, skip = 12)
    
    # Extract the first 6 digits and last 8 digits from cell B2
    first_6_digits <- substr(b2_value, 1, 6)
    last_8_digits <- substr(b2_value, nchar(b2_value) - 7, nchar(b2_value))
    
    # Add new columns to the data frame
    df$x <- first_6_digits
    df$y <- last_8_digits
    df$Woodland_name <- woodland_names[i]
    
    return(df)
  })
  names(csv_list) <- paste0(woodland_names, "_CSV")
  return(csv_list)
}

# Load CSVs to list
CSV_list <- load_csvs(CSV_path, CSV_names, Woodland_names)



########### CHANGE RAINFALL VALUES TO 0s and 1s
# Function to adapt values in columns 2-13
Rain_day_values <- function(csv_list) {
  RD_CSV_list <- lapply(csv_list, function(df) {
    df[, 2:13] <- lapply(df[, 2:13], function(x) ifelse(x >= 1, 1, 0))
    return(df)
  })
  return(RD_CSV_list)
}

# Modify values in columns 2-13
RD_CSV_list <- Rain_day_values(CSV_list)
head(RD_CSV_list[["HW_CSV"]])


### Creating new CSV isnt working so lets try add a new row to the RD_CSV_list that are the sums of 1:7200 and 28801:36000.
# BASELINE
# Function to process each dataframe and extract the new row
SUM_as_dataframe <- function(df) {
  # Calculate the sum for columns 2 to 13 for rows 1 to 7200 for 1981-2000
  sum_row <- colSums(df[1:7200, 2:13])
  
  # Divide the summed values by 20 to get annual average (multiplier to make it 365.25 day years rather than 360)
  sum_row <- sum_row * 1.01458 / 20
  
  # Get the values from row 7200 for columns 1, 14, 15, and 16
  new_row <- c(df[7200, 1], sum_row, df[7200, 14:16])
  
  # Convert the new row to a data frame and set column names
  new_row_df <- as.data.frame(t(new_row))
  colnames(new_row_df) <- colnames(df)
  
  # Return the new row dataframe
  return(new_row_df)
}

# Process each dataframe in the list and extract the new rows
new_rows_list <- lapply(RD_CSV_list, SUM_as_dataframe)

# Combine all new rows into one dataframe
Base_rows <- bind_rows(new_rows_list)

# Convert any list columns to atomic vectors
Base_RD <- data.frame(lapply(Base_rows, as.character), stringsAsFactors = FALSE)

# Create new columns for 2nd highest, central (6th highest), and 2nd lowest (11th highest)
Base_RD$`2nd highest` <- apply(Base_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[2])
Base_RD$central <- apply(Base_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[6])
Base_RD$`2nd lowest` <- apply(Base_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[11])

# Select columns 14 to 19 for export
Base_export_df <- Base_RD[, 14:19]

# Define the output path
CSV_output_path <- "Outputs/Rainfall_days_CSVs/Base_Rain_Days.csv"

# Write the combined new rows to a separate CSV file
write.csv(Base_export_df, CSV_output_path, row.names = FALSE)


# FUTURE
# Function to process each dataframe and extract the new row
SUM_as_dataframe <- function(df) {
  # Calculate the sum for columns 2 to 13 for rows 1 to 7200 for 1981-2000
  sum_row <- colSums(df[28801:36000, 2:13])
  
  # Divide the summed values by 20 to get annual average (multiplier to make it 365.25 day years rather than 360)
  sum_row <- sum_row * 1.01458 / 20
  
  # Get the values from row 7200 for columns 1, 14, 15, and 16
  new_row <- c(df[7200, 1], sum_row, df[36000, 14:16])
  
  # Convert the new row to a data frame and set column names
  new_row_df <- as.data.frame(t(new_row))
  colnames(new_row_df) <- colnames(df)
  
  # Return the new row dataframe
  return(new_row_df)
}

# Process each dataframe in the list and extract the new rows
new_rows_list <- lapply(RD_CSV_list, SUM_as_dataframe)

# Combine all new rows into one dataframe
Future_rows <- bind_rows(new_rows_list)

# Convert any list columns to atomic vectors
Future_RD <- data.frame(lapply(Future_rows, as.character), stringsAsFactors = FALSE)

# Create new columns for 2nd highest, central (6th highest), and 2nd lowest (11th highest)
Future_RD$`2nd highest` <- apply(Future_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[2])
Future_RD$central <- apply(Future_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[6])
Future_RD$`2nd lowest` <- apply(Future_RD[, 2:13], 1, function(row) sort(as.numeric(row), decreasing = TRUE)[11])

# Select columns 14 to 19 for export
Future_export_df <- Future_RD[, 14:19]

# Define the output path
CSV_output_path <- "Outputs/Rainfall_days_CSVs/Future_Rain_Days.csv"

# Write the combined new rows to a separate CSV file
write.csv(Future_export_df, CSV_output_path, row.names = FALSE)

####END
