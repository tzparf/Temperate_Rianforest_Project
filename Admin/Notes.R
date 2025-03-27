######## Notes on the scripts

# All work was done in R Studio v.4.4.1

# Make sure to set the working drive to something that will run. This project was done using windows machine, with setwd below 
setwd("D:/R_usb/Temperate_Rainforest_Project")

# Scipts are designed to be gone through chronologically, hit select All (CtrlA) and then run (CtrlEnter) on each script to run it.

# Various packages were used to complete the project, ensure these are installed by running the install.packages below. If they are already installed the the library function within the scripts will load packages
install.packages("raster")
install.packages("terra")
install.packages("sf")
install.packages("sp")
install.packages("ncdf4")
install.packages("gdalUtilities")
install.packages("colorspace")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("purrr")
install.packages("gridExtra")
install.packages("grid")

#1. Net CDF to Raster

#### NetCDF download dates
#Monthly tas 15/10/24
#Annual Pr 28/10/24
#Annual tas 31/01/25
#Summer Pr 10/02/25
#July RH 22/02/25

# The 18 daily rainfall data CSVs were all downloaded 09/03/25

# Variables left in seperate scripts for 'NetCDF to Ratser' so that they can be done seperately if that is what is all that is required. It also makes it easier to find faults and only re run o the variable that is required

# precipitation Rasters are created by converting precipitation rate in (mm/day) to total rainfall in cm.

# Each nc_file is given a number
# Summer_pr, nc_file1, nc_file2
# Annual_pr, nc_file3, nc_file4
# RHumidity, nc_file5, nc_file6
# AvTemp, nc_file7, nc_file8
# MonthlyTemp, nc_file9, nc_file10

#### For resons unkown the rasters have to be rotated 90degrees anticlockwise so that North is up

### The monthly raster are split into raster stacks.
all_ensemble_timeslices #This stacks has every time step for each ensemble member
all_monthly_means #This stack has the average temperature raster for each member

# Throughout there are scripts that rely on stuff created in earlier scripts within their title. It may be required to return to these scripts to ensure the required data is available in global environment 

# 2. Rainforest (Alaback method)
# The Rainforest Rasters are as integers, so just a 1 for rainforest or 0 for non

# On some machines 2c may not be able to be run in one go

# For the all ensemble maps (2c, 3b, 3d), the plots are made by creating a raster data frame (raster_df)

# 2d, 3c, 3e, 4b
# The output maps and areas of rainforest are determined by cell counts. These are put into a list, the 2nd, 7th(median) and 11th in order of extent are then manually entered into the ggplot.

# BASEMAPS
# There are 4 basemaps, GBR_Country is a shapefile outline of the UK. The uk_grid_5km shapefile is used to define an extent that is used for making the maps. The two landmass shapefiles are used to calculate a more accurate areas of the UK and Britain respectively.



