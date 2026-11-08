#******************************************************************************
#______________________________________________________________________________
#================================= PAPER I ====================================
#
#   Content:      Correlation of variables and response
#   Authors:      Ingrid Vesterdal Tjessem, Adam Eindride Naas
#   Last edited:  19.05.2026, by Ingrid
#______________________________________________________________________________

#******************************************************************************
#******************************************************************************
# PACKAGES ####
#******************************************************************************
#******************************************************************************

library(dplyr)


#******************************************************************************
# PACKAGES end ###
#******************************************************************************

#******************************************************************************
#******************************************************************************
# DATA PREP ####
#******************************************************************************
#******************************************************************************

# Read in full dataset
TFL_df_dataset_V6 <-
  read.csv(
    file = "input/Dataset_V6.csv",
    fileEncoding = "UTF-8-BOM",
    dec = ".",
    na.strings = c("NA", "", "NULL")
  )

# Exclude columns not needed for plotting
TFL_df_plot <- TFL_df_dataset_V6[, !(names(TFL_df_dataset_V6) %in% c(
  "Species", "Topmost", "Rep_New", "Mun_name", "Mun_ID", "County",
  "Veg_type", "Veg_symb", "Tree_height", "Condition", "Age", "Photo",
  "GPS_zone", "Easting", "Northing", "GPS_uncertainty", "Lat", "Long", "Comment", "Field_ID"))]

# Calcultating distance to summit by extracting the elevation of REG1 from mountain altitude.
TFL_df_plot$DistToSummit <- (TFL_df_plot$Mount_alt - TFL_df_plot$REG1_alt)
TFL_df_plot$years <- as.numeric(TFL_df_plot$REG3_year-TFL_df_plot$REG1_year)
TFL_df_plot$change_REG1_REG3 <- (TFL_df_plot$REG3_alt-TFL_df_plot$REG1_alt)
TFL_df_plot$REG_alt_pr_year <- as.numeric(TFL_df_plot$change_REG1_REG3/TFL_df_plot$Time_R1_R3)
summary(TFL_df_plot)


#Save csv file to disk
write.csv(TFL_df_plot, "output/analysis/TFL_df_plot.csv")
