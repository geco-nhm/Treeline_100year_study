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

# Vector with packages


#******************************************************************************
# PACKAGES end ###
#******************************************************************************
library(dplyr)

#******************************************************************************
#******************************************************************************
# DATA PREP ####
#******************************************************************************
#******************************************************************************

# Read in full dataset
TFL_df_dataset_V6 <-
  read.csv(
    file = "input/Dataset_V6-csv.csv",
    sep = ";",
    dec = ".",
    na.strings = c("NA", "", "NULL")
  )

# Exclude specific columns by column number
TFL_df_plot <- (TFL_df_dataset_V6[, -c(4:9, 26:31, 33:40)])

# Calcultating distance to summit by extracting the elevation of REG1 from mountain altitude.
TFL_df_plot$DistToSummit <- (TFL_df_plot$Mount_alt - TFL_df_plot$REG1_alt)
TFL_df_plot$years <- as.numeric(TFL_df_plot$REG3_year-TFL_df_plot$REG1_year)
TFL_df_plot$change_REG1_REG3 <- (TFL_df_plot$REG3_alt-TFL_df_plot$REG1_alt)
TFL_df_plot$REG_alt_pr_year <- as.numeric(TFL_df_plot$change_REG1_REG3/TFL_df_plot$Time_R1_R3)
summary(TFL_df_plot)


#Save csv file to disk
write.csv(TFL_df_plot, "input/TFL_df_plot.csv")
