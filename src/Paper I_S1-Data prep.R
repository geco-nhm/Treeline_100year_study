#******************************************************************************
#______________________________________________________________________________
#================================= PAPER I ====================================
#
#   Content:      Long to wide formate and variable prep
#   Authors:      Ingrid Vesterdal Tjessem, Adam Eindride Naas
#   Last edited:  19.05.2026, by Ingrid
#______________________________________________________________________________

#******************************************************************************
#******************************************************************************
# PACKAGES ####
#******************************************************************************
#******************************************************************************

library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)       # visual inspection normality
library(tidyr)
library(car)

#******************************************************************************
# PACKAGES END ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# DATA PREP ####
#******************************************************************************
#******************************************************************************

# Read in full dataset
TFL_df_prep <-
  read.csv(
    file = "input/paper_I_full_data_long_correlation.csv",
    sep = ",",
    dec = ".",
    na.strings = c("NA", "", "NULL")
  )


#Identify ID's with missing data (just 1)
missing_id <- TFL_df_prep$ID[which(is.na(TFL_df_prep$REG_alt))]
#Exclude ID's with missing data
TFL_df_prep <- TFL_df_prep[which(!TFL_df_prep$ID %in% missing_id), ]


#Function to calculate rate of change
compute.change <-
  function(data,
           variable,
           time_variable,
           id_variable,
           methods_df) {
    #Create a new data frame with only the important columns
    new_data <- data[, c(id_variable, time_variable, variable)]
    
    #Rename the columns
    colnames(new_data) <- c("id", "time", "var")
    
    #Identify matching observations
    matching <- match(new_data$id, new_data$id)
    
    #Create a vector to store the change variable
    var_change <- numeric()
    
    #Select the current method
    current_method <-
      methods_df$change_methods[methods_df$change_variables %in% variable]
    
    for (i in 1:length(unique(matching))) {
      #Identify the indices of the ith ID's that match
      current_index <- which(matching %in% unique(matching)[i])
      
      #Subset a data frame that only contain the relevant ID
      current_data <- new_data[current_index,]
      
      #Identify the index for the first year's recording
      first_year <- which.min(current_data$time)
      
      #Identify the index for the second year's recording
      last_year <- which.max(current_data$time)
      
      if (nrow(current_data) > 2) {
        #Identify the index for the middle year's recording
        middle_year <-
          which(!c(1, 2, 3) %in% c(first_year, last_year))
        
        #Compute changes between first and middle year
        new_change <-
          compute.change.method(current_data, current_method, first_year, middle_year)
        
        #Add the calculated change to the vector
        var_change <- c(var_change, new_change)
        
        #Compute changes between middle and last year
        new_change <-
          compute.change.method(current_data, current_method, middle_year, last_year)
        
        #Add the calculated change to the vector
        var_change <- c(var_change, new_change)
        
      } else {
        #Compute changes between middle and last year
        new_change <-
          compute.change.method(current_data, current_method, first_year, last_year)
        
        #Add the calculated change to the vector
        var_change <- c(var_change, new_change)
        
      }
    }
    
    #Return the vector
    return(var_change)
  }

#Define function to compute changes
compute.change.method <- function(data, method, first, last) {
  #Create vector to store results
  new_change <- numeric()
  
  if (method == 1) {
    #Calculate the change per year for the first period
    new_change <-
      (data$var[last] - data$var[first]) / (data$time[last] - data$time[first])
    
  } else if (method == 2) {
    #Calculate the change per year for the first period
    new_change <-
      (data$var[last] / data$var[first]) / (data$time[last] - data$time[first])
    
  } else if (method == 3) {
    #Calculate the change per year for the first period
    new_change <- data$var[last] - data$var[first]
    
  } else if (method == 4) {
    #Calculate the change per year for the first period
    new_change <- data$var[last] / data$var[first]
    
  }
  
  return(new_change)
  
}

#Function to calculate number of years between recordings
compute.years <- function(data, time_variable, id_variable) {
  #Create a new data frame with only the important columns
  new_data <- data[, c(id_variable, time_variable)]
  
  #Rename the columns
  colnames(new_data) <- c("id", "time")
  
  #Identify matching observations
  matching <- match(new_data$id, new_data$id)
  
  #Create a vector to store the variable
  alt_change <- numeric()
  
  for (i in 1:length(unique(matching))) {
    #Identify the indices of the ith ID's that match
    current_index <- which(matching %in% unique(matching)[i])
    
    #Subset a data frame that only contain the relevant ID
    current_data <- new_data[current_index,]
    
    #Identify the first year
    first_year <- current_data$time[which.min(current_data$time)]
    
    #Identify the last year
    last_year <- current_data$time[which.max(current_data$time)]
    
    if (nrow(current_data) > 2) {
      #Identify the middle year
      middle_year <-
        current_data$time[which(!current_data$time %in% c(first_year, last_year))]
      
      #Calculate the number of years
      new_change <- middle_year - first_year
      
      #Add the number to the vector
      alt_change <- c(alt_change, new_change)
      
      #Calculate the number of years
      new_change <- last_year - middle_year
      
      #Add the number to the vector
      alt_change <- c(alt_change, new_change)
      
    } else {
      #Calculate the number of years
      new_change <- last_year - first_year
      
      #Add the number to the vector
      alt_change <- c(alt_change, new_change)
      
    }
  }
  
  #Return the vector
  return(alt_change)
}

#Function to add other variables
add.variable <-
  function(data,
           variable,
           time_variable,
           id_variable) {
    #Create a new data frame with only the important columns
    new_data <- data[, c(id_variable, time_variable, variable)]
    
    #Rename the columns
    colnames(new_data) <- c("id", "time", "var")
    
    #Identify matching observations
    matching <- match(new_data$id, new_data$id)
    
    #Create a vector to store the variable
    alt_change <- numeric()
    
    for (i in 1:length(unique(matching))) {
      #Identify the indices of the ith ID's that match
      current_index <- which(matching %in% unique(matching)[i])
      
      #Subset a data frame that only contain the relevant ID
      current_data <- new_data[current_index,]
      
      #Identify the index for the first year's recording
      first_year <- which.min(current_data$time)
      
      #Identify the index for the last year's recording
      last_year <- which.max(current_data$time)
      
      if (nrow(current_data) > 2) {
        #Identify the index for the middle year's recording
        middle_year <-
          which(!c(1, 2, 3) %in% c(first_year, last_year))
        
        #Choose the value of the element for the last year
        new_change <- current_data$var[first_year]
        
        #Add the number to the vector
        alt_change <- c(alt_change, new_change)
        
        #Choose the value of the element for the last year
        new_change <- current_data$var[first_year]
        
        #Add the number to the vector
        alt_change <- c(alt_change, new_change)
        
      } else {
        #Choose the value of the element for the last year
        new_change <- current_data$var[first_year]
        
        #Add the number to the vector
        alt_change <- c(alt_change, new_change)
        
      }
    }
    
    #Return the vector
    return(alt_change)
  }


#Identify grazing columns
grazing_columns <- grep("grazing_", colnames(TFL_df_prep))

#Identify their names
column_names <- colnames(TFL_df_prep)[grazing_columns]

#Convert names to years
grazing_years <- sub("grazing_", "", column_names)

#Convert to numeric
grazing_years <- as.numeric(grazing_years)

#Create new column
TFL_df_prep$mean_graz_20yr <- NA

#Define function to
for (i in 1:nrow(TFL_df_prep)) {
  # Identify the current year
  current_year <- TFL_df_prep$REG_year[i]
  
  #Identify the year 20 years before
  past_year <- current_year - 20
  
  #Identify the grazing variables that span the current year and all the previous ones
  previous_years <- which(grazing_years <= current_year)
  
  #Identify the grazing variables that span "past year" and all the later ones
  later_years <- which(grazing_years >= past_year)
  
  #Identify the grazing variables that span from the year of the registration to 20 years before
  relevant_years <- which(previous_years %in% later_years)
  
  #Identify the relevant columns in the data frame
  relevant_columns <- column_names[relevant_years]
  
  #If there are no data within the period
  if (length(relevant_columns) == 0) {
    #Identify the closest year
    closest_year <- which.min(abs(current_year - grazing_years))
    
    #Identify the relevant column
    relevant_columns <- column_names[closest_year]
  }
  
  
  #Subset the data frame
  current_data_frame <- TFL_df_prep[i, relevant_columns]
  
  #Check if the object is a data frame, otherwise it should be a numeric
  if (is.data.frame(current_data_frame)) {
    #Compute the mean over all the relevant grazing years
    mean_grazing_20yr <- rowMeans(current_data_frame)
    
  } else {
    #Compute the mean over all the relevant grazing years
    mean_grazing_20yr <- mean(current_data_frame)
  }
  
  #Insert the value in the data frame
  TFL_df_prep$mean_graz_20yr[i] <- mean_grazing_20yr
  
  
}

# Properly formatted numeric vector with no spaces in the numbers
Area_municip_km2 <-
  c(
    2166,
    3180,
    2197,
    1040,
    1123,
    942,
    1277,
    1364,
    1969,
    1330,
    905,
    1141,
    742,
    1248,
    463,
    963,
    1505,
    1343,
    2274,
    2023,
    1034,
    1464,
    253,
    479,
    722,
    445,
    302,
    2704,
    3326
  )

# Properly formatted character vector with Unicode escape sequences
Municipality <-
  c(
    "Stor-Elvdal",
    "Rendalen",
    "Engerdal",
    "Os",
    "Tolga",
    "Alvdal",
    "Folldal",
    "Dovre",
    "Lom",
    "V\u00E5g\u00E5",
    "Sel",
    "Nord-Fron",
    "S\u00F8r-Fron",
    "Ringebu",
    "Vestre Slidre",
    "\u00F8ystre Slidre",
    "Vang",
    "L\u00E6rdal",
    "Oppdal",
    "Narvik",
    "Hamar\u00F8y",
    "Tysfjord",
    "Evenes",
    "V\u00E5gan",
    "Sortland",
    "Harstad",
    "Lavangen",
    "Bardu",
    "M\u00E5lselv"
  )

# Combine into a dataframe
municipality_data <- data.frame(Municipality, Area_municip_km2)

# Merge using the specific column names from each dataframe
combined_df <-
  merge(
    TFL_df_prep,
    municipality_data,
    by.x = "Mun_name",
    by.y = "Municipality",
    all.x = TRUE
  )

# Ensure the relevant columns are numeric
combined_df$mean_graz_20yr <- as.numeric(combined_df$mean_graz_20yr)
combined_df$Area_municip_km2 <-
  as.numeric(combined_df$Area_municip_km2)

# Create the new variable by dividing mean_graz_20yr by Area_municip_km2
combined_df$mean_graz_20yr_prMun <-
  combined_df$mean_graz_20yr / combined_df$Area_municip_km2

# Set a threshold for zero values
threshold <- which(combined_df$mean_graz_20yr_prMun < 4)

# Set values below the threshold to 4
combined_df$mean_graz_20yr_prMun[threshold] <- 4

# Log transform
combined_df$log_mean_graz_20yr_prMun <-
  log(combined_df$mean_graz_20yr_prMun, base = 2)


#Specify variables to calculate changes based on
change_variables <-
  c(
    "REG_alt",
    "MAT",
    "HMT",
    "LMT",
    "ATD",
    "WQ",
    "CQ",
    "M.JJAS",
    "JJAS.TD",
    "APS",
    "HMP",
    "LMP",
    "APD",
    "PSWQ",
    "PSDQ",
    "PS.JJAS",
    "JJAS.PD",
    "MT0",
    "MT5",
    "MT5.5",
    "MT6",
    "MT6.5",
    "MT7",
    "MT7.5",
    "MT10",
    "log_mean_graz_20yr_prMun"
  )


#Specify the method for estimating the change for each variable
change_methods <-   c(
  rep(x = 3, times = 9),
  rep(x = 4, times = 3),
  rep(x = 3, times = 1),
  rep(x = 4, times = 3),
  rep(x = 3, times = 9),
  rep(x = 3, times = 1)
)


#Create data frame with variables and methods
change_df <- data.frame(change_variables, change_methods)

#Compute changes based on the selected variables
results_list <- lapply(change_variables, function(variable) {
  compute.change(combined_df, variable, "REG_year", "ID", change_df)
})


#Name the list elements with the corresponding variable names
names(results_list) <- change_variables

#Convert the list to a data frame
data_change <- as.data.frame(do.call(cbind, results_list))


#Specify variables to add to the data frame
other_variables <-
  c(
    "Nr",
    "ID",
    "Forest_Tree",
    "Mun_name",
    "Mun_ID",
    "County",
    "Mount_alt",
    "REG3_alt",
    "REG3_alt_bar",
    "REG3_alt_map",
    "Veg_type",
    "Tree_height",
    "Condition",
    "Age",
    "Easting",
    "Northing",
    "Lat",
    "Long",
    "Dist_SDF",
    "REG_year",
    "REG1_Aspect",
    "REG_name",
    "REG_loc_name",
    "grazing_1890",
    "grazing_1891",
    "grazing_1907",
    "grazing_1917",
    "grazing_1918",
    "grazing_1929",
    "grazing_1939",
    "grazing_1949",
    "grazing_1959",
    "grazing_1969",
    "grazing_1979",
    "grazing_2003",
    "grazing_2004",
    "grazing_2005",
    "grazing_2006",
    "grazing_2007",
    "grazing_2008",
    "grazing_2009",
    "grazing_2010",
    "grazing_2011",
    "grazing_2012",
    "grazing_2013",
    "grazing_2014",
    "grazing_2015",
    "grazing_2016",
    "grazing_2017",
    "grazing_2018",
    "grazing_2019"
  )

#Convert the additional variables from "long" to "wide" format
results_list <- lapply(other_variables, function(variable) {
  add.variable(combined_df, variable, "REG_year", "ID")
})

#Name the list elements with the corresponding variable names
names(results_list) <- other_variables


#Convert the list to a data frame
data_descriptive <- as.data.frame(do.call(cbind, results_list))


#Bind the data frames together
data_frame <- cbind(data_descriptive,
                    years = compute.years(combined_df, "REG_year", "ID"),
                    data_change)
str(data_frame)

# Specify which ones are factor variables
factor_variables <-
  c(
    "Nr",
    "ID",
    "Forest_Tree",
    "Mun_name",
    "Mun_ID",
    "County",
    "Veg_type",
    "Age",
    "REG1_Aspect",
    "Condition",
    "REG_name",
    "REG_loc_name",
    "REG_year"
  )

# Specify which ones are numeric variables
numeric_variables <- c(
  "Mount_alt",
  "REG3_alt_bar",
  "REG3_alt_map",
  "REG3_alt",
  "Mount_alt",
  "Tree_height",
  "Easting",
  "Northing",
  "Lat",
  "Long",
  "Dist_SDF",
  "REG_alt",
  "grazing_1890",
  "grazing_1891",
  "grazing_1907",
  "grazing_1917",
  "grazing_1918",
  "grazing_1929",
  "grazing_1939",
  "grazing_1949",
  "grazing_1959",
  "grazing_1969",
  "grazing_1979",
  "grazing_2003",
  "grazing_2004",
  "grazing_2005",
  "grazing_2006",
  "grazing_2007",
  "grazing_2008",
  "grazing_2009",
  "grazing_2010",
  "grazing_2011",
  "grazing_2012",
  "grazing_2013",
  "grazing_2014",
  "grazing_2015",
  "grazing_2016",
  "grazing_2017",
  "grazing_2018",
  "grazing_2019",
  "MAT",
  "HMT",
  "LMT",
  "ATD",
  "WQ",
  "CQ",
  "M.JJAS",
  "JJAS.TD",
  "APS",
  "HMP",
  "LMP",
  "APD",
  "PSWQ",
  "PSDQ",
  "PS.JJAS",
  "JJAS.PD",
  "MT0",
  "MT5",
  "MT5.5",
  "MT6",
  "MT6.5",
  "MT7",
  "MT7.5",
  "MT10",
  "log_mean_graz_20yr_prMun"
)

# log-transformation of Dist_SDF
data_frame$Dist_SDF <- log(as.numeric(data_frame$Dist_SDF))

aspect_values <- c(
  "E" = 0.375,
  "N" = 0.125,
  "NE" = 0.125,
  "NW" = 0.375,
  "S" = 0.875,
  "SE" = 0.625,
  "SW" = 0.875,
  "W" = 0.625
)


# Applying the mapping to convert aspects to numeric values
data_frame$Aspect_favour <- aspect_values[data_frame$REG1_Aspect]
i = 1
# Convert specified columns to factor variables
for (i in seq_along(factor_variables)) {
  if (factor_variables[i] %in% names(data_frame)) {
    data_frame[, factor_variables[i]] <-
      as.factor(data_frame[, factor_variables[i]])
  }
}

for (i in seq_along(numeric_variables)) {
  if (numeric_variables[i] %in% names(data)) {
    data_frame[, numeric_variables[i]] <-
      as.numeric(data_frame[, numeric_variables[i]])
  }
}

data_frame$REG3_alt <- as.numeric(data_frame$REG3_alt)
data_frame$Tree_height <- as.numeric(data_frame$Tree_height)
data_frame$Easting <- as.numeric(data_frame$Easting)
data_frame$Northing <- as.numeric(data_frame$Northing)
data_frame$Lat <- as.numeric(data_frame$Lat)
data_frame$Long <- as.numeric(data_frame$Long)
data_frame$Mount_alt <- as.numeric(data_frame$Mount_alt)


data_frame$DistToSummit <- (data_frame$Mount_alt - 
                              (data_frame$REG3_alt - data_frame$REG_alt))

data_frame[data_frame$DistToSummit < 50,]

# Remove rows where values in DistToSummit are less than 50
#TFL_df_corr <- data_frame[data_frame$DistToSummit >= 50, ]
TFL_df_corr <- data_frame

#Save csv file to disk
write.csv(TFL_df_corr, "input/TFL_df_corr.csv")

#******************************************************************************
# DATA PREP. end ###
#******************************************************************************