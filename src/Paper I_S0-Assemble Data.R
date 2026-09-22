#******************************************************************************
#______________________________________________________________________________
#================================= PAPER I ====================================
#
#   Content:      Assemble Data
#   Authors:      Adam Eindride Naas; Ingrid Vesterdal Tjessem
#   Last edited:  19.05.2026, by Ingrid
#______________________________________________________________________________

#******************************************************************************
#******************************************************************************
#### Assemble Data ####
#******************************************************************************
#******************************************************************************

#Import response data
response_data <- read.csv("input/Dataset_V6-csv.csv", sep = ";")

#Remove REG2
#response_data <- response_data[, !grepl("REG2", colnames(response_data))]

#Import climate data
climate_data <- read.csv("input/matched_climate_data.csv", sep = ";")

#Import grazing data 
grazing_data <- read.csv("input/landuse_data_csv/Grazing_mean_1889-2019_V2-csv.csv", sep = ";")

# Full_data_II
#grazing_data <- read.csv("Delta_grazing_1819-1969_V2-csv.csv", sep = ";")

#Convert from wide to long format
data_year <- response_data %>% 
  pivot_longer(cols = ends_with("_year"),
               values_to = "REG_year",
               values_drop_na = FALSE)

data_year <- as.data.frame(data_year)

keep_columns <- !colnames(response_data) %in% c("Mount_alt","REG3_alt_bar","REG3_alt_map" )
filtered_data <- response_data[,keep_columns]

# Pivoting only the remaining "_alt" columns
data_alt <- filtered_data %>%
  pivot_longer(
    cols = ends_with("_alt"),
    values_to = "REG_alt",
    values_drop_na = FALSE
  )

data_year$REG_alt <- data_alt$REG_alt

response_data$REG2_Aspect <- response_data$REG1_Aspect

data_aspect <- response_data %>% 
  pivot_longer(cols = ends_with("_Aspect"),
               values_to = "REG_Aspect",
               values_drop_na = FALSE)

data_year$REG_Aspect <- data_aspect$REG_Aspect

keep_columns <- !colnames(response_data) %in% c("REG1_loc_name", "REG2_loc_name", 
                                                "REG3_loc_name","Mun_name")
filtered_data <- response_data[,keep_columns]

# Pivoting only the remaining "_alt" columns
data_name <- filtered_data %>%
  pivot_longer(
    cols = ends_with("_name"),
    values_to = "REG_name",
    values_drop_na = FALSE
  )

data_year$REG_name <- data_name$REG_name


data_locname <- response_data %>% 
  pivot_longer(cols = ends_with("_loc_name"),
               values_to = "REG_loc_name",
               values_drop_na = FALSE)

data_year$REG_loc_name <- data_locname$REG_loc_name

response_long <- as.data.frame(data_year)

response_long <- response_long[which(complete.cases(response_long$REG_year)),]

#Add column with rows matched by municipality
response_long$grazing_cols <- unlist(lapply(response_long$Mun_name, function(x, y) match(x, y), grazing_data$Municipality))


# Create composite keys in both data frames
response_long$composite_key <- paste(response_long$ID, response_long$REG_year, sep = "_")
climate_data$composite_key <- paste(climate_data$ID, climate_data$year, sep = "_")

# Match based on composite keys
response_long$climate_cols <- unlist(lapply(response_long$composite_key, function(x) match(x, climate_data$composite_key)))



#Add column with rows matched by ID
#response_long$climate_cols <- unlist(lapply(response_long$ID, function(x, y) match(x, y), climate_data$ID))

#Identify non-duplicated columns in the climate data
non_duplicated_cols <- which(!(colnames(climate_data) %in% colnames(response_long)))

#Add variables to the data frame
updated_data <- cbind(response_long,
                      grazing_data[response_long$grazing_cols, ],
                      climate_data[response_long$climate_cols, non_duplicated_cols])

#Reset row names
rownames(updated_data) <- NULL

#Set undesired columns
undesired_columns <- c("Species","Topmost","Rep_New","Photo","Comment","grazing_cols","climate_cols","Municipality","site_name","composite_key","name","GPS_zone","GPS_uncertainty","Veg_symb","Time_R1_R3","year","REG1_name","REG2_name","REG3_name", "REG2_alt","REG1_loc_name","REG2_loc_name","REG3_loc_name","REG3_Aspect")

#Retain only desired columns
updated_data <- updated_data[,which(!( colnames(updated_data) %in% undesired_columns))]

#Save csv file to disk
write.csv(updated_data, "input/paper_i_full_data_long.csv")

#Save csv file to disk
#write.csv(updated_data, "paper_i_full_data_long_correlation.csv")

# Full_data_II
#write.csv(updated_data, "paper_i_full_data_long_II.csv")

