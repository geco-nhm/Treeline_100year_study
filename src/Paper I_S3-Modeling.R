#******************************************************************************
#______________________________________________________________________________
#================================= PAPER I ====================================
#
#   Content:      Forward model selection
#   Authors:      Ingrid Vesterdal Tjessem, Adam Eindride Naas
#   Last edited:  19.05.2026, by Ingrid
#______________________________________________________________________________

#******************************************************************************
#******************************************************************************
# PACKAGES ####
#******************************************************************************
#******************************************************************************

# Load necessary libraries
install.load.package <- function(x) { # custom function that loads the desired package and automatically installs any non-installed packages
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c( # vector of package names
  "data.table", # vector tools
  "ggplot2", # raster tools
  "dplyr",  # spatial data tools (geospatial data abstraction library)
  "ggpubr", # color ramps (e.g. tim.colors)
  "tidyr", # for shapefile retrieval
  "car",
  "ecospat",
  "corrplot"
)

sapply(package_vec, install.load.package) # applying install/load function to each name in package vector

#******************************************************************************
# PACKAGES end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# DATA PREP ####
#******************************************************************************
#******************************************************************************

# Read in dataprep dataset for correlation
TL_data <- read.csv(file ="input/TL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))

FL_data <- read.csv(file ="input/FL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))


TL_data$Forest_Tree <- as.character(TL_data$Forest_Tree)
TL_data$Mun_name <- as.character(TL_data$Mun_name)
TL_data$County <- as.character(TL_data$County)
TL_data$Veg_type <- as.character(TL_data$Veg_type)
TL_data$REG_name <- as.character(TL_data$REG_name)
summary(TL_data$REG_alt)

FL_data$Forest_Tree <- as.character(FL_data$Forest_Tree)
FL_data$Mun_name <- as.character(FL_data$Mun_name)
FL_data$County <- as.character(FL_data$County)
FL_data$Veg_type <- as.character(FL_data$Veg_type)
FL_data$REG_name <- as.character(FL_data$REG_name)


#******************************************************************************
# DATA PREP. end ###
#******************************************************************************



#******************************************************************************
#******************************************************************************
# SUBSETS ####
#******************************************************************************
#******************************************************************************

## Reg_name TLs #---------------------------------------------------------------
BAA_TL_df <- TL_data[TL_data$REG_name == "BAA", ]
FF_TL_df <- TL_data[TL_data$REG_name == "FF", ]
HRH_TL_df <- TL_data[TL_data$REG_name == "HRH", ]
JMN_TL_df <- TL_data[TL_data$REG_name == "JMN", ]
SV_TL_df <- TL_data[TL_data$REG_name == "SV", ]

## Reg_name FLs #---------------------------------------------------------------
BAA_FL_df <- FL_data[FL_data$REG_name == "BAA", ]
FF_FL_df <- FL_data[FL_data$REG_name == "FF", ]
HRH_FL_df <- FL_data[FL_data$REG_name == "HRH", ]
JMN_FL_df <- FL_data[FL_data$REG_name == "JMN", ]
SV_FL_df <- FL_data[FL_data$REG_name == "SV", ]


## Ascending TLs #--------------------------------------------------------------
Ascending_TL_df <- TL_data[TL_data$REG_alt > 20,] # 102 obs.
Static_TL_df <-
  TL_data[TL_data$REG_alt >= -20 & TL_data$REG_alt <= 20,] # 22 obs.
Descending_TL_df <- TL_data[TL_data$REG_alt < -20,] # 5 obs.

## Ascending FLs #--------------------------------------------------------------
Ascending_FL_df <- FL_data[FL_data$REG_alt > 20,] # 84 obs.
Static_FL_df <-
  FL_data[FL_data$REG_alt >= -20 & FL_data$REG_alt <= 20,] # 41 obs.
Descending_FL_df <- FL_data[FL_data$REG_alt < -20,] # 4 obs.
Stat_Dec_FL_df <- FL_data[FL_data$REG_alt <= 20,] # 45 obs.

### Correlations #--------------------------------------------------------------
# Define the logical vector indicating which columns are numerical
# numerical_vars_logical <- sapply(Stat_Dec_FL_df, is.numeric)
# Extract the names of numerical columns/variables
# NumericalVariables <- names(Stat_Dec_FL_df)[numerical_vars_logical]

# corr_matrices_Stat_Dec_FL_df <-
#  ComputePvaluesAndCorrelation(data = Stat_Dec_FL_df, columns = NumericalVariables)


# kendalls_Stat_Dec_FL_df <- corr_matrices_Stat_Dec_FL_df[[1]]
# pValues_Stat_Dec_FL_df <- corr_matrices_Stat_Dec_FL_df[[2]]
# 
# write.csv(kendalls_Stat_Dec_FL_df, "input/Stat_Dec_FL_df_KendallsCorrelation_matrix.csv")
# write.csv(pValues_Stat_Dec_FL_df, "input/Stat_Dec_FL_df_pValues_matrix.csv")


#******************************************************************************
# SUBSETS end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# PRESELECTION EVs ####
#******************************************************************************
#******************************************************************************

## TLs and FLs #----------------------------------------------------------------
# Preselected EVs TLs:
#1. MT6.5
#2. Dist_SDF
#3. ATD
#4. WQ
#5. MT7
#6. MT6
#7. MT5.5

# Selecting EVs for modeling df
colnames(TL_data)
TL_modeling <- (TL_data[, c(2, 20, 53:54, 58:59, 73:76, 81)])


# Preselected EVs FLs:
#1. DistToSummit
#2. Dist_SDF
#3. MT10

colnames(FL_data)
FL_modeling <- (FL_data[, c(2, 20, 54, 78, 81)])

# na.omit
TL_modeling_df <- na.omit(TL_modeling)
FL_modeling_df <- na.omit(FL_modeling)


# Corr-plot TLs
corrplot.mixed(
  cor(TL_modeling_df[, ],
      method = "kendall",
      use = "complete.obs"),
  main = "TL EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)


# Corr-plot FLs
corrplot.mixed(
  cor(FL_modeling_df[, ],
      method = "kendall",
      use = "complete.obs"),
  main = "FL EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)



## Ascending TLs #--------------------------------------------------------------

# Preselected EVs TLs:
#1. DistToSummit
#2. Years
#3. MT6.5
#4. MT7
#5. Dist_SDF
#7. MAT
#8. MT5.5
#9. WQ
#10. PSDQ
#11. log_mean_graz_20yr_prMun

# Selecting EVs for modeling df
colnames(Ascending_TL_df)
TL_Ascending_modeling <- (Ascending_TL_df[, c(20, 53:55, 59, 68, 73, 75:76, 79, 81)])

# na.omit
TL_Ascending_modeling_df <- na.omit(TL_Ascending_modeling)

# Corr-plot FLs
corrplot.mixed(
  cor(TL_Ascending_modeling_df[, ],
      method = "kendall",
      use = "complete.obs"),
  main = "TL_Ascending EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)

# Corr-plot FLs
corrplot.mixed(
  cor(Ascending_TL_df[, c(20, 53:55, 59, 68, 73, 75:76, 79, 81)],
      method = "kendall",
      use = "complete.obs"),
  main = "TL_Ascending EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)


## Ascending FLs #--------------------------------------------------------------

# Preselected EVs TLs:
#1. MT10
#2. Aspect_favour


# Selecting EVs for modeling df
colnames(Ascending_FL_df)
FL_Ascending_modeling <- (Ascending_FL_df[, c(53:54, 78, 80)])

# na.omit
FL_Ascending_modeling_df <- na.omit(FL_Ascending_modeling)

# Corr-plot FLs
corrplot.mixed(
  cor(FL_Ascending_modeling_df[, ],
      method = "kendall",
      use = "complete.obs"),
  main = "FL_Ascending EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)

# Corr-plot FLs
corrplot.mixed(
  cor(Ascending_FL_df[, c(20, 53:55, 59, 68, 73, 75:76, 79, 81)],
      method = "kendall",
      use = "complete.obs"),
  main = "FL_Ascending EVs",
  lower = "number",
  upper = "ellipse",
  outline = TRUE,
  tl.pos = "lt",
  tl.cex = 0.80,
  tl.col = "black",
  cl.cex = 0.80
)


#******************************************************************************
# PRESELECTION EVs end ###
#******************************************************************************

hist(TL_modeling_df$REG_alt)
hist(FL_modeling_df$REG_alt)

#******************************************************************************
#******************************************************************************
# MODELING ####
#******************************************************************************
#******************************************************************************

##### TLs #####----------------------------------------------------------------
# Preselected EVs:
#1. MT6.5
#2. Dist_SDF
#3. ATD
#4. WQ
#5. MT7
#6. MT6
#7. MT5.5

model_TLs_0<-lm(REG_alt ~ +1, data = TL_modeling_df)
summary(model_TLs_0)


###### Round 1 ######
#______________________________________________________________________________
#'        Checking the six variables for significance
#'        by comparison with null model. Alpha = 0.05
#'        MT6.5, Dist_SDF, ATD, WQ, MT7, MT6, MT5.5
#'        = 7 hypotheses --> Bonferroni correction with alpha = 0.00714
#______________________________________________________________________________

model_TLs_1a <- lm(REG_alt ~ MT6.5, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, test = "F")        # p = 2.994e-06 ***
summary(model_TLs_1a)                               # p = 2.99e-06 ***
# Significant!


model_TLs_1b <- lm(REG_alt ~ Dist_SDF, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1b, test = "F")        # p = 0.01221 *
summary(model_TLs_1b)                               # p = 0.0122 *


model_TLs_1c <- lm(REG_alt ~ ATD, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1c, test = "F")        # p = 9.204e-05 ***
summary(model_TLs_1c)                               # p = 9.2e-05 ***
# Significant!


model_TLs_1d <- lm(REG_alt ~ WQ, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1d, test="F")          # p = 0.002707 **
summary(model_TLs_1d)                               # p = 0.00271 **
# Significant!


model_TLs_1e <- lm(REG_alt ~ MT7, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1e, test = "F")        # p = 0.000334 ***
summary(model_TLs_1e)                               # p = 0.000334 ***
# Significant!


model_TLs_1f <- lm(REG_alt ~ MT6, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1f, test = "F")        # p = 0.01706 *
summary(model_TLs_1f)                               # p = 0.0171 * 


model_TLs_1g <- lm(REG_alt ~ MT5.5, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1g, test="F")          # p = 0.001802 **
summary(model_TLs_1g)                               # p = 0.0018 ** 
# Significant!


#______________________________________________________________________________
#'       Significant variables are: 
#'       MT6.5, ATD, WQ, MT7, MT5.5
#'       
#'       We select the first EV according to the lowest p-value 
#'       --> MT6.5
#______________________________________________________________________________


###### Round 2 ######
#______________________________________________________________________________
#'        Checking the six environmental variables for significance
#'        by comparison with null model. 
#'        ATD, WQ, MT7, MT5.5
#'        = 4 hypotheses --> Bonferroni correction with alpha = 0.0125
#______________________________________________________________________________


model_TLs_2a<-lm(REG_alt ~ MT6.5 + ATD, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2a, test = "F")  # p = 0.05692 .
summary(model_TLs_2a)                                       # p = 0.05692 .


model_TLs_2b<-lm(REG_alt ~ MT6.5 + WQ, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2b, test = "F")  # p = 0.8753 
summary(model_TLs_2b)                                       # p = 0.875343


model_TLs_2c<-lm(REG_alt ~ MT6.5 + MT7, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2c, test = "F")  # p = 0.4874
summary(model_TLs_2c)                                       # p = 0.48739    


model_TLs_2c<-lm(REG_alt ~ MT6.5 + MT5.5, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2c, test = "F")  # p = 0.2866 
summary(model_TLs_2c)                                       # p = 0.286622    


#______________________________________________________________________________
#'       
#'       Final model is 1a: 
#'       --> REG_alt ~ MT6.5
#______________________________________________________________________________


##### FLs #####----------------------------------------------------------------
# Preselected EVs:
#1. Dist_SDF
#2. MT10

model_FLs_0<-lm(REG_alt ~ +1, data = FL_modeling_df)
summary(model_FLs_0) 


###### Round 1 ######
#______________________________________________________________________________
#'        Checking the six environmental variables for significance
#'        by comparison with null model. Alpha = 0.05
#'        Dist_SDF, MT10 
#'        = 2 hypotheses --> Bonferroni correction with alpha = 0.025
#______________________________________________________________________________


model_FLs_1a<-lm(REG_alt ~ Dist_SDF, data = FL_modeling_df)
anova(model_FLs_0, model_FLs_1a, test = "F")        # p = 0.002877 **
summary(model_FLs_1a)                               # p = 0.00288 **
# Significant!

#______________________________________________________________________________
#'        Final model is 1a: 
#'       --> REG_alt ~ Dist_SDF
#______________________________________________________________________________


##### Ascending TLs #####-------------------------------------------------------
# Preselected EVs:
#1. MT6.5
#2. MT7
#3. Dist_SDF
#4. MAT
#5. MT5.5
#6. WQ
#7. log_mean_graz_20yr_prMun

model_ATLs_0<-lm(REG_alt ~ +1, data = TL_Ascending_modeling_df)
summary(model_ATLs_0)                          # p = <2e-16 ***


###### Round 1 ######
#______________________________________________________________________________
#'        Checking the six environmental variables for significance
#'        by comparison with null model. 
#'        MT6.5, MT7, Dist_SDF, MAT, MT5.5, WQ,
#'        log_mean_graz_20yr_prMun
#'        = 7 hypotheses --> Bonferroni correction with alpha = 0.00714
#______________________________________________________________________________


model_ATLs_1a<-lm(REG_alt ~ MT6.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, test = "F")        # p = 6.379e-06 ***
summary(model_ATLs_1a)                                # p = 6.38e-06 ***
# Significant!


model_ATLs_1b<-lm(REG_alt ~ MT7, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1b, test = "F")        # p = 0.0008472 ***
summary(model_ATLs_1b)                                # p = 0.000847 ***
# Significant!


model_ATLs_1c<-lm(REG_alt ~ Dist_SDF, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1c, test="F")          # p = 0.06385 .
summary(model_ATLs_1c)                                # p = 0.0639 .


model_ATLs_1d<-lm(REG_alt ~ MAT, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1d,test = "F")         # p = 0.0001218 ***
summary(model_ATLs_1d)                                # p = 0.000122 ***
# Significant!


model_ATLs_1e<-lm(REG_alt ~ MT5.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1e, test = "F")        # p = 0.0002198 ***
summary(model_ATLs_1e)                                # p = 0.00022 ***
# Significant!


model_ATLs_1f<-lm(REG_alt ~ WQ, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1f, test="F")          # p = 0.005177 **
summary(model_ATLs_1f)                                # p = 0.005177 ** 
# Significant!


model_ATLs_1g<-lm(REG_alt ~ log_mean_graz_20yr_prMun, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1g, test = "F")        # p = 0.06177 .
summary(model_ATLs_1g)                                # p = 0.0618 .


#______________________________________________________________________________
#'       Significant variables are: 
#'       MT7, MAT, MT5.5, WQ
#'       We select the first EV according to the lowest p-value 
#'       --> MT6.5
#______________________________________________________________________________


###### Round 2 ######
#______________________________________________________________________________
#'        Checking the six environmental variables for significance
#'        by comparison with null model. 
#'        MT7, MAT, MT5.5, WQ
#'        = 4 hypotheses --> Bonferroni correction with alpha = 0.0125
#______________________________________________________________________________

model_ATLs_2a<-lm(REG_alt ~ MT6.5 + MT7, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2a, test = "F") # p = 0.9886
summary(model_ATLs_2a)                                        # p = 0.988647


model_ATLs_2b<-lm(REG_alt ~ MT6.5 + MAT, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2b, test = "F") # p = 0.07515 .
summary(model_ATLs_2b)                                        # p = 0.075148 .


model_ATLs_2c<-lm(REG_alt ~ MT6.5 + MT5.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2c, test = "F") # p = 0.0396 *
summary(model_ATLs_2c)                                        # p = 0.039603 *


model_ATLs_2d<-lm(REG_alt ~ MT6.5 + WQ, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2d, test = "F") # p = 0.4447
summary(model_ATLs_2d)                                        # p = 0.444687


#______________________________________________________________________________
#'       Final model is 2a: 
#'       --> REG_alt ~ MT6.5
#______________________________________________________________________________


##### Ascending FLs #####-------------------------------------------------------
# Preselected EVs:
#1. Aspect_favour


model_AFLs_0<-lm(REG_alt ~ +1, data = FL_Ascending_modeling_df)
summary(model_AFLs_0)                          # p = <2e-16 ***


###### Round 1 ######
#______________________________________________________________________________
#'        Checking the six environmental variables for significance
#'        by comparison with null model. 
#'        Aspect_favour
#'        = 1 hypotheses --> Bonferroni correction with alpha = 0.05
#______________________________________________________________________________


model_AFLs_1a<-lm(REG_alt ~ Aspect_favour, data = FL_Ascending_modeling_df)
anova(model_AFLs_0, model_AFLs_1a, test = "F")        # p = 0.02423 *
summary(model_AFLs_1a)                                # p = 0.0242 *  
# Significant!


#______________________________________________________________________________
#'       Final model is 1a: 
#'       --> REG_alt ~ Aspect_favour
#______________________________________________________________________________



#******************************************************************************
# MODELING end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# k-FOLD CROSS VALIDATION ####
#******************************************************************************
#******************************************************************************

##### TLs #####-------------------------------------------------------

# Create the 4-Fold-vector/four subsets:
(xv <- as.factor(rep(1:4, 31)))
# brackets implies that the result is printed on the screen

set.seed(1)
(xv <- sample(x = xv, size = length(xv), replace = FALSE))

###### Null model TLs #####--------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1_NULL <- lm(REG_alt[xv == 2 | xv == 3 | xv == 4] ~ +1, 
              data = TL_modeling_df)
summary(k1_NULL)$coefficients[1]  #Intercept = 69.1828

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict_NULL.1 <- summary(k1_NULL)$coefficients[1]
REG_alt.Predict_NULL.1  #69.1828

#Calculate prediction error for xv==1:
PE_NULL_1 <- TL_modeling_df$REG_alt[xv == 1] - REG_alt.Predict_NULL.1
PE_NULL_1


#Repeat for the rest of the k-folds:
k2_NULL <-
  lm(REG_alt[xv == 1 | xv == 3 | xv == 4] ~ +1, 
     data = TL_modeling_df)
REG_alt.Predict_NULL.2 <- summary(k2_NULL)$coefficients[1]
PE_NULL_2 <- TL_modeling_df$REG_alt[xv == 2] - REG_alt.Predict_NULL.2
PE_NULL_2


k3_NULL <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 4] ~ +1, 
     data = TL_modeling_df)
REG_alt.Predict_NULL.3 <- summary(k3_NULL)$coefficients[1]
PE_NULL_3 <- TL_modeling_df$REG_alt[xv == 3] - REG_alt.Predict_NULL.3
PE_NULL_3


k4_NULL <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 3] ~ +1, 
     data = TL_modeling_df)
REG_alt.Predict_NULL.4 <- summary(k4_NULL)$coefficients[1]
PE_NULL_4 <- TL_modeling_df$REG_alt[xv == 4] - REG_alt.Predict_NULL.4
PE_NULL_4


#Calculate total prediction error:
PE_NULL_Sum <- sum(abs(PE_NULL_1) + abs(PE_NULL_2) + abs(PE_NULL_3) + abs(PE_NULL_4))
PE_NULL_Sum  # 5932.108


###### DistToSummit model TLs #####-----------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv == 2 | xv == 3 | xv == 4] ~ 
           DistToSummit[xv == 2 | xv == 3 | xv == 4], 
         data = TL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 1])
REG_alt.Predict.1

#Calculate prediction error for xv==1:
PE_1 <- TL_modeling_df$REG_alt[xv == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv == 1 | xv == 3 | xv == 4] ~ 
       DistToSummit[xv == 1 | xv == 3 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 2])
PE_2 <- TL_modeling_df$REG_alt[xv == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 4] ~ 
       DistToSummit[xv == 1 | xv == 2 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 3])
PE_3 <- TL_modeling_df$REG_alt[xv == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 3] ~ 
       DistToSummit[xv == 1 | xv == 2 | xv == 3], 
     data = TL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 4])
PE_4 <- TL_modeling_df$REG_alt[xv == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  5107.901


###### MT6.5 model TLs #####------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv == 2 | xv == 3 | xv == 4] ~ 
           MT6.5[xv == 2 | xv == 3 | xv == 4], 
         data = TL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_modeling_df$MT6.5[xv == 1])
REG_alt.Predict.1 

#Calculate prediction error for xv==1:
PE_1 <- TL_modeling_df$REG_alt[xv == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv == 1 | xv == 3 | xv == 4] ~ 
       MT6.5[xv == 1 | xv == 3 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_modeling_df$MT6.5[xv == 2])
PE_2 <- TL_modeling_df$REG_alt[xv == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 4] ~ 
       MT6.5[xv == 1 | xv == 2 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_modeling_df$MT6.5[xv == 3])
PE_3 <- TL_modeling_df$REG_alt[xv == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 3] ~ 
       MT6.5[xv == 1 | xv == 2 | xv == 3], 
     data = TL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_modeling_df$MT6.5[xv == 4])
PE_4 <- TL_modeling_df$REG_alt[xv == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  5701.148


######  MT6.5 and ATD (best predictor round 2) model #####--------------------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv == 2 | xv == 3 | xv == 4] ~ 
           DistToSummit[xv == 2 | xv == 3 | xv == 4] + 
           ATD[xv == 2 | xv == 3 | xv == 4], 
         data = TL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 1]) + (summary(k1)$coefficients[3] * TL_modeling_df$ATD[xv == 1])
REG_alt.Predict.1  #-9.47035

#Calculate prediction error for xv==1:
PE_1 <- TL_modeling_df$REG_alt[xv == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv == 1 | xv == 3 | xv == 4] ~ 
       DistToSummit[xv == 1 | xv == 3 | xv == 4] + 
       ATD[xv == 1 | xv == 3 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 2]) + (summary(k2)$coefficients[3] * TL_modeling_df$ATD[xv == 2])
PE_2 <- TL_modeling_df$REG_alt[xv == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 4] ~ 
       DistToSummit[xv == 1 | xv == 2 | xv == 4] + 
       ATD[xv == 1 | xv == 2 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 3]) + (summary(k3)$coefficients[3] * TL_modeling_df$ATD[xv == 3])
PE_3 <- TL_modeling_df$REG_alt[xv == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 3] ~ 
       DistToSummit[xv == 1 | xv == 2 | xv == 3] + 
       ATD[xv == 1 | xv == 2 | xv == 3], 
     data = TL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_modeling_df$DistToSummit[xv == 4]) + (summary(k4)$coefficients[3] * TL_modeling_df$ATD[xv == 4])
PE_4 <- TL_modeling_df$REG_alt[xv == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  # 4917.583



##### Ascending TLs #####-------------------------------------------------------

# Create the 4-Fold-vector/four subsets:
(xv1 <- as.factor(rep(1:4, 23)))
# brackets implies that the result is printed on the screen

set.seed(1)
(xv1 <- sample(x = xv1, size = length(xv1), replace = FALSE))

###### Null model ascending TLs #####--------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1_NULL <- lm(REG_alt[xv1 == 2 | xv1 == 3 | xv1 == 4] ~ +1, 
         data = TL_Ascending_modeling_df)
summary(k1_NULL)$coefficients[1]  #Intercept = 86.02817

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict_NULL.1 <- summary(k1_NULL)$coefficients[1]
REG_alt.Predict_NULL.1  #86.02817

#Calculate prediction error for xv1==1:
PE_NULL_1 <- TL_Ascending_modeling_df$REG_alt[xv1 == 1] - REG_alt.Predict_NULL.1
PE_NULL_1


#Repeat for the rest of the k-folds:
k2_NULL <-
  lm(REG_alt[xv1 == 1 | xv1 == 3 | xv1 == 4] ~ +1, 
     data = TL_Ascending_modeling_df)
REG_alt.Predict_NULL.2 <- summary(k2_NULL)$coefficients[1]
PE_NULL_2 <- TL_Ascending_modeling_df$REG_alt[xv1 == 2] - REG_alt.Predict_NULL.2
PE_NULL_2


k3_NULL <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 4] ~ +1, 
     data = TL_Ascending_modeling_df)
REG_alt.Predict_NULL.3 <- summary(k3_NULL)$coefficients[1]
PE_NULL_3 <- TL_Ascending_modeling_df$REG_alt[xv1 == 3] - REG_alt.Predict_NULL.3
PE_NULL_3


k4_NULL <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 3] ~ +1, 
     data = TL_Ascending_modeling_df)
REG_alt.Predict_NULL.4 <- summary(k4_NULL)$coefficients[1]
PE_NULL_4 <- TL_Ascending_modeling_df$REG_alt[xv1 == 4] - REG_alt.Predict_NULL.4
PE_NULL_4


#Calculate total prediction error:
PE_NULL_Sum <- sum(abs(PE_NULL_1) + abs(PE_NULL_2) + abs(PE_NULL_3) + abs(PE_NULL_4))
PE_NULL_Sum  # 4278.539


###### DistToSummit model ascending TLs #####-----------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv1 == 2 | xv1 == 3 | xv1 == 4] ~ 
           DistToSummit[xv1 == 2 | xv1 == 3 | xv1 == 4], 
         data = TL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_Ascending_modeling_df$DistToSummit[xv1 == 1])
REG_alt.Predict.1

#Calculate prediction error for xv1==1:
PE_1 <- TL_Ascending_modeling_df$REG_alt[xv1 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv1 == 1 | xv1 == 3 | xv1 == 4] ~ 
       DistToSummit[xv1 == 1 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_Ascending_modeling_df$DistToSummit[xv1 == 2])
PE_2 <- TL_Ascending_modeling_df$REG_alt[xv1 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 4] ~ 
       DistToSummit[xv1 == 1 | xv1 == 2 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_Ascending_modeling_df$DistToSummit[xv1 == 3])
PE_3 <- TL_Ascending_modeling_df$REG_alt[xv1 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 3] ~ 
       DistToSummit[xv1 == 1 | xv1 == 2 | xv1 == 3], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_Ascending_modeling_df$DistToSummit[xv1 == 4])
PE_4 <- TL_Ascending_modeling_df$REG_alt[xv1 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  3613.888


###### MT6.5 model ascending TLs #####------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv1 == 2 | xv1 == 3 | xv1 == 4] ~ 
           MT6.5[xv1 == 2 | xv1 == 3 | xv1 == 4], 
         data = TL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 1])
REG_alt.Predict.1 

#Calculate prediction error for xv1==1:
PE_1 <- TL_Ascending_modeling_df$REG_alt[xv1 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv1 == 1 | xv1 == 3 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 2])
PE_2 <- TL_Ascending_modeling_df$REG_alt[xv1 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 3])
PE_3 <- TL_Ascending_modeling_df$REG_alt[xv1 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 3] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 3], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 4])
PE_4 <- TL_Ascending_modeling_df$REG_alt[xv1 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  4044.945


###### MT6.5 and MT5.5 (best predictor round 2) model #####--------------------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv1 == 2 | xv1 == 3 | xv1 == 4] ~ 
           MT6.5[xv1 == 2 | xv1 == 3 | xv1 == 4] + 
           MT5.5[xv1 == 2 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 1]) + (summary(k1)$coefficients[3] * TL_Ascending_modeling_df$MT5.5[xv1 == 1])
REG_alt.Predict.1  #-9.47035

#Calculate prediction error for xv1==1:
PE_1 <- TL_Ascending_modeling_df$REG_alt[xv1 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv1 == 1 | xv1 == 3 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 3 | xv1 == 4] + 
       MT5.5[xv1 == 1 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 2]) + (summary(k2)$coefficients[3] * TL_Ascending_modeling_df$MT5.5[xv1 == 2])
PE_2 <- TL_Ascending_modeling_df$REG_alt[xv1 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 4] + 
       MT5.5[xv1 == 1 | xv1 == 2 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 3]) + (summary(k3)$coefficients[3] * TL_Ascending_modeling_df$MT5.5[xv1 == 3])
PE_3 <- TL_Ascending_modeling_df$REG_alt[xv1 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 3] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 3] + 
       MT5.5[xv1 == 1 | xv1 == 2 | xv1 == 3], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 4]) + (summary(k4)$coefficients[3] * TL_Ascending_modeling_df$MT5.5[xv1 == 4])
PE_4 <- TL_Ascending_modeling_df$REG_alt[xv1 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  # 3532.437


##### FLs #####-------------------------------------------------------

# Create the 4-Fold-vector/four subsets:
xv2 <- as.factor(rep(1:4, 30))

set.seed(1)
(xv2 <- sample(x = xv2, size = length(xv2), replace = FALSE))
# brackets implies that the result is printed on the screen

###### Null model FLs #####--------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1_NULL <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ +1, 
              data = FL_modeling_df)
summary(k1_NULL)$coefficients[1]  #Intercept = 40.51087

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict_NULL.1 <- summary(k1_NULL)$coefficients[1]
REG_alt.Predict_NULL.1  #40.51087

#Calculate prediction error for xv2==1:
PE_NULL_1 <- FL_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict_NULL.1
PE_NULL_1


#Repeat for the rest of the k-folds:
k2_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ +1, 
     data = FL_modeling_df)
REG_alt.Predict_NULL.2 <- summary(k2_NULL)$coefficients[1]
PE_NULL_2 <- FL_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict_NULL.2
PE_NULL_2


k3_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ +1, 
     data = FL_modeling_df)
REG_alt.Predict_NULL.3 <- summary(k3_NULL)$coefficients[1]
PE_NULL_3 <- FL_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict_NULL.3
PE_NULL_3


k4_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ +1, 
     data = FL_modeling_df)
REG_alt.Predict_NULL.4 <- summary(k4_NULL)$coefficients[1]
PE_NULL_4 <- FL_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict_NULL.4
PE_NULL_4


#Calculate total prediction error:
PE_NULL_Sum <- sum(abs(PE_NULL_1) + abs(PE_NULL_2) + abs(PE_NULL_3) + abs(PE_NULL_4))
PE_NULL_Sum  # 4307.954


###### DistToSummit FLs #####-----------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ 
           DistToSummit[xv2 == 2 | xv2 == 3 | xv2 == 4], 
         data = FL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * FL_modeling_df$DistToSummit[xv2 == 1])
REG_alt.Predict.1  

#Calculate prediction error for xv2==1:
PE_1 <- FL_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ 
       DistToSummit[xv2 == 1 | xv2 == 3 | xv2 == 4], 
     data = FL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * FL_modeling_df$DistToSummit[xv2 == 2])
PE_2 <- FL_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ 
       DistToSummit[xv2 == 1 | xv2 == 2 | xv2 == 4], 
     data = FL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1]  + (summary(k3)$coefficients[2] * FL_modeling_df$DistToSummit[xv2 == 3])
PE_3 <- FL_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ 
       DistToSummit[xv2 == 1 | xv2 == 2 | xv2 == 3], 
     data = FL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * FL_modeling_df$DistToSummit[xv2 == 4])
PE_4 <- FL_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  4076.674


###### Final model #####--------------------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ 
           Dist_SDF[xv2 == 2 | xv2 == 3 | xv2 == 4], 
         data = FL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * FL_modeling_df$Dist_SDF[xv2 == 1])


#Calculate prediction error for xv2==1:
PE_1 <- FL_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ 
       Dist_SDF[xv2 == 1 | xv2 == 3 | xv2 == 4], 
     data = FL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * FL_modeling_df$Dist_SDF[xv2 == 2])
PE_2 <- FL_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ 
       Dist_SDF[xv2 == 1 | xv2 == 2 | xv2 == 4], 
     data = FL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * FL_modeling_df$Dist_SDF[xv2 == 3])
PE_3 <- FL_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ 
       Dist_SDF[xv2 == 1 | xv2 == 2 | xv2 == 3], 
     data = FL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * FL_modeling_df$Dist_SDF[xv2 == 4])
PE_4 <- FL_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  # 4158.87


##### Ascending FLs #####-----------------------------------------------------

# Create the 4-Fold-vector/four subsets:
xv2 <- as.factor(rep(1:4, 16))

set.seed(1)
(xv2 <- sample(x = xv2, size = length(xv2), replace = FALSE))
# brackets implies that the result is printed on the screen

###### Null model Ascending FLs #####-----------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1_NULL <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ +1, 
              data = FL_Ascending_modeling_df)
summary(k1_NULL)$coefficients[1]  #Intercept = 69.33929

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict_NULL.1 <- summary(k1_NULL)$coefficients[1]
REG_alt.Predict_NULL.1  #69.33929

#Calculate prediction error for xv2==1:
PE_NULL_1 <- FL_Ascending_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict_NULL.1
PE_NULL_1


#Repeat for the rest of the k-folds:
k2_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ +1, 
     data = FL_Ascending_modeling_df)
REG_alt.Predict_NULL.2 <- summary(k2_NULL)$coefficients[1]
PE_NULL_2 <- FL_Ascending_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict_NULL.2
PE_NULL_2


k3_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ +1, 
     data = FL_Ascending_modeling_df)
REG_alt.Predict_NULL.3 <- summary(k3_NULL)$coefficients[1]
PE_NULL_3 <- FL_Ascending_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict_NULL.3
PE_NULL_3


k4_NULL <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ +1, 
     data = FL_Ascending_modeling_df)
REG_alt.Predict_NULL.4 <- summary(k4_NULL)$coefficients[1]
PE_NULL_4 <- FL_Ascending_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict_NULL.4
PE_NULL_4


#Calculate total prediction error:
PE_NULL_Sum <- sum(abs(PE_NULL_1) + abs(PE_NULL_2) + abs(PE_NULL_3) + abs(PE_NULL_4))
PE_NULL_Sum  # 2363.531


###### Distance to summit Ascending FLs #####-----------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ 
           DistToSummit[xv2 == 2 | xv2 == 3 | xv2 == 4], 
         data = FL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * FL_Ascending_modeling_df$DistToSummit[xv2 == 1])
REG_alt.Predict.1  

#Calculate prediction error for xv2==1:
PE_1 <- FL_Ascending_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ 
       DistToSummit[xv2 == 1 | xv2 == 3 | xv2 == 4], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * FL_Ascending_modeling_df$DistToSummit[xv2 == 2])
PE_2 <- FL_Ascending_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ 
       DistToSummit[xv2 == 1 | xv2 == 2 | xv2 == 4], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1]  + (summary(k3)$coefficients[2] * FL_Ascending_modeling_df$DistToSummit[xv2 == 3])
PE_3 <- FL_Ascending_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ 
       DistToSummit[xv2 == 1 | xv2 == 2 | xv2 == 3], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * FL_Ascending_modeling_df$DistToSummit[xv2 == 4])
PE_4 <- FL_Ascending_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  #  4076.674


###### Final model #####--------------------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv2 == 2 | xv2 == 3 | xv2 == 4] ~ 
           Aspect_favour[xv2 == 2 | xv2 == 3 | xv2 == 4], 
         data = FL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * FL_Ascending_modeling_df$Aspect_favour[xv2 == 1])


#Calculate prediction error for xv2==1:
PE_1 <- FL_Ascending_modeling_df$REG_alt[xv2 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv2 == 1 | xv2 == 3 | xv2 == 4] ~ 
       Aspect_favour[xv2 == 1 | xv2 == 3 | xv2 == 4], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * FL_Ascending_modeling_df$Aspect_favour[xv2 == 2])
PE_2 <- FL_Ascending_modeling_df$REG_alt[xv2 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 4] ~ 
       Aspect_favour[xv2 == 1 | xv2 == 2 | xv2 == 4], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * FL_Ascending_modeling_df$Aspect_favour[xv2 == 3])
PE_3 <- FL_Ascending_modeling_df$REG_alt[xv2 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv2 == 1 | xv2 == 2 | xv2 == 3] ~ 
       Aspect_favour[xv2 == 1 | xv2 == 2 | xv2 == 3], 
     data = FL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * FL_Ascending_modeling_df$Aspect_favour[xv2 == 4])
PE_4 <- FL_Ascending_modeling_df$REG_alt[xv2 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(PE_1) + abs(PE_2) + abs(PE_3) + abs(PE_4))
PE_Sum  # 2264.917



#******************************************************************************
# k-FOLD CROSS VALIDATION end ###
#******************************************************************************

#******************************************************************************