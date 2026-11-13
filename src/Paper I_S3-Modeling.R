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

# Load necessary libraries (install them beforehand; see README)
library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)
library(car)
library(corrplot)

#******************************************************************************
# PACKAGES end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# DATA PREP ####
#******************************************************************************
#******************************************************************************

# Read in dataprep dataset for correlation
TL_data <- read.csv(file ="output/analysis/TL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))

FL_data <- read.csv(file ="output/analysis/FL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))


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
TL_modeling <- (TL_data[, c("Nr", "Dist_SDF", "years", "REG_alt", "ATD", "WQ", "MT5.5", "MT6", "MT6.5", "MT7", "DistToSummit")])


# Preselected EVs FLs:
#1. DistToSummit
#2. Dist_SDF
#3. MT10

colnames(FL_data)
FL_modeling <- (FL_data[, c("Nr", "Dist_SDF", "REG_alt", "MT10", "DistToSummit")])

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
#9. MT6
#10. CQ
#11. PSDQ
#12. log_mean_graz_20yr_prMun
# (Release for the paper: with the corrected data, CQ and MT6 are significantly
#  correlated with TL advance and WQ is not; see Table 2.)

# Selecting EVs for modeling df
colnames(Ascending_TL_df)
TL_Ascending_modeling <- (Ascending_TL_df[, c("Dist_SDF", "years", "REG_alt", "MAT", "CQ", "PSDQ", "MT5.5", "MT6", "MT6.5", "MT7", "log_mean_graz_20yr_prMun", "DistToSummit")])

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
  cor(Ascending_TL_df[, c("Dist_SDF", "years", "REG_alt", "MAT", "CQ", "PSDQ", "MT5.5", "MT6", "MT6.5", "MT7", "log_mean_graz_20yr_prMun", "DistToSummit")],
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
FL_Ascending_modeling <- (Ascending_FL_df[, c("years", "REG_alt", "MT10", "Aspect_favour", "DistToSummit")])

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
  cor(Ascending_FL_df[, c("Dist_SDF", "years", "REG_alt", "MAT", "WQ", "PSDQ", "MT5.5", "MT6.5", "MT7", "log_mean_graz_20yr_prMun", "DistToSummit")],
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
anova(model_TLs_0, model_TLs_1a, test = "F")        # p = 1.54e-05 ***
summary(model_TLs_1a)                               # p = 1.54e-05 ***
# Significant!


model_TLs_1b <- lm(REG_alt ~ Dist_SDF, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1b, test = "F")        # p = 0.01221 *
summary(model_TLs_1b)                               # p = 0.0122 *


model_TLs_1c <- lm(REG_alt ~ ATD, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1c, test = "F")        # p = 4.147e-05 ***
summary(model_TLs_1c)                               # p = 4.15e-05 ***
# Significant!


model_TLs_1d <- lm(REG_alt ~ WQ, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1d, test="F")          # p = 0.006099 **
summary(model_TLs_1d)                               # p = 0.0061 **
# Significant!


model_TLs_1e <- lm(REG_alt ~ MT7, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1e, test = "F")        # p = 0.0005775 ***
summary(model_TLs_1e)                               # p = 0.000578 ***
# Significant!


model_TLs_1f <- lm(REG_alt ~ MT6, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1f, test = "F")        # p = 0.01016 *
summary(model_TLs_1f)                               # p = 0.0102 * 


model_TLs_1g <- lm(REG_alt ~ MT5.5, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1g, test="F")          # p = 0.001576 **
summary(model_TLs_1g)                               # p = 0.00158 ** 
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
anova(model_TLs_0, model_TLs_1a, model_TLs_2a, test = "F")  # p = 0.0202
summary(model_TLs_2a)                                       # p = 0.0202


model_TLs_2b<-lm(REG_alt ~ MT6.5 + WQ, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2b, test = "F")  # p = 0.8803
summary(model_TLs_2b)                                       # p = 0.8803


model_TLs_2c<-lm(REG_alt ~ MT6.5 + MT7, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2c, test = "F")  # p = 0.4025
summary(model_TLs_2c)                                       # p = 0.4025


model_TLs_2d<-lm(REG_alt ~ MT6.5 + MT5.5, data = TL_modeling_df)
anova(model_TLs_0, model_TLs_1a, model_TLs_2d, test = "F")  # p = 0.1901
summary(model_TLs_2d)                                       # p = 0.1901


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
anova(model_FLs_0, model_FLs_1a, test = "F")        # p = 0.004351 **
summary(model_FLs_1a)                               # p = 0.00435 **
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
#6. CQ
#7. MT6
#8. log_mean_graz_20yr_prMun

model_ATLs_0<-lm(REG_alt ~ +1, data = TL_Ascending_modeling_df)
summary(model_ATLs_0)


###### Round 1 ######
#______________________________________________________________________________
#'        Checking the eight environmental variables for significance
#'        by comparison with null model.
#'        MT6.5, MT7, Dist_SDF, MAT, MT5.5, CQ, MT6,
#'        log_mean_graz_20yr_prMun
#'        = 8 hypotheses --> Bonferroni correction with alpha = 0.00625
#'        (p-values below: corrected data, n = 94)
#______________________________________________________________________________


model_ATLs_1a<-lm(REG_alt ~ MT6.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, test = "F")        # p = 3.99e-05 ***
summary(model_ATLs_1a)
# Significant!


model_ATLs_1b<-lm(REG_alt ~ MT7, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1b, test = "F")        # p = 0.00149 **
summary(model_ATLs_1b)
# Significant!


model_ATLs_1c<-lm(REG_alt ~ Dist_SDF, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1c, test="F")          # p = 0.0639 .
summary(model_ATLs_1c)


model_ATLs_1d<-lm(REG_alt ~ MAT, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1d,test = "F")         # p = 7.31e-05 ***
summary(model_ATLs_1d)
# Significant!


model_ATLs_1e<-lm(REG_alt ~ MT5.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1e, test = "F")        # p = 0.000185 ***
summary(model_ATLs_1e)
# Significant!


model_ATLs_1f<-lm(REG_alt ~ CQ, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1f, test="F")          # p = 0.0024 **
summary(model_ATLs_1f)
# Significant!


model_ATLs_1g<-lm(REG_alt ~ log_mean_graz_20yr_prMun, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1g, test = "F")        # p = 0.0618 .
summary(model_ATLs_1g)


model_ATLs_1h<-lm(REG_alt ~ MT6, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1h, test = "F")        # p = 0.00683 ** (not below 0.00625)
summary(model_ATLs_1h)


#______________________________________________________________________________
#'       Significant variables are:
#'       MT6.5, MT7, MAT, MT5.5, CQ
#'       We select the first EV according to the lowest p-value
#'       --> MT6.5
#______________________________________________________________________________


###### Round 2 ######
#______________________________________________________________________________
#'        Checking the remaining significant variables
#'        MT7, MAT, MT5.5, CQ
#'        = 4 hypotheses --> Bonferroni correction with alpha = 0.0125
#______________________________________________________________________________

model_ATLs_2a<-lm(REG_alt ~ MT6.5 + MT7, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2a, test = "F") # p = 0.794
summary(model_ATLs_2a)


model_ATLs_2b<-lm(REG_alt ~ MT6.5 + MAT, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2b, test = "F") # p = 0.0289
summary(model_ATLs_2b)


model_ATLs_2c<-lm(REG_alt ~ MT6.5 + MT5.5, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2c, test = "F") # p = 0.0218
summary(model_ATLs_2c)


model_ATLs_2d<-lm(REG_alt ~ MT6.5 + CQ, data = TL_Ascending_modeling_df)
anova(model_ATLs_0, model_ATLs_1a, model_ATLs_2d, test = "F") # p = 0.0178
summary(model_ATLs_2d)


#______________________________________________________________________________
#'       No variable passes alpha = 0.0125 in round 2.
#'       Final model is 1a:
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
anova(model_AFLs_0, model_AFLs_1a, test = "F")        # p = 0.02826 *
summary(model_AFLs_1a)                                # p = 0.0283 *  
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
cv_results <- list()  # collected for Table D1
(xv <- as.factor(rep(1:4, length.out = nrow(TL_modeling_df))))
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
REG_alt.Predict_NULL.1

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
PE_NULL_Sum <- sum(abs(c(PE_NULL_1, PE_NULL_2, PE_NULL_3, PE_NULL_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_change", model = "Null", prediction_error = PE_NULL_Sum)
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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_change", model = "DistToSummit (exploratory)", prediction_error = PE_Sum)
PE_Sum  # 5107.901


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_change", model = "MT6.5 (final)", prediction_error = PE_Sum)
PE_Sum  # 5719.284


######  MT6.5 and ATD (best predictor round 2) model #####--------------------------------------------------------

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv == 2 | xv == 3 | xv == 4] ~ 
           MT6.5[xv == 2 | xv == 3 | xv == 4] + 
           ATD[xv == 2 | xv == 3 | xv == 4], 
         data = TL_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_modeling_df$MT6.5[xv == 1]) + (summary(k1)$coefficients[3] * TL_modeling_df$ATD[xv == 1])
REG_alt.Predict.1

#Calculate prediction error for xv==1:
PE_1 <- TL_modeling_df$REG_alt[xv == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv == 1 | xv == 3 | xv == 4] ~ 
       MT6.5[xv == 1 | xv == 3 | xv == 4] + 
       ATD[xv == 1 | xv == 3 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_modeling_df$MT6.5[xv == 2]) + (summary(k2)$coefficients[3] * TL_modeling_df$ATD[xv == 2])
PE_2 <- TL_modeling_df$REG_alt[xv == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 4] ~ 
       MT6.5[xv == 1 | xv == 2 | xv == 4] + 
       ATD[xv == 1 | xv == 2 | xv == 4], 
     data = TL_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_modeling_df$MT6.5[xv == 3]) + (summary(k3)$coefficients[3] * TL_modeling_df$ATD[xv == 3])
PE_3 <- TL_modeling_df$REG_alt[xv == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv == 1 | xv == 2 | xv == 3] ~ 
       MT6.5[xv == 1 | xv == 2 | xv == 3] + 
       ATD[xv == 1 | xv == 2 | xv == 3], 
     data = TL_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_modeling_df$MT6.5[xv == 4]) + (summary(k4)$coefficients[3] * TL_modeling_df$ATD[xv == 4])
PE_4 <- TL_modeling_df$REG_alt[xv == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_change", model = "MT6.5 + ATD (alternative)", prediction_error = PE_Sum)
PE_Sum  # 5642.726



##### Ascending TLs #####-------------------------------------------------------

# Create the 4-Fold-vector/four subsets:
(xv1 <- as.factor(rep(1:4, length.out = nrow(TL_Ascending_modeling_df))))
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
REG_alt.Predict_NULL.1

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
PE_NULL_Sum <- sum(abs(c(PE_NULL_1, PE_NULL_2, PE_NULL_3, PE_NULL_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_advance", model = "Null", prediction_error = PE_NULL_Sum)
PE_NULL_Sum  # 4209.115


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_advance", model = "DistToSummit (exploratory)", prediction_error = PE_Sum)
PE_Sum  # 3632.742


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_advance", model = "MT6.5 (final)", prediction_error = PE_Sum)
PE_Sum  # 4229.146


###### MT6.5 and CQ (best predictor round 2) model #####--------------------------------------------------------
# (MT6.5 + MT5.5, p = 0.0218 in round 2, gives 4040.951)

#We calculate prediction errors for each k-fold prediction and compares it to the evaluation subset
k1 <- lm(REG_alt[xv1 == 2 | xv1 == 3 | xv1 == 4] ~ 
           MT6.5[xv1 == 2 | xv1 == 3 | xv1 == 4] + 
           CQ[xv1 == 2 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)

# which corresponds to an estimate of REG_alt given by
REG_alt.Predict.1 <- summary(k1)$coefficients[1] + (summary(k1)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 1]) + (summary(k1)$coefficients[3] * TL_Ascending_modeling_df$CQ[xv1 == 1])
REG_alt.Predict.1

#Calculate prediction error for xv1==1:
PE_1 <- TL_Ascending_modeling_df$REG_alt[xv1 == 1] - REG_alt.Predict.1
PE_1


#Repeat for the rest of the k-folds:
k2 <-
  lm(REG_alt[xv1 == 1 | xv1 == 3 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 3 | xv1 == 4] + 
       CQ[xv1 == 1 | xv1 == 3 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.2 <- summary(k2)$coefficients[1] + (summary(k2)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 2]) + (summary(k2)$coefficients[3] * TL_Ascending_modeling_df$CQ[xv1 == 2])
PE_2 <- TL_Ascending_modeling_df$REG_alt[xv1 == 2] - REG_alt.Predict.2
PE_2


k3 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 4] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 4] + 
       CQ[xv1 == 1 | xv1 == 2 | xv1 == 4], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.3 <- summary(k3)$coefficients[1] + (summary(k3)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 3]) + (summary(k3)$coefficients[3] * TL_Ascending_modeling_df$CQ[xv1 == 3])
PE_3 <- TL_Ascending_modeling_df$REG_alt[xv1 == 3] - REG_alt.Predict.3
PE_3


k4 <-
  lm(REG_alt[xv1 == 1 | xv1 == 2 | xv1 == 3] ~ 
       MT6.5[xv1 == 1 | xv1 == 2 | xv1 == 3] + 
       CQ[xv1 == 1 | xv1 == 2 | xv1 == 3], 
     data = TL_Ascending_modeling_df)
REG_alt.Predict.4 <- summary(k4)$coefficients[1] + (summary(k4)$coefficients[2] * TL_Ascending_modeling_df$MT6.5[xv1 == 4]) + (summary(k4)$coefficients[3] * TL_Ascending_modeling_df$CQ[xv1 == 4])
PE_4 <- TL_Ascending_modeling_df$REG_alt[xv1 == 4] - REG_alt.Predict.4
PE_4


#Calculate total prediction error:
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "TL_advance", model = "MT6.5 + CQ (alternative)", prediction_error = PE_Sum)
PE_Sum  # 4092.455


##### FLs #####-------------------------------------------------------

# Create the 4-Fold-vector/four subsets:
xv2 <- as.factor(rep(1:4, length.out = nrow(FL_modeling_df)))

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
REG_alt.Predict_NULL.1

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
PE_NULL_Sum <- sum(abs(c(PE_NULL_1, PE_NULL_2, PE_NULL_3, PE_NULL_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_change", model = "Null", prediction_error = PE_NULL_Sum)
PE_NULL_Sum  # 4359.322


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_change", model = "DistToSummit (exploratory)", prediction_error = PE_Sum)
PE_Sum  # 4139.369


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_change", model = "Dist_SDF (final)", prediction_error = PE_Sum)
PE_Sum  # 4266.601


##### Ascending FLs #####-----------------------------------------------------

# Create the 4-Fold-vector/four subsets:
xv2 <- as.factor(rep(1:4, length.out = nrow(FL_Ascending_modeling_df)))

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
REG_alt.Predict_NULL.1

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
PE_NULL_Sum <- sum(abs(c(PE_NULL_1, PE_NULL_2, PE_NULL_3, PE_NULL_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_advance", model = "Null", prediction_error = PE_NULL_Sum)
PE_NULL_Sum  # 2333.733


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_advance", model = "DistToSummit (exploratory)", prediction_error = PE_Sum)
PE_Sum  # 2343.865


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
PE_Sum <- sum(abs(c(PE_1, PE_2, PE_3, PE_4)))
cv_results[[length(cv_results) + 1]] <- data.frame(response = "FL_advance", model = "Aspect_favour (final)", prediction_error = PE_Sum)
PE_Sum  # 2235.049



#******************************************************************************
# k-FOLD CROSS VALIDATION end ###
#******************************************************************************

#******************************************************************************

#******************************************************************************
#******************************************************************************
# RESULT TABLES ####
#******************************************************************************
#******************************************************************************
# Table 2: Kendall's tau and p-values between each predictor and the four
# response variables. Advance = records with REG_alt > 20 m (same subsets as
# the models above). Written to output/analysis/Table2_kendall.csv.

table2_predictors <- c(
  "Annual precipitation anomaly"       = "APD",
  "Annual precipitation sum"           = "APS",
  "Annual temperature anomaly"         = "ATD",
  "Aspect favourability"               = "Aspect_favour",
  "Coldest quarter"                    = "CQ",
  "Mean annual temperature"            = "MAT",
  "Mean grazing pressure"              = "log_mean_graz_20yr_prMun",
  "Monthly highest temperature"        = "HMT",
  "Monthly lowest temperature"         = "LMT",
  "Monthly temperature > 0 °C"         = "MT0",
  "Monthly temperature > 5 °C"         = "MT5",
  "Monthly temperature > 5.5 °C"       = "MT5.5",
  "Monthly temperature > 6 °C"         = "MT6",
  "Monthly temperature > 6.5 °C"       = "MT6.5",
  "Monthly temperature > 7 °C"         = "MT7",
  "Monthly temperature > 7.5 °C"       = "MT7.5",
  "Tetratherm mean temperature"        = "M.JJAS",
  "Tetratherm precipitation anomaly"   = "JJAS.PD",
  "Tetratherm precipitation sum"       = "PS.JJAS",
  "Tetratherm temperature anomaly"     = "JJAS.TD",
  "Proximity to mountain summer farms" = "Dist_SDF",
  "Warmest quarter"                    = "WQ"
)

table2_responses <- list(
  TL_change  = TL_data,
  TL_advance = Ascending_TL_df,
  FL_change  = FL_data,
  FL_advance = Ascending_FL_df
)

kendall_row <- function(df, var) {
  ct <- suppressWarnings(cor.test(df[[var]], df$REG_alt, method = "kendall"))
  c(tau = unname(ct$estimate), p = ct$p.value, n = sum(complete.cases(df[[var]], df$REG_alt)))
}

Table2 <- data.frame(Predictor = names(table2_predictors), Variable = unname(table2_predictors))
for (r in names(table2_responses)) {
  res <- t(sapply(table2_predictors, function(v) kendall_row(table2_responses[[r]], v)))
  Table2[[paste0(r, "_tau")]] <- round(res[, "tau"], 4)
  Table2[[paste0(r, "_p")]]   <- round(res[, "p"], 4)
  Table2[[paste0(r, "_n")]]   <- res[, "n"]
}
write.csv(Table2, "output/analysis/Table2_kendall.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Final models (Fig. 5) with sample sizes
final_models <- list(
  TL_change  = lm(REG_alt ~ MT6.5, data = TL_modeling_df),
  TL_advance = lm(REG_alt ~ MT6.5, data = TL_Ascending_modeling_df),
  FL_change  = lm(REG_alt ~ Dist_SDF, data = FL_modeling_df),
  FL_advance = lm(REG_alt ~ Aspect_favour, data = FL_Ascending_modeling_df)
)
final_model_table <- do.call(rbind, lapply(names(final_models), function(m) {
  s <- summary(final_models[[m]])
  data.frame(model = m, predictor = rownames(s$coefficients)[2], n = nobs(final_models[[m]]),
             b = s$coefficients[2, 1], SE = s$coefficients[2, 2], p = s$coefficients[2, 4],
             R2 = s$r.squared)
}))
write.csv(final_model_table, "output/analysis/final_models.csv", row.names = FALSE)

# Table D1: summed absolute 4-fold cross-validation prediction errors
TableD1 <- do.call(rbind, cv_results)
write.csv(TableD1, "output/analysis/TableD1_crossvalidation.csv", row.names = FALSE)
print(TableD1)
print(final_model_table)
