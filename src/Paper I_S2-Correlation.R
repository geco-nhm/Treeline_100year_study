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

library(corrplot)
library(ggpubr)


#******************************************************************************
# PACKAGES end ###
#******************************************************************************

#******************************************************************************
#******************************************************************************
# READ DATA ####
#******************************************************************************
#******************************************************************************

TFL_df_corr <- read.csv("output/analysis/TFL_df_corr.csv", sep = ",")

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
    "REG_year",
    "Aspect_favour"
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

# Convert specified columns to factor variables
for (i in seq_along(factor_variables)) {
  if (factor_variables[i] %in% names(TFL_df_corr)) {
    TFL_df_corr[, factor_variables[i]] <-
      as.factor(TFL_df_corr[, factor_variables[i]])
  }
}

for (i in seq_along(numeric_variables)) {
  if (numeric_variables[i] %in% names(TFL_df_corr)) {
    TFL_df_corr[, numeric_variables[i]] <-
      as.numeric(TFL_df_corr[, numeric_variables[i]])
  }
}


summary(TFL_df_corr)

#******************************************************************************
# READ DATA end ###
#******************************************************************************


#******************************************************************************
# CORRELATION AND P-VALUES ####
#******************************************************************************

# Define the logical vector indicating which columns are numerical
numerical_vars_logical <- sapply(TFL_df_corr, is.numeric)

# Extract the names of numerical columns/variables
NumericalVariables <- names(TFL_df_corr)[numerical_vars_logical]

# Print or use the vector of numerical variable names
print(NumericalVariables)

ComputePvaluesAndCorrelation <- function(data, columns) {
  corr_df <- data[, columns]
  
  
  ev <- names(corr_df)
  
  # Number of variables
  n <- ncol(corr_df)
  
  # Make empty datasets
  # For p-values
  pvalues <- matrix(data = NA,
                    nrow = n,
                    ncol = n)
  pvalues <- as.data.frame(pvalues)
  colnames(pvalues) <- ev
  rownames(pvalues) <- ev
  
  # For tau-values
  tauvalues <- matrix(data = NA,
                      nrow = n,
                      ncol = n)
  tauvalues <- as.data.frame(tauvalues)
  colnames(tauvalues) <- ev
  rownames(tauvalues) <- ev
  
  # Loop over all variables
  for (i in 1:n) {
    # Loop over all variables (for each variable)
    for (j in 1:n) {
      # Make an object of correlation coefficients and p-values
      corr <-
        cor.test(corr_df[, i], corr_df[, j], method = "kendall")
      
      # Insert p-value in data frame
      pvalues[i, j] <- corr$p.value
      
      # Insert tau value in data frame
      tauvalues[i, j] <- corr$estimate
      
    }
  }
  
  
  pvalues
  tauvalues
  
  
  # From here: Alternative ways to display results
  
  ## Simplified display of results (with symbols indicative of 'significance')
  symnum(cor(corr_df, method = "kendall"), abbr = TRUE)
  
  
  
  # Plot correlation matrix with corrplot
  # corrplot requires that the data are stored in a matrix format
  corr_matrix <- as.matrix(tauvalues)
  p_matrix <- as.matrix(pvalues)
  
  corr_list <- list(corr_matrix, p_matrix)
  
  return(corr_list)
}


## TLs ####
# p-values and tau for TLs
TL_df_corr <- TFL_df_corr[TFL_df_corr$Forest_Tree == "TL",]


## FLs ####
# p-values and tau for TLs
FL_df_corr <- TFL_df_corr[TFL_df_corr$Forest_Tree == "FL",]


#******************************************************************************
# P-VALUES end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# SUBSETS ####
#******************************************************************************
#******************************************************************************

## TLs and FLs #----------------------------------------------------------------
TL_df_corr <- TFL_df_corr[TFL_df_corr$Forest_Tree == "TL",]
FL_df_corr <- TFL_df_corr[TFL_df_corr$Forest_Tree == "FL",]

## Reg_name #-------------------------------------------------------------------
HRH_df_corr <- TFL_df_corr[TFL_df_corr$REG_name == "HRH",]
JMN_df_corr <- TFL_df_corr[TFL_df_corr$REG_name == "JMN",]
SV_df_corr <- TFL_df_corr[TFL_df_corr$REG_name == "SV",]
BAA_df_corr <- TFL_df_corr[TFL_df_corr$REG_name == "BAA",]

## Reg_name TLs #---------------------------------------------------------------
BAA_TL_df_corr <- TL_df_corr[TL_df_corr$REG_name == "BAA",]
FF_TL_df_corr <- TL_df_corr[TL_df_corr$REG_name == "FF",]
HRH_TL_df_corr <- TL_df_corr[TL_df_corr$REG_name == "HRH",]
JMN_TL_df_corr <- TL_df_corr[TL_df_corr$REG_name == "JMN",]
SV_TL_df_corr <- TL_df_corr[TL_df_corr$REG_name == "SV",]

## Reg_name FLs #---------------------------------------------------------------
BAA_FL_df_corr <- FL_df_corr[FL_df_corr$REG_name == "BAA",]
FF_FL_df_corr <- FL_df_corr[FL_df_corr$REG_name == "FF",]
HRH_FL_df_corr <- FL_df_corr[FL_df_corr$REG_name == "HRH",]
JMN_FL_df_corr <- FL_df_corr[FL_df_corr$REG_name == "JMN",]
SV_FL_df_corr <- FL_df_corr[FL_df_corr$REG_name == "SV",]

#******************************************************************************
# SUBSETS end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# DISTANCE TO SUMMIT ####
#******************************************************************************
#******************************************************************************

### Treelines-------------------------------------------------------------------

library(ggpubr)

TL_plots <- vector("list", 20)  # preallocate list for 20 plots

for (i in 1:20) {
  TL_df_Filtered_DistToSummit <- TL_df_corr[TL_df_corr$DistToSummit >= i*10, ]
  
  p <- ggscatter(
    TL_df_Filtered_DistToSummit,
    x = "DistToSummit",
    y = "REG_alt",
    title = i*10,
    add = "reg.line",
    conf.int = TRUE,
    cor.coef = TRUE,
    cor.method = "kendall"
  )
  
  TL_plots[[i]] <- p
}


# 2. Arrange them
TL_figure <- ggarrange(
  plotlist = TL_plots,
  ncol = 4,
  nrow = 5
)


# 3. Save the modified grob
ggsave(
  "output/figures/Appendix_SummitSyndrome_TLs.pdf",
  plot   = TL_figure,
  width  = 410,
  height = 497,
  units  = "mm",
  device = cairo_pdf
)

#for (i in 1:20) {
#  TL_df_Filtered_DistToSummit <- TL_df_corr[TL_df_corr$DistToSummit >= i*10, ]
#  print(ggscatter(
#    TL_df_Filtered_DistToSummit,
#    x = "DistToSummit",
#    y = "REG_alt",
#    title = i*10,
#    add = "reg.line",
#    conf.int = TRUE,
#    cor.coef = TRUE,
#    cor.method = "kendall"
#  )
#  )
#}

# Toppeffekten ser ut til C% korrelere med temperatur ettersom den ikke avtar.

ggscatter(
  TL_df_corr,
  x = "MT6.5",
  y = "DistToSummit",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
) # Strongly correlated (0.37) with longer growing season. Indicates The Massenerhebung effect (German for "mountain mass elevation") which describes variation in the tree line based on mountain size and location. Larger mountains have higher treelines due to heat retention and wind shadowing.


# As we cannot determine exclude the summit syndrome (it correlates with temperature) we make a buffer based on the mean TL change.
summary(TL_df_corr$REG_alt) # 65.57 m.
TL_df_corr[TL_df_corr$DistToSummit <= 65.57, ] # 10 registrations.


# New dataframe with Summit syndrome buffer
TL_df_NoSummitSyndrome <- TL_df_corr[TL_df_corr$DistToSummit >= 65.57, ]

corr_matrices_TLSummitSyndrome <-
  ComputePvaluesAndCorrelation(data = TL_df_NoSummitSyndrome, columns = NumericalVariables)

# Kendalls tau
kendalls_TLs <- corr_matrices_TLSummitSyndrome[[1]]

# p-values
pValuesTLs <- corr_matrices_TLSummitSyndrome[[2]]

# Assuming 'cor_matrix' is your matrix of correlation coefficients
#write.csv(kendalls_TLs, "TLs_KendallsCorrelation_matrix.csv")
#write.csv(pValuesTLs, "TLs_pValues_matrix.csv")


### Forest lines----------------------------------------------------------------

FL_plots <- vector("list", 20)  # preallocate list for 20 plots

for (i in 1:20) {
  FL_df_Filtered_DistToSummit <- FL_df_corr[FL_df_corr$DistToSummit >= i*10, ]
  
  p <- ggscatter(
    FL_df_Filtered_DistToSummit,
    x = "DistToSummit",
    y = "REG_alt",
    title = i*10,
    add = "reg.line",
    conf.int = TRUE,
    cor.coef = TRUE,
    cor.method = "kendall"
  )
  
  FL_plots[[i]] <- p
}


# 2. Arrange them
FL_figure <- ggarrange(
  plotlist = FL_plots,
  ncol = 4,
  nrow = 5
)


# 3. Save the modified grob
ggsave(
  "output/figures/Appendix_SummitSyndrome_FLs.pdf",
  plot   = FL_figure,
  width  = 410,
  height = 497,
  units  = "mm",
  device = cairo_pdf
)




#for (i in 1:20) {
#  FL_df_Filtered_DistToSummit <- FL_df_corr[FL_df_corr$DistToSummit >= i*10, ]
#  print(ggscatter(
#    FL_df_Filtered_DistToSummit,
#    x = "DistToSummit",
#    y = "REG_alt",
#    title = i*10,
#    add = "reg.line",
#    conf.int = TRUE,
#    cor.coef = TRUE,
#    cor.method = "kendall"
#  )
#  )
#}

# Toppeffekten ser ut til C% avta etter 140 m til toppen.

ggscatter(
  FL_df_corr,
  x = "MT6.5",
  y = "DistToSummit",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
) # Slightly correlated (0.18) with longer growing season. 


# As we cannot determine exclude the summit syndrome (it correlates with temperature) we make a buffer based on the mean FL change.
summary(FL_df_corr$REG_alt) # 44.59 m.
FL_df_corr[FL_df_corr$DistToSummit <= 44.59, ] # 6 registrations.


# New dataframe with Summit syndrome buffer
FL_df_NoSummitSyndrome <- FL_df_corr[FL_df_corr$DistToSummit >= 44.59, ]

corr_matrices_FLSummitSyndrome <-
  ComputePvaluesAndCorrelation(data = FL_df_NoSummitSyndrome, columns = NumericalVariables)

# Kendalls tau
kendalls_FLs <- corr_matrices_FLSummitSyndrome[[1]]

# p-values
pValuesFLs <- corr_matrices_FLSummitSyndrome[[2]]

# Assuming 'cor_matrix' is your matrix of correlation coefficients
#write.csv(kendalls_FLs, "input/FLs_KendallsCorrelation_matrix.csv")
#write.csv(pValuesFLs, "input/FLs_pValues_matrix.csv")

#******************************************************************************
# DISTANCE TO SUMMIT end ###
#******************************************************************************

#******************************************************************************
#******************************************************************************
# TIME CORRELATES WITH JMN ####
#******************************************************************************
#******************************************************************************

# New dataframe with Summit syndrome buffer
TL_df_NoJMN <- TL_df_corr[TL_df_corr$REG_name != "JMN", ]

# Remove unused levels in REG_name after filtrering
TL_df_NoJMN$REG_name <- droplevels(TL_df_NoJMN$REG_name)

# New dataframe with Summit syndrome buffer
TL_df_NoJMN_NoSummitSyndrome <- TL_df_NoJMN[TL_df_NoJMN$DistToSummit >= 65.57, ]
TL_df_NoJMN_NoSummitSyndrome

corr_matrices_TL_NoJMN_SummitSyndrome <-
  ComputePvaluesAndCorrelation(data = TL_df_NoJMN_NoSummitSyndrome, columns = NumericalVariables)

# Kendalls tau
kendalls_TLs_NoJMN <- corr_matrices_TL_NoJMN_SummitSyndrome[[1]]

# p-values
pValuesTLs_NoJMN <- corr_matrices_TL_NoJMN_SummitSyndrome[[2]]


# Assuming 'cor_matrix' is your matrix of correlation coefficients
#write.csv(kendalls_TLs_NoJMN, "input/TLs_NoJMN_KendallsCorrelation_matrix.csv")
#write.csv(pValuesTLs_NoJMN, "input/TLs_NoJMN_pValues_matrix.csv")


#******************************************************************************
# TIME CORRELATES WITH JMN ###
#******************************************************************************

#******************************************************************************
#******************************************************************************
# SAVING DATAFRAMES ####
#******************************************************************************
#******************************************************************************

write.csv(TL_df_NoSummitSyndrome, "output/analysis/TL_df_NoSummitSyndrome.csv")
write.csv(FL_df_NoSummitSyndrome, "output/analysis/FL_df_NoSummitSyndrome.csv")
