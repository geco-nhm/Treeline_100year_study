#******************************************************************************
# Exploratory single-covariate correlation plots, split off from
# 'Paper I_S2-Correlation.R'. Not needed for the paper's results.
# Needs the 'ecospat' package and the objects created by S2 (source S2 first).
#******************************************************************************


#******************************************************************************
#******************************************************************************
# SINGLE COVARIATE CORRELATION ####
#******************************************************************************
#******************************************************************************

#******************************************************************************
## Land use variables #####
#******************************************************************************
### Grazing pressure-----------------------------------------------------------

# TFLs
par(mfrow = c(1, 2))
hist(TL_df_corr$log_mean_graz_20yr_prMun, main = "TLs change in grazing REG1-REG3")
hist(FL_df_corr$log_mean_graz_20yr_prMun, main = "FLs change in grazing REG1-REG3")


# Check for correlation between variables
cor.test(TL_df_corr$DistToSummit, #
         TL_df_corr$REG_alt,  method = "kendall")
# Kendalls tau = 0.1727197


library(ggpubr)
ggscatter(
  TL_df_corr,
  x = "DistToSummit",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "kendall"
)

ggscatter(
  TL_df_corr,
  x = "Long",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "kendall"
)

ggscatter(
  TL_df_corr,
  x = "MAT",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "ATD",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "WQ",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "M.JJAS",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

filtered_data <- TL_df_corr[-32, ]
ggscatter(
  TL_df_corr,
  x = "JJAS.TD",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "kendall"
)

ggscatter(
  TL_df_corr,
  x = "MT0",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "MT5.5",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "MT6",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "MT6.5",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "MT7",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "HMP",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "LMP",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "PSWQ",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

ggscatter(
  TL_df_corr,
  x = "PSDQ",
  y = "REG_alt",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
)

#******************************************************************************
#******************************************************************************
# Correlation plots ####
#******************************************************************************
#******************************************************************************

# TLs
names(TL_df_corr)
# Land use + site
ecospat.cor.plot(TL_df_corr[, c(14:19, 53)])
ecospat.cor.plot(TL_df_corr[, c(53, 78:80)])

# Temperature
ecospat.cor.plot(TL_df_corr[, c(53, 54:61)])
ecospat.cor.plot(TL_df_corr[, c(53, 70:77)])

# Beregn p-verdier for korrelasjonene
p_values <- cor.mtest(TL_df_corr[, c(53, 54:61)])
corrplot.mixed(
  cor(TL_df_corr[, c(53, 54:61)],
      use = "complete.obs"),
  method = "ellipse",
  type = "upper",
  diag = FALSE,
  outline = TRUE,
  order = "alphabet",
  tl.col = "black",
  tl.srt = 0,
  tl.offset = 2,
  p.mat = p_matrix,
  sig.level = 0.05,
  insig = "label_sig"
)


#******************************************************************************
##### DTSF #####---------------------------------------------------------------

# Plot DTSF <500m
# Backtransforming Dist_SDF to meters
TFL_df$Dist_SDF_meter <- exp(as.numeric(TFL_df$Dist_SDF))
# Filter data to include only values less than or equal to 500 meters
filtered_data <- TFL_df$Dist_SDF_meter[TFL_df$Dist_SDF_meter <= 500]

# Define breaks from 0 to 500 with an interval of 50
breaks <- seq(0, 500, by = 50)

# Create the histogram with the filtered data and specified breaks
hist(
  filtered_data,
  # Data column with values up to 500
  breaks = breaks,
  # Break points from 0 to 500
  main = "Histogram of Dist_SDF_meter (0 to 500 meters)",
  # Main title of the plot
  xlab = "Distance in meters",
  # Label for the x-axis
  ylab = "Frequency",
  # Label for the y-axis
  col = "lightblue",
  # Color of the bars
  border = "black"
)  # Border color of the bars
# only 8/274 observations with SDF less than 500 m


#******************************************************************************
#### Climate variables ####
#******************************************************************************
##### Temperature #####--------------------------------------------------------


# TLs
par(mfrow = c(4, 4))
hist(TL_df_corr$MAT,
     main = "TLs Mean Annual Temperature")
hist(TL_df_corr$HMT,
     main = "TLs Highest Monthly Temperature")
hist(TL_df_corr$LMT,
     main = "TLs Lowest Monthly Temperature")
hist(TL_df_corr$ATD,
     main = "TLs Annual Temperature Deviation")
hist(TL_df_corr$WQ,
     main = "TLs Warmest Quarter")
hist(TL_df_corr$CQ,
     main = "TLs Coldest Quarter")
hist(TL_df_corr$M.JJAS,
     main = "TLs Mean Temperature June-September")
hist(TL_df_corr$JJAS.TD,
     main = "TLs JJAS Temperature Deviation")
hist(TL_df_corr$MT0,
     main = "TLs Monthly Temperature > 0")
hist(TL_df_corr$MT5,
     main = "TLs Monthly Temperature > 5")
hist(TL_df_corr$MT5.5,
     main = "TLs Monthly Temperature > 5.5")
hist(TL_df_corr$MT6,
     main = "TLs Monthly Temperature > 5.5")
hist(TL_df_corr$MT6.5,
     main = "TLs Monthly Temperature > 6.5")
hist(TL_df_corr$MT7,
     main = "TLs Monthly Temperature > 7")
hist(TL_df_corr$MT7.5,
     main = "TLs Monthly Temperature > 7.5")
hist(TL_df_corr$MT10,
     main = "TLs Monthly Temperature > 10")
# Temperature variables missing: MMT, MTD,


# FLs
hist(FL_df$MAT,
     main = "FLs Mean Annual Temperature (20 yrs)")
hist(FL_df$HMT,
     main = "FLs Highest Monthly Temperature (20 yrs)")
hist(FL_df$LMT,
     main = "YFLs Lowest Monthly Temperature (20 yrs)")
hist(FL_df$ATD,
     main = "FLs Annual Temperature Deviation (20 yrs)")
hist(FL_df$WQ,
     main = "FLs Warmest Quarter (20 yrs)")
hist(FL_df$CQ,
     main = "FLs Coldest Quarter (20 yrs)")
hist(FL_df$M.JJAS,
     main = "FLs Mean Temperature June-September (20 yrs)")
hist(FL_df$JJAS.TD,
     main = "FLs JJAS Temperature Deviation (20 yrs)")
hist(FL_df$MT0,
     main = "FLs Monthly Temperature > 0 (20 yrs)")
hist(FL_df$MT5,
     main = "FLs Monthly Temperature > 5 (20 yrs)")
hist(FL_df$MT5.5,
     main = "FLs Monthly Temperature > 5.5 (20 yrs)")
hist(FL_df$MT6,
     main = "FLs Monthly Temperature > 6 (20 yrs)")
hist(FL_df$MT6.5,
     main = "FLs Monthly Temperature > 6.5 (20 yrs)")
hist(FL_df$MT7,
     main = "FLs Monthly Temperature > 7 (20 yrs)")
hist(FL_df$MT7.5,
     main = "FLs Monthly Temperature > 7.5 (20 yrs)")
hist(FL_df$MT10,
     main = "FLs Monthly Temperature > 10 (20 yrs)")
# Temperature variables missing: MMT, MTD,

#******************************************************************************
#******************************************************************************
##### Precipitation variables ######-------------------------------------------

# TLs
par(mfrow = c(4, 2))
hist(TL_df$APS, main = "TLs Annual Precipitation Sum")
hist(TL_df$HMP, main = "TLs Highest Monthly Precipitation")
hist(TL_df$LMP, main = "TLs Lowest Monthly Precipitation")
hist(TL_df$APD, main = "TLs Annual Precipitation Deviation")
hist(TL_df$PSWQ, main = "TLs Precipitation Sum Wettest Quarter")
hist(TL_df$PSDQ, main = "TLs Precipitation Sum Driest Quarter")
hist(TL_df$PS.JJAS, main = "TLs Precipitation Sum Jun-Sep")
hist(TL_df$JJAS.PD, main = "TLs JJAS Precipitation Deviation")
# Precipitation variables missing: MPS, MPD


# FLs
hist(FL_df$APS, main = "FLs Annual Precipitation Sum")
hist(FL_df$HMP, main = "FLs Highest Monthly Precipitation")
hist(FL_df$LMP, main = "FLs Lowest Monthly Precipitation")
hist(FL_df$APD, main = "FLs Annual Precipitation Deviation")
hist(FL_df$PSWQ, main = "FLs Precipitation Sum Wettest Quarter")
hist(FL_df$PSDQ, main = "FLs Precipitation Sum Driest Quarter")
hist(FL_df$PS.JJAS, main = "FLs Precipitation Sum Jun-Sep")
hist(FL_df$JJAS.PD, main = "FLs JJAS Precipitation Deviation")
# Precipitation variables missing: MPS, MPD


#******************************************************************************
