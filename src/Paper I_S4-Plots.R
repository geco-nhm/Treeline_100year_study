#******************************************************************************
#______________________________________________________________________________
#================================= PAPER I ====================================
#
#   Content:      Plots
#   Authors:      Ingrid Vesterdal Tjessem
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
  "corrplot",
  "see", # for model evaluation
  "performance", # for model evaluation
  "sjPlot", # for model evaluation
  "merTools" # plot
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

# Read in full dataset
TFL_df_plot <- read.csv(file ="input/TFL_df_plot.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))


# Read in dataprep dataset for correlation
TL_data <- read.csv(file ="input/TL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))

FL_data <- read.csv(file ="input/FL_df_NoSummitSyndrome.csv", sep = ",", dec = ".", na.strings = c("NA", "", "NULL"))


# Redefine variables
TFL_df_plot$Forest_Tree <- as.factor(TFL_df_plot$Forest_Tree)
TFL_df_plot$REG1_name <- as.factor(TFL_df_plot$REG1_name)
summary(TFL_df_plot)

# REG_alt = total change
TL_data$Forest_Tree <- as.character(TL_data$Forest_Tree)
TL_data$Mun_name <- as.character(TL_data$Mun_name)
TL_data$County <- as.character(TL_data$County)
TL_data$Veg_type <- as.character(TL_data$Veg_type)
TL_data$REG_name <- as.character(TL_data$REG_name)
summary(FL_data)

# REG_alt = total change
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

## TLs and FLs #----------------------------------------------------------------
TL_df_plot <- TFL_df_plot[TFL_df_plot$Forest_Tree == "TL",] # REG_alt = m/year
FL_df_plot <- TFL_df_plot[TFL_df_plot$Forest_Tree == "FL",] # REG_alt = m/year


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
  TL_data[TL_data$REG_alt >= -20 & TL_data$REG_alt <= 23,] # 25 obs.
Descending_TL_df <- TL_data[TL_data$REG_alt < -20,] # 5 obs.

#******************************************************************************
# SUBSETS end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# PRESELECTION EVs ####
#******************************************************************************
#******************************************************************************

### TLs and FLs #----------------------------------------------------------------
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
TL_modeling <- (TL_data[, c(2, 20, 23, 53:54, 58:59, 73:76, 81)])


# Preselected EVs FLs:
#1. DistToSummit
#2. Dist_SDF
#3. MT10

colnames(FL_data)
FL_modeling <- (FL_data[, c(2, 20, 23, 54, 78, 81)])

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
TL_Ascending_modeling <- (Ascending_TL_df[, c(20, 23, 53:55, 59, 68, 73, 75:76, 79, 81)])

# na.omit
TL_Ascending_modeling_df <- na.omit(TL_Ascending_modeling)


## Ascending FLs #--------------------------------------------------------------

# Preselected EVs TLs:
#1. MT10
#2. Aspect_favour


# Selecting EVs for modeling df
colnames(Ascending_FL_df)
FL_Ascending_modeling <- (Ascending_FL_df[, c(23, 53:54, 78, 80)])

# na.omit
FL_Ascending_modeling_df <- na.omit(FL_Ascending_modeling)



#******************************************************************************
# PRESELECTION EVs end ###
#******************************************************************************


#******************************************************************************
#******************************************************************************
# PLOTS ####
#******************************************************************************
#******************************************************************************

#### Mean annual change TFLs #####----------------------------------------------

# New dataframe without FF
TFL_df_plot_NoFF <- TFL_df_plot[TFL_df_plot$REG1_name != "FF", ]

# Remove unused levels in REG_name after filtrering
TFL_df_plot_NoFF$REG1_name <- droplevels(TFL_df_plot_NoFF$REG1_name)
summary(TFL_df_plot_NoFF$REG1_name)

# H0: No sign. difference between TLs and FLs advance.
tfl_test <-
  wilcox.test(TL_df_plot$REG_alt_pr_year, FL_df_plot$REG_alt_pr_year, alternative = "two.sided", paired = FALSE)
# p-value = p-value = 0.03329 and we reject H0.

# H0: No sign. difference between TLs and FLs advance.
tl_test <-
  wilcox.test(TL_df_plot$REG_alt_pr_year, alternative = "greater", paired = FALSE)
# p-value = p-value < 2.2e-16 and we reject H0.

# H0: No sign. difference from 0 FLs advance.
fl_test <-
  wilcox.test(FL_df_plot$REG_alt_pr_year, alternative = "greater", paired = FALSE)
# p-value = p-value = < 2.2e-16 and we reject H0.


# Extract p-values
tl_pval <- tl_test$p.value
fl_pval <- fl_test$p.value
tfl_pval <- tfl_test$p.value


# Define significance level labels
pval_to_signif <- function(pval) {
  if (pval < 0.001) {
    return("italic(p) < 0.001")
  } else if (pval < 0.01) {
    return("**")
  } else if (pval < 0.05) {
    return("italic(p) < 0.05")
  } else {
    return("italic(p) > 0.05")
  }
}


# Convert p-values to significance levels
tl_signif <- pval_to_signif(tl_pval)
fl_signif <- pval_to_signif(fl_pval)
tfl_signif <- pval_to_signif(tfl_pval)

# Calcculate mean and SD
# TLs
mean_TL <- mean(TFL_df_plot_NoFF$REG_alt_pr_year[TFL_df_plot_NoFF$Forest_Tree == "TL"], na.rm = TRUE)
sd(TFL_df_plot_NoFF$REG_alt_pr_year[TFL_df_plot_NoFF$Forest_Tree == "TL"], na.rm = TRUE)

# FLs
mean_FL <- mean(TFL_df_plot_NoFF$REG_alt_pr_year[TFL_df_plot_NoFF$Forest_Tree == "FL"], na.rm = TRUE)
sd(TFL_df_plot_NoFF$REG_alt_pr_year[TFL_df_plot_NoFF$Forest_Tree == "FL"], na.rm = TRUE)


# Calcculate largest mean and SD
JMN_mean <- (TFL_df_plot_NoFF[TFL_df_plot_NoFF$REG1_name == "JMN", ])

# TLs
largest_mean_TL <- mean(JMN_mean$REG_alt_pr_year[JMN_mean$Forest_Tree == "TL"], na.rm = TRUE)
sd(JMN_mean$REG_alt_pr_year[JMN_mean$Forest_Tree == "TL"], na.rm = TRUE)

# FLs
largest_mean_FL <- mean(JMN_mean$REG_alt_pr_year[JMN_mean$Forest_Tree == "FL"], na.rm = TRUE)
sd(JMN_mean$REG_alt_pr_year[JMN_mean$Forest_Tree == "FL"], na.rm = TRUE)


# Calcculate least mean and SD
# HRH
HRH_mean <- (TFL_df_plot_NoFF[TFL_df_plot_NoFF$REG1_name == "HRH", ])

# TLs
least_mean_TL <- mean(HRH_mean$REG_alt_pr_year[HRH_mean$Forest_Tree == "TL"], na.rm = TRUE)
sd(HRH_mean$REG_alt_pr_year[HRH_mean$Forest_Tree == "TL"], na.rm = TRUE)

# FLs
largest_mean_FL <- mean(HRH_mean$REG_alt_pr_year[HRH_mean$Forest_Tree == "FL"], na.rm = TRUE)
sd(HRH_mean$REG_alt_pr_year[HRH_mean$Forest_Tree == "FL"], na.rm = TRUE)

# SV
SV_mean <- (TFL_df_plot_NoFF[TFL_df_plot_NoFF$REG1_name == "SV", ])

# TLs
mean(SV_mean$REG_alt_pr_year[SV_mean$Forest_Tree == "TL"], na.rm = TRUE)
median(SV_mean$REG_alt_pr_year[SV_mean$Forest_Tree == "TL"], na.rm = TRUE)

# FLs
mean(SV_mean$REG_alt_pr_year[SV_mean$Forest_Tree == "FL"], na.rm = TRUE)
median(SV_mean$REG_alt_pr_year[SV_mean$Forest_Tree == "FL"], na.rm = TRUE)


# Change the factor order in the data
TFL_df_plot_NoFF$REG1_name   <- factor(TFL_df_plot_NoFF$REG1_name,
                                       levels = c("JMN", "HRH", "SV", "BAA"))
TFL_df_plot_NoFF$Forest_Tree <- factor(TFL_df_plot_NoFF$Forest_Tree,
                                       levels = c("TL", "FL"))


# Create the plot with annotations and median change 
ttest.plot <-
  ggplot(TFL_df_plot_NoFF,
         aes(
           x = interaction(REG1_name, Forest_Tree),
           y = REG_alt_pr_year,
           fill = Forest_Tree,
           color = Forest_Tree
         )) +
  geom_boxplot() +
  scale_fill_manual(values = c("TL" = "#8dd3c7", "FL" = "#80b1d3")) +
  scale_color_manual(values = c("TL" = "#006666", "FL" = "#006699")) +
  labs(y = "Mean annual change (m)", x = "") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 15)
  ) +
  scale_x_discrete(labels = c("JMN\n(130 years)",
                              "HRH\n(100 years)",
                              "SV\n(80 years)", 
                              "BAA\n(50 years)",
                              "JMN\n(130 years)",
                              "HRH\n(100 years)",
                              "SV\n(80 years)",
                              "BAA\n(50 years)")) +
  
  #scale_x_discrete(labels = c("BAA\n(1961-1967)", 
   #                           "HRH\n(1914-1919)", 
    #                          "JMN\n(1887-1894)", 
     #                         "SV\n(1938)", 
      #                        "BAA\n(1967)", 
       #                       "HRH\n(1913-1919)",
        #                      "JMN\n(1887-1894)", 
         #                     "SV\n(1938)"))
  # Adding the horizontal line between error bars
  annotate(
    "segment",
    x = 0.6,
    xend = 8.4,
    y = 1.8,
    color = "black",
    size = 1,
    alpha = 0.9
  ) +
  annotate(
    "segment",
    x = 0.6,
    xend = 0.6,
    y = 1.5,
    yend = 1.8,
    color = "black",
    size = 1,
    alpha = 0.9
  ) +
  annotate(
    "segment",
    x = 8.4,
    xend = 8.4,
    y = 1.5,
    yend = 1.8,
    color = "black",
    size = 1,
    alpha = 0.9
  ) +
  annotate(
    "segment",
    x = 4.6,
    xend = 8.4,
    y = mean_FL,
    color = "black",
    size = 1,
    alpha = 0.9
  ) +
  annotate(
    "segment",
    x = 0.6,
    xend = 4.4,
    y = mean_TL,
    color = "black",
    size = 1,
    alpha = 0.9
  ) +
  annotate(
    "rect",
    xmin = 2,
    xmax = 3,
    ymin = 0.74,
    ymax = 0.91,
    fill = "white",
    alpha = 0.5,
    size = 0.2
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 0.78 * 1.05,
    label = tl_signif,
    parse = TRUE,  # Enable parsing to format correctly
    size = 6,
    color = "black",
    fontface = "bold"
  ) +
  annotate(
    "rect",
    xmin = 6,
    xmax = 7,
    ymin = 0.76,
    ymax = 0.59,
    fill = "white",
    alpha = 0.5,
    size = 0.2
  ) +
  annotate(
    "text",
    x = 6.5,
    y = 0.62 * 1.07,
    label = fl_signif,
    parse = TRUE,  # Enable parsing to format correctly
    size = 6,
    color = "black",
    fontface = "bold"
  ) +
  annotate(
    "rect",
    xmin = 4.1,
    xmax = 5,
    ymin = 1.84,
    ymax = 2.2,
    fill = "white",
    alpha = 0.5,
    size = 0.2
  ) +
  annotate(
    "text",
    x = 4.5,
    y = 2,
    label = tfl_signif,
    parse = TRUE,  # Enable parsing to format correctly
    size = 6,
    color = "black",
    fontface = "bold",
    alpha = 1
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 2.4,
    label = "Treelines",
    size = 8,
    color = "black",
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 6.5,
    y = 2.4,
    label = "Forest lines",
    size = 8,
    color = "black",
    fontface = "bold"
  )


# Save the ggplot to a file
ggsave("Fig3.pdf", 
       plot = ttest.plot, 
       width = 297, 
       height = 210, 
       units = "mm", 
       device = cairo_pdf
)



## TLs #------------------------------------------------------------------------

# Final model
model_TLs <- lm(REG_alt ~ MT6.5, data = TL_modeling_df)
model_summary_TLs <- summary(model_TLs)
# Extract beta estimates, SE, and RB2
beta_TLs <- round(model_summary_TLs$coefficients[2, 1], 2)  # Coefficient for MT6.5
se_beta_TLs <- round(model_summary_TLs$coefficients[2, 2], 2)  # Standard error for the coefficient
r_squared_TLs <- round(model_summary_TLs$r.squared, 2)  # RB2 value


# QQ plot
qqnorm(resid(model_TLs))
qqline(resid(model_TLs))
plot(check_model(model_TLs))

# Estimates
plot_model(model_TLs)


# Scatterplot final model
plot(TL_modeling_df$MT6.5, TL_modeling_df$REG_alt,
     main = "Scatter Plot of all TLs",
     xlab = "MT6.5",
     ylab = "REG_alt",
     pch = 19,          # Point type
     col = "blue")     # Point color



# Model predictions
TL_modeling_df$predicted <- predict(model_TLs)


custom_colors <- c("#253494","#df65b0","#cccc33","#1a9850")


# Create the label text
label_text_TLs <- paste("\u03B2: ", beta_TLs,
                        "\nSE: ", se_beta_TLs,
                        "\nR\u00B2: ", r_squared_TLs,
                        sep = "")


# Plot with observations and model predictions
Fig_4a <- ggplot(TL_modeling_df, aes(x = MT6.5, y = REG_alt)) +
  geom_point(aes(x = MT6.5, y = REG_alt, fill = REG_name),
             shape = 21,
             # Use round shape that has both fill and border
             stroke = 0.4,
             # Thickness of the outline (border)
             color = "black",
             # Outline color
             alpha = 1, 
             show.legend = FALSE) + # Scatter plot of the observations
  guides(color = "none") +
  scale_fill_manual(values = custom_colors) +  # Use the same custom colors for filling
  geom_smooth(
    method = "lm",
    se = TRUE,
    alpha = 0.3,
    color = "black",
    size = 1.5,
    show.legend = FALSE
  ) +  # Add model estimates to the upper left corner
  annotate("text", 
           x = -.25, y = 255, 
           hjust = 0,
           label = label_text_TLs, 
           size = 4,
           color = "black", 
           fontface = "italic") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.title = element_text(lineheight = 0.8) # Change legend title to two lines
  ) +
  labs(
    title = "Treeline change",
    x = expression(
      "Difference in growing season length
with mean temperature above 6.5" * degree * "C"
    ),
    y = expression("Full rate of change (m)"),
    color = "Originators of the\nhistorical records"
  ) +
  theme(axis.title.x = element_text(vjust = -4),
        plot.margin = unit(c(1, 1, 2, 1), "lines")
        )


## FLs #------------------------------------------------------------------------

# Final modelx
model_FLs<-lm(REG_alt ~ Dist_SDF, data = FL_modeling_df)
model_summary_FLs <- summary(model_FLs)
# Extract beta estimates, SE, and RB2
# Extract beta (coefficient for Dist_SDF), SE, and RB2
beta1_FLs <- round(model_summary_FLs$coefficients[2, 1], 2)  # Coefficient for Dist_SDF
se_beta1_FLs <- round(model_summary_FLs$coefficients[2, 2], 2)  # Standard error for the coefficient
r_squared_FLs <- round(model_summary_FLs$r.squared, 2)  # RB2 value


# QQ plot
qqnorm(resid(model_FLs))
qqline(resid(model_FLs))
check_model(model_FLs)

# Estimates
plot_model(model_FLs)


# Scatterplot final model
plot(FL_modeling_df$Dist_SDF, FL_modeling_df$REG_alt,
     main = "Scatter Plot of all FLs",
     xlab = "Dist_SDF",
     ylab = "REG_alt",
     pch = 19,          # Point type
     col = "blue")     # Point color



# Model predictions
FL_modeling_df$predicted <- predict(model_FLs)
# Create the label text
label_text_FLs <- paste("\u03B2: ", beta1_FLs,
                         "\nSE: ", se_beta1_FLs,
                        "\nR\u00B2: ", r_squared_FLs,  # Use "B2" to display R squared
                         sep = "")




# Plot with observations and model predictions
Fig_4c <- ggplot(FL_modeling_df, aes(x = Dist_SDF, y = REG_alt)) +
  geom_point(
    aes(x = Dist_SDF, y = REG_alt, fill = REG_name),
    shape = 21,
    # Use round shape that has both fill and border
    stroke = 0.4,
    # Thickness of the outline (border)
    color = "black",
    # Outline color
    alpha = 1,
    show.legend = TRUE
  ) +  # Scatter plot of the observations
  scale_fill_manual(values = custom_colors) +  # Use the same custom colors for filling
  scale_x_continuous(
    labels = function(x) round(exp(x)),   # show original distance
    ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    alpha = 0.3,
    color = "black",
    size = 1.5) +
  theme_bw() +
  # Add model estimates to the upper left corner
  annotate("text", 
           x = log(150),  # put text at distance = 5 m on the x-axis
           y = 130, 
           hjust = 0,
           label = label_text_FLs, 
           size = 4, 
           color = "black", 
           fontface = "italic") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(
    title = "Forest line change",
    x = expression("Proximity to mountain summer farms (m)"),
    y = expression("Full rate of change (m)"),
    fill = "Originators of the historical records"  # Sets the legend title for fill
    ) +
  theme(axis.title.x = element_text(vjust = -1),
        plot.margin = unit(c(1, 1, 2, 1), "lines")
  )  # push title down # Optional: adjust line height


## Ascending TLs #-------------------------------------------------------------

# Final model
model_ATLs <- lm(REG_alt ~ MT6.5, data = TL_Ascending_modeling_df)
model_summary_ATLs <- summary(model_ATLs)
# Extract beta estimates, SE, and RB2
beta_ATLs <- round(model_summary_ATLs$coefficients[2, 1], 2)  # Coefficient for MT6.5
se_beta_ATLs <- round(model_summary_ATLs$coefficients[2, 2], 2)  # Standard error for the coefficient
r_squared_ATLs <- round(model_summary_ATLs$r.squared, 2)  # RB2 value

# QQ plot
qqnorm(resid(model_ATLs))
qqline(resid(model_ATLs))
check_model(model_ATLs)

# Estimates
plot_model(model_ATLs)


# Scatterplot final model
plot(TL_Ascending_modeling_df$MT6.5, TL_Ascending_modeling_df$REG_alt,
     main = "Scatter Plot of Ascending TLs",
     xlab = "MT6.5",
     ylab = "REG_alt",
     pch = 19,          # Point type
     col = "blue")     # Point color


# Model predictions
TL_Ascending_modeling_df$predicted <- predict(model_ATLs)

# Create the label text
label_text_ATLs <- paste("\u03B2: ", beta_ATLs,
                         "\nSE: ", se_beta_ATLs,
                         "\nR\u00B2: ", r_squared_ATLs,  # Use "B2" to display R squared
                         sep = "")




# Plot with observations and model predictions
Fig_4b <- ggplot(TL_Ascending_modeling_df, aes(x = MT6.5, y = REG_alt)) +
  geom_point(aes(x = MT6.5, y = REG_alt, fill = REG_name),
             shape = 21,
             # Use round shape that has both fill and border
             stroke = 0.4,
             # Thickness of the outline (border)
             color = "black",
             # Outline color
             alpha = 1,
             show.legend = TRUE) +  # Scatter plot of the observations
  scale_fill_manual(values = custom_colors) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    alpha = 0.3,
    color = "black",
    size = 1.5,
    show.legend = FALSE
  ) +
  # Add model estimates to the upper left corner
  annotate("text", 
           x = -.23, y = 255, 
           hjust = 0,
           label = label_text_ATLs, 
           size = 4, 
           color = "black", 
           fontface = "italic") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(
    title = "Treeline advance",
    x = expression(
      "Difference in growing season length
with mean temperature above 6.5"*degree*"C"),
    y = expression("Full rate of change (m)"),
    fill = "Originators of the historical records"  # Sets the legend title for fill
    ) +
  theme(axis.title.x = element_text(vjust = -4),
        plot.margin = unit(c(1, 1, 2, 1), "lines")
        )  # push title down # Optional: adjust line height


## Ascending FLs #-------------------------------------------------------------


# Final model
model_AFLs <- lm(REG_alt ~ Aspect_favour, data = FL_Ascending_modeling_df)
model_summary_AFLs <- summary(model_AFLs)
# Extract beta estimates, SE, and RB2
beta_AFLs <- round(model_summary_AFLs$coefficients[2, 1], 2)  # Coefficient for Aspect_favour
se_beta_AFLs <- round(model_summary_AFLs$coefficients[2, 2], 2)  # Standard error for the coefficient
r_squared_AFLs <- round(model_summary_AFLs$r.squared, 2)  # RB2 value

# QQ plot
qqnorm(resid(model_AFLs))
qqline(resid(model_AFLs))
check_model(model_AFLs)

# Estimates
plot_model(model_AFLs)


# Scatterplot final model
plot(FL_Ascending_modeling_df$Aspect_favour, FL_Ascending_modeling_df$REG_alt,
     main = "Scatter Plot of Ascending TLs",
     xlab = "Aspect_favour",
     ylab = "REG_alt",
     pch = 19,          # Point type
     col = "blue")     # Point color


# Model predictions
FL_Ascending_modeling_df$predicted <- predict(model_AFLs)

# Create the label text
label_text_AFLs <- paste("\u03B2: ", beta_AFLs,
                         "\nSE: ", se_beta_AFLs,
                         "\nR\u00B2: ", r_squared_AFLs,  # Use "B2" to display R squared
                         sep = "")




# Plot with observations and model predictions
Fig_4d <- ggplot(FL_Ascending_modeling_df, aes(x = Aspect_favour, y = REG_alt)) +
  geom_point(aes(x = Aspect_favour, y = REG_alt, fill = REG_name),
             shape = 21,
             # Use round shape that has both fill and border
             stroke = 0.4,
             # Thickness of the outline (border)
             color = "black",
             # Outline color
             alpha = 1,
             show.legend = TRUE) +  # Scatter plot of the observations
  scale_fill_manual(values = custom_colors) +
  
  geom_smooth(
    method = "lm",
    se = TRUE,
    alpha = 0.3,
    color = "black",
    size = 1.5,
    show.legend = FALSE
  ) +
  # Add model estimates to the upper left corner
  annotate("text", 
           x = 0.10, y = 138, 
           hjust = 0,
           label = label_text_AFLs, 
           size = 4, 
           color = "black", 
           fontface = "italic") +
  scale_x_continuous(
    breaks = c(0.125, 0.375, 0.625, 0.875)
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  labs(
    title = "Forest line advance",
    x = expression(
      "Aspect favourability"),
    y = expression("Full rate of change (m)"),
    fill = "Originators of the historical records"  # Sets the legend title for fill
  ) +
  theme(axis.title.x = element_text(vjust = -4),
        plot.margin = unit(c(1, 1, 2, 1), "lines")
  )  # push title down # Optional: adjust line height

## MT6.5 #----------------------------------------------------------------------

# Your data with ordered factors
data <- data.frame(
  MT = factor(c("MT0", "MT5", "MT5.5", "MT6", "MT6.5", "MT7", "MT7.5"),
              levels = c("MT0", "MT5", "MT5.5", "MT6", "MT6.5", "MT7", "MT7.5")),
  ValueTL = c(0.113187791355215, 0.0790694869842204, 0.123440609725415, 0.129901103291538, 0.243960608620693, 0.207626339220608, 0.0769535917873393),
  ValueFL = c(-0.0212329914640283, 0.0489110966008968, 0.103004758270397, 0.0829570846739447, 0.0593848043991257, 0.120148496099994, 0.0267234938875529)
)

# Load the ggplot2 package
library(ggplot2)

# Create the plot with the specified color gradient
ggplot(data, aes(x = MT, y = ValueTL, color = as.numeric(MT))) +  # Map color to the numeric value of MT
  geom_point(size = 5) +  # Adjust size of points for better visibility
  geom_line(aes(group = 1),  size = 1.2) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "blue") +
  labs(
    title = NULL,
    x = "Difference in growing season length [between first and last TFL record] with mean temperature",
    y = "Kendall's rank correlation coefficient (\u03C4)",
    color = NULL
  ) +
  scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue to red
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05),
                     expand = c(0.02, 0.02)) +  # Adjust space and add more breaks
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) +
  scale_x_discrete(
    labels = expression(
      "0" * degree * "C",
      "5" * degree * "C",
      "5.5" * degree * "C",
      "6" * degree * "C",
      "6.5" * degree * "C",
      "7" * degree * "C",
      "7.5" * degree * "C",
      "10" * degree * "C"
    )
  ) +
  geom_point(aes(y = ValueFL, color = as.numeric(MT), size = 5)) +
  geom_line(aes(y = ValueFL, color = as.numeric(MT), group = 1), size = 1.2) +
  geom_smooth(aes(y = ValueFL, color = as.numeric(MT),
                  method = "lm",
                  se = FALSE,
                  size = 1.2)) +
  annotate(
    "text",
    x = 1.2,
    y = 0.13,
    label = "Treeline change",
    size = 5,
    color = "black",
    fontface = "bold") +
  annotate(
    "text",
    x = 1.2,
    y = 0.04,
    label = "Forest line change",
    size = 5,
    color = "black",
    fontface = "bold")
#+
#  annotate(
#      "text",
#      x = 1.8,
#      y = -0.04,
#      label = "Growing season length with mean temperature",
#      size = 5,
#      color = "black")



## Incident plot #---------------------------------------------------------------

# Creating a new variable which indicate the records that should be exemplified
n <- nrow(TL_modeling_df)  # Get the number of rows in TL_data
# Create a new column "Incident" initialized to 0
TL_modeling_df$Incident <- 0
# Set the first 5 rows of the "Incident" column to 1
TL_modeling_df$Incident[1:5] <- 1
# View the updated data frame
head(TL_modeling_df)  # Display the first few rows to check the new column


# Assuming 'Nr' is a column in TL_modeling_df containing row numbers
Fig_4a <- ggplot(TL_modeling_df, aes(x = MT6.5, y = REG_alt)) +
  geom_point(
    aes(
      x = MT6.5,
      y = REG_alt,
      #size = factor(Incident),
      fill = REG_name
    ),
    shape = 21,
    stroke = 0.8,
    color = "black",
    alpha = 1,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_size_manual(values = c(2, 8)) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    alpha = 0.3,
    color = "black",
    size = 1.5,
    show.legend = FALSE
  ) +
 # geom_text(
#    data = subset(TL_modeling_df, Incident == 1),
#    aes(label = Nr),
#    vjust = 1.5,
#    hjust = 1,
#    size = 4
#  ) +  
  # Add model estimates to the upper left corner
  annotate("text", 
           x = -.28, y = 270, 
           hjust = 0,
           label = label_text_TLs, 
           size = 4, 
           color = "black", 
           fontface = "italic") +
  theme_bw() +
  theme(
    legend.title = element_text(lineheight = 0.8),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text = element_text(size = 12)
  ) + 
  labs(
    title = "Treeline change",
    x = expression(
      "Change in months with mean temperature above 6.5" * degree * "C"
    ),
    y = expression("Total treeline change (m)")
  )


#******************************************************************************
# PLOTS end ###
#******************************************************************************

#******************************************************************************
#******************************************************************************
# SAVEING PLOTS ####
#******************************************************************************
#******************************************************************************

# Gridding all the plots into one plot
library(ggpubr)
#ggarrange(Fig_4a, Fig_4b, Fig_4c, 
#          ncol = 1, nrow = 3, 
#          common.legend = T, 
#          font.label = list(face = "bold", color = "black"))

#Fig.4bc <- ggarrange(Fig_4b, Fig_4c, 
#          ncol = 2, nrow = 1, 
#          common.legend = T, legend = "bottom",
#          font.label = list(face = "bold", color = "black"),
#          labels = c("B", "C"))

#Fig.4a <- ggarrange(Fig_4a, 
#          ncol = 1, nrow = 1, 
#          common.legend = T, 
#          font.label = list(face = "bold", color = "black"),
#          labels = "A")

#ggarrange(Fig.4a, Fig.4bc,
#          ncol = 1, nrow = 2, 
#          font.label = list(face = "bold", color = "black"),
#          labels = "")


ggarrange(Fig_4a, Fig_4b, Fig_4c, Fig_4d,
          ncol = 2, nrow = 2, 
          common.legend = T, legend = "bottom",
          font.label = list(face = "bold", color = "black"),
          labels = c("A", "B", "C", "D"))


# Saving gridded plots
ggsave(
  "Fig5abcd_v2.pdf",
  plot = last_plot(),
  width = 210, height = 297,
  units = "mm",
  device = cairo_pdf)


# Saving MT6.5 plots
ggsave("Fig4.pdf", 
       plot = last_plot(), 
       width = 297, height = 210, 
       units = "mm",
       device = cairo_pdf)

ggsave(
  "Fig4",
  plot = last_plot(),
  width = 14, height = 8,
  units = "in",
  device = cairo_pdf)


# Appendix plot Distance summer dairy farm
par(mfrow = c(1, 1))
gghistogram(
  TL_df_plot$Dist_SDF,
  xlab = "Proximity to summer dairy farm (m)",
  ylab = "TL frequency"
)

# Saving MT6.5 plots
ggsave("Appendix_Dist_SDF_TL.pdf", 
       plot = last_plot(), 
       width = 297, height = 210, 
       units = "mm",
       device = cairo_pdf)

#******************************************************************************
# SAVEING PLOTS end ###
#******************************************************************************
