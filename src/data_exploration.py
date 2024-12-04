#%%
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
import numpy as np
import difflib
import json

# Define the home folder path
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\'
#%%
# Load the treeline data - from file wide_to_long.py
import wide_to_long 
tfl = pd.read_csv(home_folder + 'Treeline_long_V3.csv')
climate_data = pd.read_csv(home_folder + 'climate_data.csv', encoding='latin1')
#%%
# Perform partial matching between 'ID' and 'site_name'
tfl['matched_site_name'] = tfl['ID'].apply(lambda x: difflib.get_close_matches(x, climate_data['site_name'], n=1, cutoff=0.8))
tfl['matched_site_name'] = tfl['matched_site_name'].apply(lambda x: x[0] if x else None)

# Drop rows where no match was found
tfl_matched = tfl.dropna(subset=['matched_site_name'])

# Merge the datasets on 'matched_site_name' and 'site_name'
merged_data = pd.merge(tfl_matched, climate_data, left_on='matched_site_name', right_on='site_name', how='inner')
#%%

# Calculate the correlation matrix between REG_alt (treeline altitude) and climate variables
correlation_matrix = merged_data.corr()
#%%
# # Plotting a heatmap of the correlation matrix
# plt.figure(figsize=(12, 8))
# sns.heatmap(correlation_matrix, annot=True, cmap='coolwarm', fmt=".2f", cbar=True)
# plt.title('Correlation of Climate Variables with Treeline Altitude (REG_alt)')
# plt.show()

#%%
# Calculate the mean REG_alt and M-JJAS for each REG_year
mean_reg_alt_m_jjas = merged_data.groupby('REG_year')[['REG_alt', 'M-JJAS']].mean().reset_index()

# Recalculate the mean trends for REG_alt for each county, year, and forest type
mean_trends = tfl.groupby(['County', 'REG_year', 'Forest_Tree'])['REG_alt'].mean().reset_index()

# Define unique counties and markers for plotting
counties = tfl['County'].unique()
markers = {'TL': 'o', 'FL': '^'}

# Plotting treeline altitude and M-JJAS on different scales
fig, ax1 = plt.subplots(figsize=(12, 8))

# Define the first axis for Treeline Altitude
ax1.set_xlabel('Year')
ax1.set_ylabel('Treeline Altitude (m)')
ax1.set_title('Treeline Altitude and M-JJAS Over Time by County and Type')
ax1.set_ylim(0, tfl['REG_alt'].max() + 200)  # Adjust the y-limit for better visualization

# Plot scatter points and trend lines for Treeline Altitude
for county in counties:
    county_data = tfl[tfl['County'] == county]
    for tree_type in county_data['Forest_Tree'].unique():
        type_data = county_data[county_data['Forest_Tree'] == tree_type]
        # Adding jitter to REG_year to avoid overlapping points
        jittered_years = type_data['REG_year'] + np.random.uniform(-2, 2, size=len(type_data))
        ax1.scatter(jittered_years, type_data['REG_alt'], 
                    label=f"{county} - {tree_type}", marker=markers[tree_type], alpha=0.7)

        # Plot trend line for mean REG_alt
        trend_data = mean_trends[(mean_trends['County'] == county) & (mean_trends['Forest_Tree'] == tree_type)]
        ax1.plot(trend_data['REG_year'], trend_data['REG_alt'], linestyle='--', color='black')

# Define the second axis for one of the available variables
#'MAT', 'HMT', 'LMT', 'ATD', 'WQ', 'CQ', 'M-JJAS', 'JJAS-TD', 'APS', 'HMP', 'LMP', 'APD', 'PSWQ', 'PSDQ', 'PS-JJAS', 'JJAS-PD', 'MT0', 'MT5', 'MT5.5', 'MT6', 'MT6.5', 'MT7', 'MT7.5', 'MT10'
with open('climate_variables_dict.json', 'r') as climate_variables_dict:
    climate_dict = json.load(climate_variables_dict)
    #print(climate_dict)

# Define a variable to be plotted from the climate dictionary using shortcut and label
variable_shortcut = 'MAT'  # Change this to any desired shortcut from the dictionary
variable_description = climate_dict.get(variable_shortcut)

ax2 = ax1.twinx()
ax2.set_ylabel(f'Mean {variable_description}')
ax2.set_ylim(0, merged_data[variable_shortcut].max() + 5)  # Adjust the y-limit for better visualization

# Plot the selected variable mean values over the years on the second axis
ax2.plot(mean_reg_alt_m_jjas['REG_year'], mean_reg_alt_m_jjas[variable_shortcut], 
         label=f'Mean {variable_description}', color='red', linestyle='-', linewidth=2)

# Add legends for both axes
fig.legend(loc='upper left', bbox_to_anchor=(0.15, 0.95))

plt.show()

############################################################
############################################################
# %%



# General information about the dataset
print(climate_data.info())

# Plotting distribution of numeric variables
climate_data.hist(figsize=(15, 10), bins=20)
plt.suptitle('Distribution of Climate Variables')
plt.show()

# Pairplot to understand relationships between variables
sns.pairplot(climate_data.sample(500), diag_kind='kde', plot_kws={'alpha': 0.5})
plt.suptitle('Pairplot of Sampled Climate Data', y=1.02)
plt.show()

# Correlation heatmap for the climate dataset
plt.figure(figsize=(15, 10))
sns.heatmap(climate_data.corr(), annot=True, cmap='coolwarm', fmt='.2f')
plt.title('Correlation Heatmap of Climate Data')
plt.show()

# Time series plot for a few important variables
plt.figure(figsize=(12, 6))
for variable in ['MAT', 'HMT', 'LMT', 'ATD', 'WQ', 'CQ', 'M-JJAS',
       'JJAS-TD',]:  
    if variable in climate_data.columns:
        plt.plot(climate_data['year'], climate_data[variable], label=variable)
plt.xlabel('Year')
plt.ylabel('Value')
plt.title('Climate Variables Over Time')
plt.legend()
plt.show()


# Time series plot for a few important variables
plt.figure(figsize=(12, 6))
variables = ['MAT', 'HMT', 'LMT', 'ATD', 'WQ', 'CQ', 'M-JJAS',
       'JJAS-TD', 'APS', 'HMP', 'LMP', 'APD', 'PSWQ', 'PSDQ', 'PS-JJAS',
       'JJAS-PD', 'MT0', 'MT5', 'MT5.5', 'MT6', 'MT6.5', 'MT7', 'MT7.5',
       'MT10']
for variable in variables:
    if variable in climate_data.columns:
        plt.figure(figsize=(12, 6))
        plt.plot(climate_data['year'], climate_data[variable], label=variable)
        plt.xlabel('Year')
        plt.ylabel('Value')
        plt.title(f'{variable} Over Time')
        plt.legend()
        plt.show()
# %%
