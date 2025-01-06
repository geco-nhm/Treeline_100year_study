#%%
import pandas as pd
import os
# Define the home folder path
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\'
#%%
# Iterate through all climate data text files in the directory
climate_files = [f for f in os.listdir(home_folder +'input\\' + 'climate_data_csv\\') if f.endswith('.csv')]
print(climate_files)
# Initialize an empty list to store climate dataframes
climate_data_list = []
#%%
# Load each climate data file and append to the list
for file in climate_files:
    try:
        # Load the data and use the first column as the site name
        climate_data = pd.read_csv(home_folder +'input\\'+ 'climate_data_csv\\' + file, delim_whitespace=True, encoding='utf-8')
        site_name = climate_data.columns[0]  # Save the first column name as the site name
        climate_data.rename(columns={site_name: 'year'}, inplace=True)  # Rename the first column to 'year'
        climate_data.insert(0, 'site_name', site_name)  # Insert site name as the first column with column name 'site_name'
        climate_data_list.append(climate_data)
    except UnicodeDecodeError:
        print(f"Error reading file: {file}. Skipping this file.")

# Concatenate all climate data into a single dataframe vertically (new tables go beneath each other)
climate_data = pd.concat(climate_data_list, axis=0, ignore_index=True)
#%%
# Save the concatenated climate data to disk
climate_data.to_csv(home_folder + 'climate_data.csv', index=False)


# %%
