#%%
import pandas as pd

# Load the data from the uploaded file
# Define the home folder path
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\'
wide_df = pd.read_excel(home_folder +'input\\' + 'Dataset_V6.xlsx', sheet_name='Data_values_TOP_REP_only')

# Define reference columns and REG groups columns
reference_columns = ['Nr', 'ID', 'Forest_Tree', 'Mun_name', 
                     'County', 'Mount_alt', 'REG1_Aspect']
reg_columns_groups = [
    ['REG1_name', 'REG1_loc_name', 'REG1_alt', 'REG1_year'],
    ['REG2_name', 'REG2_loc_name', 'REG2_alt', 'REG2_year'],
    ['REG3_name', 'REG3_loc_name', 'REG3_alt', 'REG3_year']
]

# Create an empty list to store the dataframes for each REG group
long_format_list = []

# Iterate over each REG group and create a new dataframe by retaining reference columns
for reg_group in reg_columns_groups[:2]:
    reg_df = wide_df[reference_columns + reg_group].copy()
    
    # Drop rows where all REG group values are NaN
    reg_df = reg_df.dropna(subset=reg_group, how='all')
    
    # Rename REG columns to have common names for easier stacking
    reg_df = reg_df.rename(columns={
        reg_group[0]: 'REG_name',
        reg_group[1]: 'REG_loc_name',
        reg_group[2]: 'REG_alt',
        reg_group[3]: 'REG_year'
    })
    
    # Add the REG group identifier
    reg_df['REG_Group'] = reg_group[0][:4]  # e.g., 'REG1', 'REG2'
    
    # Append the transformed dataframe to the list
    long_format_list.append(reg_df)

# Define columns to be added to the REG3 group
additional_columns = [
    'REG3_alt_bar', 'REG3_alt_map', 'Veg_type', 'Veg_symb', 
    'Tree_height', 'Condition', 'Age', 'Photo', 'REG3_year', 'GPS_zone',
    'Easting', 'Northing', 'GPS_uncertainty', 'Lat', 'Long', 'Dist_SDF'
]
#%%
# Create a new dataframe for REG3 group with all additional columns included
reg3_df_extended = wide_df[reference_columns + reg_columns_groups[2] + additional_columns].copy()

# Rename REG3 columns to have common names for easier stacking
reg3_df_extended = reg3_df_extended.rename(columns={
    'REG3_name': 'REG_name',
    'REG3_loc_name': 'REG_loc_name',
    'REG3_alt': 'REG_alt',
    'REG3_year': 'REG_year'
})

# Add the REG group identifier
reg3_df_extended['REG_Group'] = 'REG3'

# To handle duplicate columns, ensure that we drop any duplicate columns in the REG3 extended dataframe
reg3_df_extended = reg3_df_extended.loc[:, ~reg3_df_extended.columns.duplicated()]

# Concatenate the REG1, REG2, and extended REG3 dataframes
final_long_format_df = pd.concat(long_format_list + [reg3_df_extended], ignore_index=True)
#%%
final_long_format_df.to_csv(home_folder + 'output\\'+ 'Treeline_long_V5.csv', index=False)
# %%
