#%%
import pandas as pd
import difflib

# Define the home folder path
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\'

# Load the data from the CSV files
treeline_df = pd.read_csv(home_folder + 'Treeline_long_V3.csv')
climate_df = pd.read_csv(home_folder + 'climate_data.csv')

# Extract the columns we are interested in
columns_of_interest = ['site_name', 'year', 'MAT', 'HMT', 'LMT', 'ATD', 'WQ', 'CQ', 'M-JJAS', 'JJAS-TD',
                       'APS', 'HMP', 'LMP', 'APD', 'PSWQ', 'PSDQ', 'PS-JJAS', 'JJAS-PD', 'MT0', 'MT5', 
                       'MT5.5', 'MT6', 'MT6.5', 'MT7', 'MT7.5', 'MT10']

# Extract only the relevant columns from the climate data
climate_df = climate_df[columns_of_interest]

# Define a function to find approximate matches between two strings
def find_close_match(target, possibilities, threshold=0.8):
    closest_matches = difflib.get_close_matches(target, possibilities, n=1, cutoff=threshold)
    return closest_matches[0] if closest_matches else None

# Get a list of all unique 'site_name' values in climate_df for matching
unique_site_names = climate_df['site_name'].unique()

# Initialize a list to collect the matched data
matched_data = []

# Initialize a list to collect unmatched IDs
unmatched_ids = []

# Iterate over each row in treeline_df
for _, row in treeline_df.iterrows():
    current_id = row['ID']
    reg_year = row['REG_year']
    
    # Find records in climate_df with matching 'site_name' and 'year'
    matched_rows = climate_df[(climate_df['site_name'] == current_id) & (climate_df['year'] == reg_year)]
    
    if not matched_rows.empty:
        # Extract the 20 years prior to the matched year and calculate the average for each column
        previous_20_years = climate_df[(climate_df['site_name'] == current_id) & (climate_df['year'] < reg_year) & (climate_df['year'] >= reg_year - 20)]
        if not previous_20_years.empty:
            average_values = previous_20_years.mean(numeric_only=True)
            average_values['site_name'] = current_id
            average_values['year'] = reg_year
            average_values['ID'] = current_id  # Add the ID column
            matched_data.append(average_values)
        else:
            # If no data for the previous 20 years, append the matched rows as is
            matched_rows['ID'] = current_id  # Add the ID column
            matched_data.append(matched_rows)
    else:
        # If no match is found, add the current ID to unmatched_ids
        unmatched_ids.append(current_id)

# Concatenate all matched rows into a DataFrame
matched_data_df = pd.DataFrame(matched_data) if matched_data else pd.DataFrame(columns=columns_of_interest + ['ID'])

# Attempt to find approximate matches for unmatched IDs
newly_matched_data = []

# Iterate over unmatched IDs and attempt to find approximate matches
for unmatched_id in unmatched_ids:
    # Try to find a close match in the site names from the climate dataset
    close_match = find_close_match(unmatched_id, unique_site_names)
    
    if close_match:
        # Find records in climate_df with the close match and respective year
        reg_year = treeline_df[treeline_df['ID'] == unmatched_id]['REG_year'].values[0]
        matched_rows = climate_df[(climate_df['site_name'] == close_match) & (climate_df['year'] == reg_year)]
        
        if not matched_rows.empty:
            # Extract the 20 years prior to the matched year and calculate the average for each column
            previous_20_years = climate_df[(climate_df['site_name'] == close_match) & (climate_df['year'] < reg_year) & (climate_df['year'] >= reg_year - 20)]
            if not previous_20_years.empty:
                average_values = previous_20_years.mean(numeric_only=True)
                average_values['site_name'] = close_match
                average_values['year'] = reg_year
                average_values['ID'] = unmatched_id  # Add the ID column
                newly_matched_data.append(average_values)
            else:
                # If no data for the previous 20 years, append the matched rows as is
                matched_rows['ID'] = unmatched_id  # Add the ID column
                newly_matched_data.append(matched_rows)

# Concatenate newly matched rows into a DataFrame
newly_matched_data_df = pd.DataFrame(newly_matched_data) if newly_matched_data else pd.DataFrame(columns=columns_of_interest + ['ID'])

# Combine the original matched data with the newly matched data
final_matched_data_df = pd.concat([matched_data_df, newly_matched_data_df], ignore_index=True)

# Reorder columns to have 'site_name' as the first column
columns_order = ['site_name', 'ID', 'year'] + [col for col in columns_of_interest if col not in ['site_name', 'year']]
final_matched_data_df = final_matched_data_df[columns_order]


# Save the updated matched data to a new CSV file
final_matched_data_df.to_csv(home_folder + 'final_matched_climate_data.csv', index=False)

# Save and display an overview of unmatched entries
unmatched_ids_df = pd.DataFrame(unmatched_ids, columns=['unmatched_IDs'])
unmatched_ids_df.to_csv(home_folder + 'unmatched_ids_overview.csv', index=False)
# %%
