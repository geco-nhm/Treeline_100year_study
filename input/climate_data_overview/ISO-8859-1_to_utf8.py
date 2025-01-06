#%%
import json
import os
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\input\\climate_data\\'
with open('c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\input\\climate_data_overview\\site_id_mapping.json', 'r') as file:
    sites_to_matched_dict = json.load(file)
#%%    
    # Function to update SITE_ID in a file
def update_site_id(file_path, mapping_dict):
    with open(file_path, 'r', encoding='ISO-8859-1') as file:
        lines = file.readlines()

    # Replace SITE_ID on the first line if it matches a key in the dictionary
    site_id = lines[0].strip()
    if site_id in mapping_dict:
        lines[0] = mapping_dict[site_id] + '\n'

    # Write the updated lines back to the file
    with open(file_path, 'w', encoding='utf-8') as file:
        file.writelines(lines)

# Iterate through each file in the directory and update SITE_ID
for filename in os.listdir(home_folder):
    file_path = os.path.join(home_folder, filename)
    if os.path.isfile(file_path):  # Ensure it's a file
        update_site_id(file_path, sites_to_matched_dict)

print("SITE_IDs updated successfully!")
# %%
