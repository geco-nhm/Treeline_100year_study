# %%
import pandas as pd 
import os
import re

# Load the mapping CSV
csv_file_path = r"c:\Users\peterhor\Documents\GitHub\Treeline_100year_study\input\climate_data_overview\sites_mapping.csv"
mapping_df = pd.read_csv(csv_file_path, header=0)  # Read the CSV with the first line as column names

# Ensure the ID column is properly stripped of whitespace
mapping_df['ID'] = mapping_df['ID'].astype(str).str.strip()

# Define the folder containing the text files
folder_path = r"c:\Users\peterhor\Documents\GitHub\Treeline_100year_study\input\climate_data"
output_folder = r"c:\Users\peterhor\Documents\GitHub\Treeline_100year_study\input\climate_data_csv"

# Ensure the output folder exists
os.makedirs(output_folder, exist_ok=True)

# Natural sorting function
def natural_sort_key(s):
    return [int(text) if text.isdigit() else text.lower() for text in re.split(r'(\d+)', s)]
#%%
# Process files in natural order
for _, row in mapping_df.iterrows():
    file_id = str(row['ID'])  # Get the ID from the current row
    replacement = row.iloc[2]  # Assuming the third column contains the replacement string

    file_name = f"{file_id}.txt"
    file_path = os.path.join(folder_path, file_name)

    if os.path.exists(file_path):
        with open(file_path, "r", encoding="ISO-8859-1") as file:
            lines = file.readlines()

        # Debug: Print the first line before modification
        if lines:
            print(f"Original first line in {file_name}: {lines[0].strip()}")
            first_line_parts = lines[0].split(maxsplit=1)
            if len(first_line_parts) > 1:
                lines[0] = replacement + '\t' + first_line_parts[1]  # Keep the remaining parts of the first line
            else:
                lines[0] = replacement + '\n'  # If there's only one string, replace it entirely

            # Debug: Print the modified first line
            print(f"Modified first line in {file_name}: {lines[0].strip()}")

        # Save the updated content as a CSV in the output folder
        output_file_path = os.path.join(output_folder, file_name.replace(".txt", ".csv"))
        with open(output_file_path, "w", encoding="utf-8") as file:
            file.writelines(lines)
