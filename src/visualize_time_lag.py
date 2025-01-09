#%%
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
#%%
# Load the matched climate data
file_path = r"C:\Users\peterhor\Documents\GitHub\Treeline_100year_study\output\matched_climate_data.csv"
matched_climate_data = pd.read_csv(file_path, encoding='utf-8')
#%%
# Display the first few rows and summary information about the dataset to understand its structure
matched_climate_data.head(), matched_climate_data.info()

#%% 
matched_climate_data['year'] = matched_climate_data['year'].astype(int)
#%%
matched_climate_data["ID"]

# %%
# Finding unique combinations of ID for two different years
unique_ids = matched_climate_data['ID'].unique()

# Selecting one example ID for two different years
example_id = unique_ids[0]
example_data = matched_climate_data[matched_climate_data['ID'] == example_id]

# Extracting data for two specific years
years = example_data['year'].unique()
if len(years) >= 2:
    year_1, year_2 = years[:2]
    example_years_data = example_data[example_data['year'].isin([year_1, year_2])]
else:
    print("Not enough years of data for the selected ID.")

# Visualizing the changes in MAT between the two years
plt.figure(figsize=(8, 6))
sns.barplot(data=example_years_data, x="year", y="MAT", palette="coolwarm")
plt.title(f"Changes in MAT for ID: {example_id} ({year_1} vs {year_2})", fontsize=16)
plt.xlabel("Year", fontsize=12)
plt.ylabel("Mean Annual Temperature (MAT)", fontsize=12)
plt.grid(axis="y")
plt.show()
# %%
# Selecting multiple example IDs
example_ids = matched_climate_data['ID'].unique()[:5]  # Taking the first 5 IDs as an example
example_data_multiple = matched_climate_data[matched_climate_data['ID'].isin(example_ids)]

# Ensuring that we have data for at least two years for each ID
valid_ids = example_data_multiple.groupby('ID')['year'].nunique()
valid_ids = valid_ids[valid_ids > 1].index
example_data_multiple = example_data_multiple[example_data_multiple['ID'].isin(valid_ids)]

# Visualizing changes in MAT across multiple sites and years
plt.figure(figsize=(12, 8))
sns.lineplot(data=example_data_multiple, x="year", y="MAT", hue="ID", marker="o")
plt.title("Changes in MAT Across Multiple Sites Over Years", fontsize=16)
plt.xlabel("Year", fontsize=12)
plt.ylabel("Mean Annual Temperature (MAT)", fontsize=12)
plt.legend(title="Site ID", bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True)
plt.tight_layout()
plt.show()
# %%

# Load the Treeline_long_V6 data
treeline_data_path = r"C:\Users\peterhor\Documents\GitHub\Treeline_100year_study\output\Treeline_long_V6.csv"
treeline_data = pd.read_csv(treeline_data_path, encoding='utf-8')

# Display the structure of the treeline dataset to understand the columns and check for lat/long or region information
treeline_data.head(), treeline_data.info()
# %%
# Merge the climate data with treeline data including 'Mun_name'
merged_data = pd.merge(
    matched_climate_data,
    treeline_data[['ID', 'County', 'Mun_name', 'REG_name', 'Lat', 'Long']],
    on='ID',
    how='inner'
)
#%%
# Group by region (County) to visualize temperature trends
plt.figure(figsize=(16, 10))
sns.lineplot(data=merged_data, x="year", y="MAT", hue="County", marker="o", palette="tab10")
plt.title("Temperature Trends (MAT) by Region (County)", fontsize=16)
plt.xlabel("Year", fontsize=12)
plt.ylabel("Mean Annual Temperature (MAT)", fontsize=12)
plt.legend(title="County", bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True)
plt.tight_layout()
plt.show()
#%%
# Group by region (Municipality) to visualize temperature trends
plt.figure(figsize=(16, 10))
sns.lineplot(data=merged_data, x="year", y="MAT", hue="Mun_name", marker="o", palette="tab20", alpha=0.7)
plt.title("Temperature Trends (MAT) by Municipality", fontsize=16)
plt.xlabel("Year", fontsize=12)
plt.ylabel("Mean Annual Temperature (MAT)", fontsize=12)
plt.legend(title="Municipality", bbox_to_anchor=(1.05, 1), loc='upper left', ncol=2)
plt.grid(True)
plt.tight_layout()
plt.show()
# %%
# Group by region (County) to visualize temperature trends
plt.figure(figsize=(16, 10))
sns.lineplot(data=merged_data, x="year", y="APS", hue="County", marker="o", palette="tab10")
plt.title("Precipitation Trends (APS) by Region (County)", fontsize=16)
plt.xlabel("Year", fontsize=12)
plt.ylabel("Annual Precipitation Sum (APS)", fontsize=12)
plt.legend(title="County", bbox_to_anchor=(1.05, 1), loc='upper left')
plt.grid(True)
plt.tight_layout()
plt.show()

# %%
