from sklearn.cluster import KMeans
import pandas as pd

# Define the home folder path
home_folder = 'c:\\Users\\peterhor\\Documents\\GitHub\\Treeline_100year_study\\'


tfl = pd.read_excel(home_folder + 'Treeline_V3.xlsx', sheet_name=0)

# Extracting the latitude and longitude columns for clustering
coordinates = tfl[['Lat', 'Long']].dropna()


# Applying KMeans clustering to split the tfl into 4 geographical regions
kmeans = KMeans(n_clusters=4, random_state=42)
tfl['Region'] = kmeans.fit_predict(coordinates)

#%%
tfl.to_excel(home_folder + 'Treeline_V3_reg.xlsx', index=False)
#%%

#%%
import matplotlib.pyplot as plt
import geopandas as gpd
from shapely.geometry import Point
from mpl_toolkits.basemap import Basemap

# Setting up the figure
plt.figure(figsize=(12, 8))

# Creating a Basemap instance focused on the region of interest
m = Basemap(projection='merc', 
            llcrnrlat=tfl['Lat'].min() - 2, urcrnrlat=tfl['Lat'].max() + 2,
            llcrnrlon=tfl['Long'].min() - 2, urcrnrlon=tfl['Long'].max() + 2,
            resolution='i')

# Draw map details
m.drawcoastlines(linewidth=0.25)
m.drawcountries(linewidth=0.25)
m.drawmapboundary(fill_color='aqua')
m.fillcontinents(color='lightgrey', lake_color='aqua')

# Converting latitude and longitude to map coordinates
x, y = m(tfl['Long'].values, tfl['Lat'].values)

# Defining a discrete colormap for 4 regions
from matplotlib.colors import ListedColormap

discrete_cmap = ListedColormap(['red', 'blue', 'green', 'orange'])

# Plotting the points colored by region with discrete colors
sc = m.scatter(x, y, c=tfl['Region'], cmap=discrete_cmap, marker='o', alpha=0.7)

# Adding a colorbar with discrete ticks
cbar = plt.colorbar(sc, ticks=[0, 1, 2, 3])
cbar.ax.set_yticklabels(['Region 1', 'Region 2', 'Region 3', 'Region 4'])
plt.title('Geographical Clustering of Points by Region')
plt.show()

# %%
