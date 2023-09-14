# Databricks notebook source
# MAGIC %md
# MAGIC #### Take the following code with a pinch of salt as I am very new to geopandas and python

# COMMAND ----------

# Importing relevant packages

import pandas as pd, numpy as np, matplotlib.pyplot as plt
from geopandas import GeoSeries, GeoDataFrame
import numpy as np
import pandas as pd
import geopandas as gpd
import scipy.stats as sts  
from matplotlib.pyplot import figure
import matplotlib.pyplot as plt
import matplotlib.colors as clrs
#from ripley_k_function import ripley_K 
from pyspark.sql import functions as F, types as T
import seaborn as sns


# COMMAND ----------

# Reading in and joining datasets

path_sfi = "/dbfs/mnt/lab/unrestricted/elm_data/rural_payments_agency/sfi_agreements/20230315.csv"
path_geo = "dbfs:/mnt/lab/unrestricted/elm_data/rpa/reference_parcels/2023_02_07.parquet"
df = pd.read_csv(path_sfi)
gdf = spark.read.parquet(path_geo)

# COMMAND ----------

#etl of the sfi agreements to have agreement/offering/parcel_id as columns
df = pd.read_csv(path_sfi).dropna(subset=["PARCEL_NUMBER"])

drop_cols = [
    "NUTS2",
    "APP_ID",
    "APP_START_DATE",
    "APP_END_DATE",
    "CLAIMED_AREA",
    "CLAIMED_AMB",
    "VERIFIED_AREA",
    "VERIFIED_AMB",
    "ANNUAL_VALUE_OF_OPTION",
    "AGREEMENT_END",
    "STATUS",
]

df = (
    df.rename({"PARCEL_NUMBER": "id_parcel"}, axis=1)
    .assign(uptake=True)
    .assign(offering=lambda df: df.CLAIMED_AMB)
    .drop(columns=drop_cols)
    .rename({"AGREEMENT_START": "AGREEMENT_START".lower()}, axis=1)
)
print(df.offering.value_counts())
df

# COMMAND ----------

#change gdf to concatenate sheet id with parcel id so it matches with the sfi data 
gdf = gdf.withColumn('id_parcel', F.concat('SHEET_ID','PARCEL_ID'))
(gdf)

# COMMAND ----------

# turning gdf into a pandas geo dataframe (code from heather)
gdf = (gdf
  .withColumn('geometry', F.expr('(wkb_geometry)'))
  .toPandas()
  .pipe(lambda pdf:  gpd.GeoDataFrame({
    'id_parcel':  pdf['id_parcel'],
    'geometry': gpd.GeoSeries.from_wkb(pdf['wkb_geometry'], crs=27700),
  }, crs=27700, geometry='geometry'))
)



# COMMAND ----------

# merging gdf and df
gdf_agr = pd.merge(gdf, df, how="outer", on=['id_parcel'])


# COMMAND ----------

## Ripley's K gained importance within spatial point pattern analysis for being a distance-based measure which indicates spatial clustering or dispersion. The user can identify a pattern's spatial tendency by graphically comparing its K function to the K evaluation of complete spatial randomness (CSR) patterns. Within the realm of subsurface modeling, Ripley's K can indicate the spatial distribution of samples with desired properties, such as porosity and permeability, from a known sample.

# COMMAND ----------

# taking a small sample so everything runs a bit quicker
gdf1 = gdf_agr.sample(n=5)

# COMMAND ----------

display(gdf1)

# COMMAND ----------

# making a copy and finding centroids of polygons and adding a new column of T/F for uptake of SFI
gdf2 = gdf1.copy()

gdf2["x"] = gdf1.centroid.map(lambda p: p.x)
gdf2["y"] = gdf1.centroid.map(lambda p: p.y)

gdf2["uptake"].fillna(False, inplace = True)

# COMMAND ----------

display(gdf2)

# COMMAND ----------

# plotting the centroids - increase sample size to see a bit more of england
X = gdf2.iloc[:,5]; Y = gdf2.iloc[:,6]
plt.style.use('ggplot')
figure = plt.figure(figsize=(14,10))
sns.scatterplot(x = "x",y = "y", hue='uptake', data=gdf2)
plt.title('Scatter Plot of Parcel Centroid Locations',size=30); plt.xlabel('X (m)',size=20); plt.ylabel('Y (m)',size=20);

# COMMAND ----------

# to find the area of study we must fine the min and max  area

x_min = 0; x_max = gdf2["x"].max()
y_min = 0; y_max = gdf2["y"].max()
area = (x_max - x_min)*(y_max - y_min)

# COMMAND ----------

#The next step is to determine the size and number of search circles which will be drawn around each event 
# Also, because the dimensions of range we take 50 circles about each event to get enough detail but not overwhelm our calculation. Hence:

radii = np.linspace(0,x_max-x_min,50).reshape(50,1) 


# COMMAND ----------

# a new df of just the location of centroids

gdf3 = gdf2[["x","y"]]

# COMMAND ----------

# First all events are treated equally, with no distinction due to property values. 

# let us compute all possible distances within and break those down into and displacements since we are using Cartesian coordinates:

npts = np.shape(gdf3)[0]                                 # Number of events in A

diff = np.zeros(shape = (npts*(npts-1)//2,2))               # Decomposed distances matrix
k = 0
for i in range(npts - 1):
    size = npts - i - 1
    diff[k:k + size] = abs(gdf3.iloc[i] - gdf3.iloc[i+1:])
    k += size

print(diff)

# COMMAND ----------

# compute the distances from the pairwise differences (as simple as applying the Pythagorean Theorem to every row of the diff matrix) and determine whether these distances are less than a given radius

n_ripley = np.zeros(len(radii))
distances = np.hypot(diff[:,0], diff[:,1])                      # Pythagorean Theorem (a^2+b^2=c^2)

for r in range(len(radii)):
    n_ripley[r] = (distances<radii[r]).sum()                    # Indicator function and summation term

n_ripley = area * 2. * n_ripley / (npts * (npts - 1))           # Expectation vector element-wise divided by scalar intensity

print(n_ripley)

# COMMAND ----------

# distance between parcels - nested for loop
# clustering of farms
# message ed
# plot only the sfi parcels 
# choose a spot on the country and plot
# join farm business id
#  
