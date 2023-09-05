# Databricks notebook source
# MAGIC %md
# MAGIC
# MAGIC ##Total area with eligible for no till from the whole farm model at parcel level
# MAGIC
# MAGIC - Area used is arable area only
# MAGIC - Parcels with the following crops in rotation are not eligible for till include
# MAGIC   - Potatoes
# MAGIC   - Peas
# MAGIC   - Maize
# MAGIC   - Sugar Beets
# MAGIC   
# MAGIC

# COMMAND ----------

# importing relevant libraries

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# grabbing the dataset
path = "/dbfs/mnt/lab/unrestricted/elm/wfm/2023_06_09/wfm_parcels.feather"

df = pd.read_feather(path)

df

# COMMAND ----------

# getting the arable columns to create total hectarage
 
# relavent cols 
arable_cols = ['ha_fallow', 'ha_field_beans', 'ha_fodder_maize', 'ha_grain_maize', 'ha_other_crop', 'ha_peas', 'ha_potatoes', 'ha_rapeseed_oil', 'ha_spring_barley', 'ha_spring_oats', 'ha_spring_wheat', 'ha_sugar_beet', 'ha_temporary_pasture', 'ha_winter_barley', 'ha_winter_oats', 'ha_winter_wheat']

# summing up total arable hectares
df["ha_arable"] = df[arable_cols].sum(axis=1)

# what counts as no till crops
not_eligible_for_no_till = ["ha_grain_maize",'ha_fodder_maize',"ha_potatoes","ha_sugar_beet","ha_peas","ha_other_crop" ]

# adding in 5 new columns
for crop in not_eligible_for_no_till:
    df['{}_pct_of_arable'.format(crop)] = 100 * df[crop] / df["ha_arable"]



# COMMAND ----------

# adding bool columns if not_eligible_for_no_till column percentages are above 1,2,3,4 or 5 percent
for i in range (1,11):
     df['till_above{}'.format(i)] = (df['ha_grain_maize_pct_of_arable'] >= i) | (df['ha_fodder_maize_pct_of_arable'] >= i) | (df['ha_potatoes_pct_of_arable'] >= i) | (df['ha_sugar_beet_pct_of_arable'] >= i) | (df['ha_peas_pct_of_arable'] >= i) | (df['ha_other_crop_pct_of_arable'] >= i)

# df["till"] = (df["ha_grain_maize"] > 0) | (df['ha_fodder_maize'] > 0) | (df["ha_potatoes"] > 0)  | (df["ha_sugar_beet"] > 0) | (df["ha_peas"] > 0) | (df["ha_other_crop"] > 0)

# COMMAND ----------

df[["ha_grain_maize",'ha_fodder_maize',"ha_potatoes","ha_sugar_beet","ha_peas","ha_other_crop", "ha_grain_maize_pct_of_arable",'ha_fodder_maize_pct_of_arable',"ha_potatoes_pct_of_arable","ha_sugar_beet_pct_of_arable","ha_peas_pct_of_arable","ha_other_crop_pct_of_arable","ha_arable", "till_above1",  "till_above2",  "till_above3",  "till_above4", "till_above5"]]

# COMMAND ----------

# choosing data that only has +ve arable land
arable_data = df[df["ha_arable"] > 0]

# count trues/falses of each percentage cap column
arable_data_counts = arable_data[["till_above1",  "till_above2",  "till_above3",  "till_above4", "till_above5", "till_above6", "till_above7", "till_above8", "till_above9", "till_above10"]].apply(pd.value_counts) 

# COMMAND ----------

arable_data_counts
# create a table of percentages

true_percentages = []
false_percentages = []

for column in arable_data_counts.columns:
    true_count = arable_data_counts[column].loc[True]
    total_count = arable_data_counts[column].sum()
    false_count = arable_data_counts[column].loc[False]
    true_percentage = true_count / total_count * 100
    false_percentage = false_count / total_count * 100
    true_percentages.append(true_percentage)
    false_percentages.append(false_percentage)

df_percentages = pd.DataFrame({'True': true_percentages, 'False': false_percentages}, index = arable_data_counts.columns)

df_percentages

# COMMAND ----------

# Create a plot showing which area is eligible for till and no till above certain percentages. 
df_percentages.plot.bar()

# COMMAND ----------


