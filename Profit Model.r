# Databricks notebook source
# MAGIC %md
# MAGIC
# MAGIC #Profit Model
# MAGIC ###I created a linear model using data created by Sofia Hauck and the WFM to estimate the profit from SFI22 and SFI23 associated with various farm level characteristics.
# MAGIC
# MAGIC

# COMMAND ----------

# install relevant packages
library('dplyr')
library('tidyr')
library('ggplot2')
library('scales')
library("magrittr")
install.packages('janitor')
install.packages("rattle.data")
install.packages("nnet")
library("nnet")
install.packages("caret")
library("caret")
 

# COMMAND ----------

#loading in the data

wfms_mod <- read.csv('/dbfs/mnt/lab/unrestricted/elm/uptake/wfms_sfi.csv')
hedgerow <- read.csv('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-03.csv')
sfi23 <- read.csv("/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-13.csv")


# COMMAND ----------

# group by business ID and sum across relevant columns to create a new data frame hedgerow business
hedgerow_business <- hedgerow %>%
  group_by(id_business) %>% 
  summarise(across(c(ha_total_eligible_sfi_hedgerows_manage, payment_total_sfi_hedgerows_manage, profit_sfi_hedgerows_manage, profit_perha_sfi_hedgerows_manage, ha_total_eligible_sfi_hedgerows_assess, payment_total_sfi_hedgerows_assess, profit_sfi_hedgerows_assess, profit_perha_sfi_hedgerows_assess, ha_total_eligible_sfi_hedgerows_trees, payment_total_sfi_hedgerows_trees, profit_sfi_hedgerows_trees, profit_perha_sfi_hedgerows_trees), sum),  .groups = 'keep', na.rm = TRUE)


# COMMAND ----------

# Filter out the regions with zero
wfms_mod <- wfms_mod %>% 
filter(region != 0) %>%
left_join(sfi23,by="id_business")


# COMMAND ----------

display(wfms_mod)

# COMMAND ----------

# Split the data into training and test set

set.seed(2345)
sample <- wfms_mod$total_profit %>%
  createDataPartition(p = 0.7, list = FALSE)
train_data  <- wfms_mod[sample, ]
test_data <- wfms_mod[-sample, ]

# COMMAND ----------

#testing the linear model with different varibles to find the best R squared

lm_profit0 <- lm(total_profit ~ 
                + aes 
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type
                + region 
                + gm_per_ha
                , data = train_data)

summary(lm_profit0)


# COMMAND ----------

# Make predictions

prediction0 <- lm_profit0 %>% predict(test_data)

# Model performance

print(RMSE(prediction0, test_data$total_sfi_profit))

print(R2(prediction0, test_data$total_sfi_profit))

# COMMAND ----------

# this is test 2 using different variables:

lm_profit1 <- lm(total_profit ~ 
                + aes
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type
                + gm_per_ha
                , data = wfms_mod)

summary(lm_profit1)



# COMMAND ----------

# Make predictions
prediction1 <- lm_profit1 %>% predict(test_data)
# Model performance

print(RMSE(prediction1, test_data$total_sfi_profit)) 

print(R2(prediction1, test_data$total_sfi_profit))


################# THIS ONE #######################

# COMMAND ----------

# saving the model as a model object

saveRDS(lm_profit, file = "/dbfs/mnt/lab/unrestricted/elm/uptake/lm_profit1.rds")

model <- readRDS(file = "/dbfs/mnt/lab/unrestricted/elm/uptake/lm_profit1.rds")
