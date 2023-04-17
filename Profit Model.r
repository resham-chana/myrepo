# Databricks notebook source
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

#%python
#import pandas as pd
#
#hedgerow = pd.read_parquet('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-03.parquet')
#hedgerow.to_csv('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-03.csv')


# COMMAND ----------

#%python
#
#dbutils.fs.mv('dbfs:/FileStore/total_ELM_profit_no_negatives_James.csv',
#             'dbfs:/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-13.csv')

# COMMAND ----------

#%python
#import pandas as pd

#sfi23 = pd.read_parquet('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-04.parquet')


#sfi23.to_csv('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-04.csv')

# COMMAND ----------

wfms_mod <- read.csv('/dbfs/mnt/lab/unrestricted/elm/uptake/wfms_sfi.csv')
hedgerow <- read.csv('/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-03.csv')
sfi23 <- read.csv("/dbfs/mnt/lab/unrestricted/elm/uptake/elmo_features/elmo_sfi23_profit/2023-04-13.csv")


# COMMAND ----------

names(sfi23)

# COMMAND ----------

names(hedgerow)

# COMMAND ----------

hedgerow_business <- hedgerow %>%
  group_by(id_business) %>% 
  summarise(across(c(ha_total_eligible_sfi_hedgerows_manage, payment_total_sfi_hedgerows_manage, profit_sfi_hedgerows_manage, profit_perha_sfi_hedgerows_manage, ha_total_eligible_sfi_hedgerows_assess, payment_total_sfi_hedgerows_assess, profit_sfi_hedgerows_assess, profit_perha_sfi_hedgerows_assess, ha_total_eligible_sfi_hedgerows_trees, payment_total_sfi_hedgerows_trees, profit_sfi_hedgerows_trees, profit_perha_sfi_hedgerows_trees), sum),  .groups = 'keep', na.rm = TRUE)


# COMMAND ----------

names(sfi23)

# COMMAND ----------

wfms_mod <- wfms_mod %>% 
filter(region != 0) %>%
left_join(sfi23,by="id_business")



# COMMAND ----------

display(wfms_mod)

# COMMAND ----------

###################

# PREDICTS SFI 22/23 PROFIT 
# DO IT AT THE BUSINESS LEVEL
# FORMULA OF TOTAL SFI PROFIT: ADD HEDGEROWS TO TOTAL SFI
# 5% SFI 2023 + HEDGROWS + SFI 22
# 

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

min(test_data$total_profit)

# COMMAND ----------

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

lm_profit1 <- lm(total_profit ~ 
                + aes
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type
                + gm_per_ha
                , data = wfms_mod)

summary(lm_profit1)

################# THIS ONE #######################

# COMMAND ----------

# Make predictions
prediction1 <- lm_profit1 %>% predict(test_data)
# Model performance

print(RMSE(prediction1, test_data$total_sfi_profit)) 

print(R2(prediction1, test_data$total_sfi_profit))


################# THIS ONE #######################

# COMMAND ----------

# train the model on the entire data set

lm_profit <- lm(total_sfi_profit ~ 
                + aes 
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type 
                + aonb 
                + gm_per_ha
                , data = wfms_mod)

summary(lm_profit)

# COMMAND ----------

names(model)

# COMMAND ----------

(model$coefficients)

# COMMAND ----------

saveRDS(lm_profit, file = "/dbfs/mnt/lab/unrestricted/elm/uptake/lm_profit1.rds")

model <- readRDS(file = "/dbfs/mnt/lab/unrestricted/elm/uptake/lm_profit1.rds")

# COMMAND ----------

lm_profit2 <- lm(total_sfi_profit ~ 
                any_sfi 
                + aes 
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type 
                + aonb 
                + gm_per_ha
                + I(total_gm^2) 
                + I(bps_eligible_area^2)
                + I(livestock_lu^2)
                + I(gm_per_ha^2)
                , data = train_data)

summary(lm_profit2)


# COMMAND ----------

# Make predictions
prediction2 <- lm_profit2 %>% predict(test_data)

# Model performance

print(RMSE(prediction2, test_data$total_sfi_profit)) 

print(R2(prediction2, test_data$total_sfi_profit))


# COMMAND ----------

lm_profit3 <- lm(total_sfi_profit ~ 
                + total_gm 
                + bps_eligible_area
                + livestock_lu
                + farm_type 
                + I(total_gm^2) 
                + I(bps_eligible_area^2)
                + I(livestock_lu^2)
                , data = train_data)

summary(lm_profit3)


# COMMAND ----------

# Make predictions
prediction3 <- lm_profit3 %>% predict(test_data)

# Model performance

print(RMSE(prediction3, test_data$total_sfi_profit)) 

print(R2(prediction3, test_data$total_sfi_profit))


# COMMAND ----------

prediction3

# COMMAND ----------


ggplot(data = data.frame(predictions = prediction1, actuals = test_data$total_sfi_profit), aes(x = actuals, y = predictions)) +
  geom_point() +
  ggtitle(paste0("Predicted vs Actual Values for lm2")) +
  xlab("Actual Values") +
  ylab("Predicted Values")

# COMMAND ----------



# COMMAND ----------


ggplot(data = data.frame(predictions = prediction3, actuals = test_data$total_sfi_profit), aes(x = actuals, y = predictions)) +
  geom_point() +
  ggtitle(paste0("Predicted vs Actual Values for lm3")) +
  xlab("Actual Values") +
  ylab("Predicted Values")


# COMMAND ----------

ggplot(data = data.frame(residuals = residuals(lm_profit3)), aes(x = residuals)) +
  geom_histogram() +
  ggtitle("Histogram of Residuals of lm3") +
  xlab("Residuals")

# COMMAND ----------

# total profit or total profit per ha
# how to combine profit 
# step 1: add hedgerow just as is 
# how to add legume 
# the model will be profit overall
# assume most profitable one first 
# 10th of april for the deadline 
# output will be a model object

