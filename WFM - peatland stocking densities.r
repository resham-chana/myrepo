# Databricks notebook source
# MAGIC %md 
# MAGIC ##Peatland Stocking Density
# MAGIC ###The following notebook is used to compare the livestock on land with AES (agri environment scheme) participation and those without. 

# COMMAND ----------

#attach necessary packages 
library('dplyr')
library('ggplot2')


# COMMAND ----------

#load in peatland/wfm data

wfm_peat <- read.csv('/dbfs/mnt/lab/unrestricted/DSMT/wfm_peat.csv')

# COMMAND ----------

display(wfm_peat) 

# COMMAND ----------

# calculate total livestock for each category
wfm_peat2 <- wfm_peat %>%
  dplyr::mutate(total_beef = rowSums(select(., starts_with("bf")), na.rm=T)) %>%
  dplyr::mutate(total_dairy = rowSums(select(., starts_with(c("dd", 'dx'))), na.rm=T)) %>% 
  dplyr::mutate(total_heads = rowSums(select(., starts_with(c("dd", 'dx','goats','sheep'))), na.rm=T)) %>%

# calculate stocking density (sd) using bps eligible area and heads
  dplyr::mutate(sd_beef_bps = if_else(total_beef == 0 | is.na(total_beef), 0, model_ha/total_beef)) %>%
  dplyr::mutate(sd_dairy_bps = if_else(total_dairy == 0 | is.na(total_dairy), 0, model_ha/total_dairy)) %>%
  dplyr::mutate(sd_sheep_bps = if_else(sheep == 0 | is.na(sheep), 0, model_ha/sheep)) %>%
  dplyr::mutate(sd_goats_bps = if_else(goats == 0 | is.na(goats), 0, model_ha/goats)) %>%
  dplyr::mutate(sd_heads_bps = if_else(total_heads == 0 | is.na(total_heads), 0, model_ha/total_heads)) %>%
  
# stocking density using grassland and heads
  dplyr::mutate(sd_beef_grass = if_else(total_beef == 0 | is.na(total_beef), 0, model_ha/total_beef)) %>%
  dplyr::mutate(sd_dairy_grass = if_else(total_dairy == 0 | is.na(total_dairy), 0, model_ha/total_dairy)) %>%
  dplyr::mutate(sd_sheep_grass = if_else(sheep == 0 | is.na(sheep), 0, model_ha/sheep)) %>%
  dplyr::mutate(sd_goats_grass = if_else(goats == 0 | is.na(goats), 0, model_ha/goats)) %>%
  dplyr::mutate(sd_heads_grass = if_else(total_heads == 0 | is.na(total_heads), 0, model_ha/total_heads)) %>%

# stocking density using livestock units bps/grassland
  dplyr::mutate(sd_bps_lu = if_else(livestock_lu == 0 | is.na(livestock_lu), 0, model_ha/livestock_lu)) %>% 
  dplyr::mutate(sd_grass_lu = if_else(livestock_lu == 0 | is.na(livestock_lu), 0, pp_ha/livestock_lu)) %>% 

# if agriorgarnic is above zero then assign it yes for an AES
  dplyr::mutate(agri_organic = as.character(if_else(is.na(agri_organic), 0,1))) %>%
# exclufe null livestock units and make them zero
  dplyr::mutate(livestock_lu = if_else(is.na(livestock_lu), 0, livestock_lu)) %>%

  dplyr::select('peat_desc','proportion', 'id_parcel', 'id_business', 'agri_organic','livestock_lu','model_ha','pp_ha', 'sd_beef_bps','sd_dairy_bps','sd_sheep_bps','sd_goats_bps','sd_heads_bps','sd_beef_grass','sd_dairy_grass','sd_sheep_grass','sd_goats_grass','sd_heads_grass','sd_bps_lu','sd_grass_lu', 'total_heads')

#group by business

wfm_peat_business <- wfm_peat2 %>%
  dplyr::group_by(id_business, agri_organic, livestock_lu, model_ha, pp_ha, sd_beef_bps,sd_dairy_bps,sd_sheep_bps,sd_goats_bps,sd_heads_bps,sd_beef_grass,sd_dairy_grass,sd_sheep_grass,sd_goats_grass,sd_heads_grass,sd_bps_lu,sd_grass_lu, total_heads) %>%
  dplyr::summarise(proportion = sum(proportion), na.rm = TRUE)

# take out businesses with zero livestock
wfm_livestock <- wfm_peat_business %>%
  dplyr::filter(total_heads != 0)

# COMMAND ----------

display(wfm_peat_business)


# COMMAND ----------

#calculating the mean livestock in LU (livestock units) by those with and with no AES 
mu_lu <- wfm_peat_business %>%
    dplyr::group_by(agri_organic) %>% 
    dplyr::summarise(mean_livestock = mean(livestock_lu, na.rm = TRUE)) %>%
    as.data.frame()

#calculating the mean livestock in heads (actual animals) by those with and with no AES 

mu_heads <- wfm_peat_business %>%
    dplyr::group_by(agri_organic) %>% 
    dplyr::summarise(mean_heads = mean(total_heads, na.rm = TRUE)) %>%
    as.data.frame()

display(mu_heads)

# COMMAND ----------

# plotting the stocking density against livestock LU 

ggplot(wfm_peat_business, aes(x=livestock_lu, color=agri_organic, fill=agri_organic)) +
geom_histogram(aes(y=..density..), position="dodge", alpha=0.7, bins = 20)+
geom_density(alpha=0.1)+
geom_vline(data=mu_lu, aes(xintercept=mean_livestock, color=agri_organic),
           linetype="dashed")+
xlim(0,400)+
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_discrete(labels = c("Participants", ' Non-participants'))+
scale_colour_discrete(labels = c("Participants", ' Non-participants'))+
  guides(fill = guide_legend(title = "Agri-environment and organic scheme participation"),
         colour = guide_legend(title = "Agri-environment and organic scheme participation"))+
labs(title="Livestock histogram",x="Livestock (LU)", y = "Density")+
theme(legend.position="top")

# COMMAND ----------

# plotting the stocking density against livestock LU 

ggplot(wfm_livestock, aes(x=total_heads, color=agri_organic, fill=agri_organic)) +
geom_histogram(aes(y=..density..), position="dodge", alpha=0.7, bins = 30)+
geom_density(alpha=0.1)+
geom_vline(data=mu_heads, aes(xintercept=mean_heads, color=agri_organic),
           linetype="dashed")+
xlim(0,300)+
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_discrete(labels = c("Participants", ' Not Participants'))+
scale_colour_discrete(labels = c("Participants", ' Not Participants'))+
  guides(fill = guide_legend(title = "Agri-environment and organic scheme participation"),
         colour = guide_legend(title = "Agri-environment and organic scheme participation"))+
labs(title="Livestock histogram",x="Livestock (Heads)", y = "Density")+
theme(legend.position="top")

# COMMAND ----------


# summary table for the mean/median/min and max stocking density for grassland area and BPS eligible area

summary_bps_lu <- wfm_peat_business %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_bps_lu, na.rm = TRUE),
            med = median(sd_bps_lu, na.rm = TRUE),
           min = min(sd_bps_lu, na.rm = TRUE),
           max = max(sd_bps_lu, na.rm = TRUE))
  
summary_grass_heads <- wfm_peat_business %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_heads_grass, na.rm = TRUE),
            med = median(sd_heads_grass, na.rm = TRUE),
           min = min(sd_heads_grass, na.rm = TRUE),
           max = max(sd_heads_grass, na.rm = TRUE))
  
summary_grass_lu <- wfm_peat_business %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_grass_lu, na.rm = TRUE),
            med = median(sd_grass_lu, na.rm = TRUE),
           min = min(sd_grass_lu, na.rm = TRUE),
           max = max(sd_grass_lu, na.rm = TRUE))
  
summary_heads_bps <- wfm_peat_business %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_heads_bps, na.rm = TRUE),
            med = median(sd_heads_bps, na.rm = TRUE),
           min = min(sd_heads_bps, na.rm = TRUE),
           max = max(sd_heads_bps, na.rm = TRUE))
  
  

# COMMAND ----------

display(summary_bps_lu)

# COMMAND ----------

display(summary_grass_heads)

# COMMAND ----------

display(summary_grass_lu)

# COMMAND ----------

display(summary_heads_bps)

# COMMAND ----------

p <- ggplot(wfm_peat_business, aes(agri_organic, log(sd_bps_lu), fill=agri_organic)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") 

p

# COMMAND ----------

p <- ggplot(wfm_peat_business, aes(agri_organic, log(sd_grass_lu), fill=agri_organic)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") 

p

# COMMAND ----------

p <- ggplot(wfm_peat_business, aes(agri_organic, log(sd_heads_bps), fill=agri_organic)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") 

p

# COMMAND ----------

p <- ggplot(wfm_peat_business, aes(agri_organic, log(sd_heads_grass), fill=agri_organic)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2") 

p

# COMMAND ----------

# MAGIC %md
# MAGIC #**Same code with no outliers in model hectres and livestock units**

# COMMAND ----------

# calculating the auantiles from the livestock (LU) column
quartiles <- quantile(wfm_peat_business$livestock_lu, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wfm_peat_business$livestock_lu)

# Calculating upper and lower quartiles 
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
 
wfm_no_lu_outlier <- subset(wfm_peat_business, wfm_peat_business$livestock_lu > Lower & wfm_peat_business$livestock_lu < Upper)


quartiles <- quantile(wfm_no_lu_outlier$model_ha, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(wfm_no_lu_outlier$model_ha)
 
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
 
wfm_no_outlier <- subset(wfm_no_lu_outlier, wfm_no_lu_outlier$model_ha > Lower & wfm_no_lu_outlier$model_ha < Upper)



# COMMAND ----------

# mean livestock with no outliers 
mu_lu_no_out <- wfm_no_outlier %>%
    dplyr::group_by(agri_organic) %>% 
    dplyr::summarise(mean_livestock = mean(livestock_lu, na.rm = TRUE)) %>%
    as.data.frame()


mu_heads_no_out <- wfm_no_outlier %>%
    dplyr::group_by(agri_organic) %>% 
    dplyr::summarise(mean_heads = mean(total_heads, na.rm = TRUE)) %>%
    as.data.frame()


# COMMAND ----------

# repeating the same plot as above with mo outlier 

ggplot(wfm_no_outlier, aes(x=livestock_lu, color=agri_organic, fill=agri_organic)) +
geom_histogram(aes(y=..density..), position="dodge", alpha=0.7, bins = 20)+
geom_density(alpha=0.1)+
geom_vline(data=mu_lu_no_out, aes(xintercept=mean_livestock, color=agri_organic),
           linetype="dashed")+
xlim(0,400)+
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_discrete(labels = c("Participants", ' Non-participants'))+
scale_colour_discrete(labels = c("Participants", ' Non-participants'))+
  guides(fill = guide_legend(title = "Agri-environment and organic scheme participation"),
         colour = guide_legend(title = "Agri-environment and organic scheme participation"))+
labs(title="Livestock histogram",x="Livestock (LU)", y = "Density")+
theme(legend.position="top")

# COMMAND ----------


# repeating the same plot as above with mo outlier 

ggplot(wfm_no_outlier, aes(x=total_heads, color=agri_organic, fill=agri_organic)) +
geom_histogram(aes(y=..density..), position="dodge", alpha=0.7, bins = 30)+
geom_density(alpha=0.1)+
geom_vline(data=mu_heads_no_out, aes(xintercept=mean_heads, color=agri_organic),
           linetype="dashed")+
xlim(0,300)+
scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
scale_fill_discrete(labels = c("Participants", ' Not Participants'))+
scale_colour_discrete(labels = c("Participants", ' Not Participants'))+
  guides(fill = guide_legend(title = "Agri-environment and organic scheme participation"),
         colour = guide_legend(title = "Agri-environment and organic scheme participation"))+
labs(title="Livestock histogram",x="Livestock (Heads)", y = "Density")+
theme(legend.position="top")

# COMMAND ----------

# summary tables with no outliers 

summary_bps_lu_nooutliers <- wfm_no_outlier %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_bps_lu, na.rm = TRUE),
            med = median(sd_bps_lu, na.rm = TRUE),
           min = min(sd_bps_lu, na.rm = TRUE),
           max = max(sd_bps_lu, na.rm = TRUE))
  
summary_grass_heads_nooutliers <- wfm_no_outlier %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_heads_grass, na.rm = TRUE),
            med = median(sd_heads_grass, na.rm = TRUE),
           min = min(sd_heads_grass, na.rm = TRUE),
           max = max(sd_heads_grass, na.rm = TRUE))
  
summary_grass_lu_nooutliers <- wfm_no_outlier %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_grass_lu, na.rm = TRUE),
            med = median(sd_grass_lu, na.rm = TRUE),
           min = min(sd_grass_lu, na.rm = TRUE),
           max = max(sd_grass_lu, na.rm = TRUE))
  
summary_heads_bps_nooutliers <- wfm_no_outlier %>%
  group_by(agri_organic) %>%
  summarise(mean = mean(sd_heads_bps, na.rm = TRUE),
            med = median(sd_heads_bps, na.rm = TRUE),
           min = min(sd_heads_bps, na.rm = TRUE),
           max = max(sd_heads_bps, na.rm = TRUE))
  
  

# COMMAND ----------

display(summary_bps_lu_nooutliers)

# COMMAND ----------

display(summary_grass_heads_nooutliers)

# COMMAND ----------

display(summary_grass_lu_nooutliers)

# COMMAND ----------

display(summary_heads_bps_nooutliers)
