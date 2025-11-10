### Script 5: Compute yearly and cummulate losses/lost revenue by admin1/farm size/event type
library(tidyverse)
library(ggrepel)

### PART 1: Historical or SSPs
#drought
testdrought <- read_csv("intermediate/spei_12_ensemble_drought_historical_predictions_high.csv")
testdrought$high_loss <- testdrought$pred_loss*testdrought$high*testdrought$frm2020
testdrought$high_rev <- testdrought$high*testdrought$pred_rev

#loss
drought_thresholds <- testdrought
drought_thresholds <- drought_thresholds %>% filter(farm_sz!="All Farms")
drought_thresholds <- drought_thresholds %>% group_by(nm_slc0, farm_sz, model, gcm, year) %>% summarize(numberofarms=sum(high_loss, na.rm=TRUE))
allfarms <- drought_thresholds %>% group_by(nm_slc0,model, gcm, year) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
allfarms$farm_sz <- "All Farms"
drought_thresholds <- bind_rows(drought_thresholds, allfarms)
drought_thresholds <- drought_thresholds %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- drought_thresholds %>% group_by(farm_sz,model, gcm, year) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
drought_thresholds <- bind_rows(drought_thresholds, ssa_drt)
drought_thresholds <-  drought_thresholds %>% group_by(nm_slc0,model, gcm, farm_sz) %>%  mutate(cum_sum = cumsum(numberofarms))
drought_thresholds$Event <- "Drought"
drought_thresholds$threshold <- "Farms Likely Impacted"
drought_thresholds <- drought_thresholds %>% select(nm_slc0, farm_sz, model, gcm, year, threshold, numberofarms, cum_sum, Event)


#rev
drought_thresholdsrev <- testdrought
drought_thresholdsrev$drought_loss_5 <- ifelse(drought_thresholdsrev$high_rev>0.05, drought_thresholdsrev$high_loss, 0)
drought_thresholdsrev$drought_loss_10 <- ifelse(drought_thresholdsrev$high_rev>0.1, drought_thresholdsrev$high_loss, 0)
drought_thresholdsrev$drought_loss_20 <- ifelse(drought_thresholdsrev$high_rev>0.2, drought_thresholdsrev$high_loss, 0)
drought_thresholdsrev$drought_loss_30 <- ifelse(drought_thresholdsrev$high_rev>0.3, drought_thresholdsrev$high_loss, 0)
drought_thresholdsrev$drought_loss_40 <- ifelse(drought_thresholdsrev$high_rev>0.4, drought_thresholdsrev$high_loss, 0)
drought_thresholdsrev$drought_loss_50 <- ifelse(drought_thresholdsrev$high_rev>0.5, drought_thresholdsrev$high_loss, 0)
#drought_thresholdsrev$drought_loss_60 <- ifelse(drought_thresholdsrev$high_rev>0.6, drought_thresholdsrev$high_loss, 0)

drought_thresholdsrev <- drought_thresholdsrev %>% group_by(nm_slc0, farm_sz,model, gcm, year) %>% summarize(`5% lost revenue`=sum(drought_loss_5, na.rm=TRUE),
                                                                                                             `10% lost revenue`=sum(drought_loss_10, na.rm=TRUE),
                                                                                                             `20% lost revenue`=sum(drought_loss_20, na.rm=TRUE),
                                                                                                             `30% lost revenue`=sum(drought_loss_30, na.rm=TRUE),
                                                                                                             `40% lost revenue`=sum(drought_loss_40, na.rm=TRUE),
                                                                                                             `50% lost revenue`=sum(drought_loss_50, na.rm=TRUE))

drought_thresholdsrev <- drought_thresholdsrev %>% filter(farm_sz!="All Farms")
drought_thresholdsrev <- drought_thresholdsrev %>% gather(`5% lost revenue`:`50% lost revenue`, key="threshold", value="numberofarms")
allfarmsrev <- drought_thresholdsrev %>% group_by(nm_slc0, model, gcm, year, threshold) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
allfarmsrev$farm_sz <- "All Farms"
drought_thresholdsrev <- bind_rows(drought_thresholdsrev, allfarmsrev)
drought_thresholdsrev <- drought_thresholdsrev %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drtrev <- drought_thresholdsrev %>% group_by(farm_sz, model, gcm, year, threshold) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
ssa_drtrev$nm_slc0 <- "Sub-Saharan Africa"
drought_thresholdsrev <- bind_rows(drought_thresholdsrev, ssa_drtrev)
drought_thresholdsrev <-  drought_thresholdsrev %>% group_by(nm_slc0, farm_sz, model, gcm, threshold) %>%  mutate(cum_sum = cumsum(numberofarms))
drought_thresholdsrev$Event <- "Drought"


#floods
testflood <- read_csv("intermediate/spei_12_ensemble_flood_historical_predictions_high.csv")
testflood$high_loss <- testflood$pred_loss*testflood$high*testflood$frm2020
testflood$high_rev <- testflood$high*testflood$pred_rev

#loss
flood_thresholds <- testflood
flood_thresholds <- flood_thresholds %>% filter(farm_sz!="All Farms")
flood_thresholds <- flood_thresholds %>% group_by(nm_slc0, farm_sz, model, gcm, year) %>% summarize(numberofarms=sum(high_loss, na.rm=TRUE))
allfarms <- flood_thresholds %>% group_by(nm_slc0, model, gcm, year) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
allfarms$farm_sz <- "All Farms"
flood_thresholds <- bind_rows(flood_thresholds, allfarms)
flood_thresholds <- flood_thresholds %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- flood_thresholds %>% group_by(farm_sz, model, gcm, year) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
flood_thresholds <- bind_rows(flood_thresholds, ssa_drt)
flood_thresholds <-  flood_thresholds %>% group_by(nm_slc0, model, gcm, farm_sz) %>%  mutate(cum_sum = cumsum(numberofarms))
flood_thresholds$Event <- "Flood"
flood_thresholds$threshold <- "Farms Likely Impacted"
flood_thresholds <- flood_thresholds %>% select(nm_slc0, farm_sz, model, gcm, year, threshold, numberofarms, cum_sum, Event)

#rev flood
flood_thresholdsrev <- testflood
flood_thresholdsrev$flood_loss_5 <- ifelse(flood_thresholdsrev$high_rev>0.05, flood_thresholdsrev$high_loss, 0)
flood_thresholdsrev$flood_loss_10 <- ifelse(flood_thresholdsrev$high_rev>0.1, flood_thresholdsrev$high_loss, 0)
flood_thresholdsrev$flood_loss_20 <- ifelse(flood_thresholdsrev$high_rev>0.2, flood_thresholdsrev$high_loss, 0)
flood_thresholdsrev$flood_loss_30 <- ifelse(flood_thresholdsrev$high_rev>0.3, flood_thresholdsrev$high_loss, 0)
flood_thresholdsrev$flood_loss_40 <- ifelse(flood_thresholdsrev$high_rev>0.4, flood_thresholdsrev$high_loss, 0)
flood_thresholdsrev$flood_loss_50 <- ifelse(flood_thresholdsrev$high_rev>0.5, flood_thresholdsrev$high_loss, 0)

flood_thresholdsrev <- flood_thresholdsrev %>% group_by(nm_slc0, farm_sz, model, gcm, year) %>% summarize(`5% lost revenue`=sum(flood_loss_5, na.rm=TRUE),
                                                                                                          `10% lost revenue`=sum(flood_loss_10, na.rm=TRUE),
                                                                                                          `20% lost revenue`=sum(flood_loss_20, na.rm=TRUE),
                                                                                                          `30% lost revenue`=sum(flood_loss_30, na.rm=TRUE),
                                                                                                          `40% lost revenue`=sum(flood_loss_40, na.rm=TRUE),
                                                                                                          `50% lost revenue`=sum(flood_loss_50, na.rm=TRUE))

flood_thresholdsrev <- flood_thresholdsrev %>% filter(farm_sz!="All Farms")
flood_thresholdsrev <- flood_thresholdsrev %>% gather(`5% lost revenue`:`50% lost revenue`, key="threshold", value="numberofarms")
allfarmsrev <- flood_thresholdsrev %>% group_by(nm_slc0, model, gcm, year, threshold) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
allfarmsrev$farm_sz <- "All Farms"
flood_thresholdsrev <- bind_rows(flood_thresholdsrev, allfarmsrev)
flood_thresholdsrev <- flood_thresholdsrev %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drtrev <- flood_thresholdsrev %>% group_by(farm_sz, model, gcm, year, threshold) %>%  summarize(numberofarms=sum(numberofarms, na.rm=TRUE))
ssa_drtrev$nm_slc0 <- "Sub-Saharan Africa"
flood_thresholdsrev <- bind_rows(flood_thresholdsrev, ssa_drtrev)
flood_thresholdsrev <-  flood_thresholdsrev %>% group_by(nm_slc0, model, gcm, farm_sz, threshold) %>%  mutate(cum_sum = cumsum(numberofarms))
flood_thresholdsrev$Event <- "Flood"


#join all datasets
loss <- bind_rows(drought_thresholds, flood_thresholds)
loss$losstype <- "Loss"
rev <- bind_rows(drought_thresholdsrev, flood_thresholdsrev)
rev$losstype <- "Revenue"

joint <- bind_rows(loss, rev)
joint$model <- "baseline"
write_csv(joint, "intermediate/allloss_by_threshold_fin.csv")


### PART 2: Join all historical and ssp datasets
joint1 <- read_csv("intermediate/allloss_by_threshold_fin.csv")
joint2 <- read_csv("sintermediate/allloss_by_threshold_fin_spei12_ssp245.csv")
joint3 <- read_csv("intermediate/allloss_by_threshold_fin_spei12_ssp585.csv")

alljoint <- bind_rows(joint1, joint2, joint3)
alljoint$Event <- ifelse(alljoint$Event=="Drought", "Very Dry", "Very Wet")
alljoint$farm_sz2 <- alljoint$farm_sz
alljoint$farm_sz2 <- ifelse(alljoint$farm_sz2=="N0_1", "Under 1 HA",
                            ifelse(alljoint$farm_sz=="N1_2", "1-2 HA",
                                   ifelse(alljoint$farm_sz=="N2_5","2-5 HA",
                                          ifelse(alljoint$farm_sz=="N5_10", "5-10 HA",
                                                 ifelse(alljoint$farm_sz=="N10_20", "10-20 HA",
                                                        ifelse(alljoint$farm_sz=="N20_", "20+ HA", "All Farms"))))))

alljoint$model <- ifelse(alljoint$model=="ssp245", "SSP245",
                         ifelse(alljoint$model=="ssp585", "SSP585", alljoint$model))

alljointcount <- alljoint %>% filter(threshold == "Farms Likely Impacted")
alljointpct <- alljoint %>% filter(threshold != "Farms Likely Impacted")
alljointpct$threshold2 <- as.numeric(str_extract(alljointpct$threshold, "\\d+"))
alljointpct_wet <- alljointpct %>% filter(Event=="Very Wet")
alljointpct_dry <- alljointpct %>% filter(Event=="Very Dry")

alljointpct_wet <- alljointpct_wet %>% filter(threshold2 %in% c(30, 40))
alljointpct_dry <- alljointpct_dry %>% filter(threshold2 %in% c(40, 50))

all_rejoined <- bind_rows(alljointcount, alljointpct_wet, alljointpct_dry)

write_csv(all_rejoined, "finaloutput/allloss_by_threshold_fin_spei12_high_all.csv")


