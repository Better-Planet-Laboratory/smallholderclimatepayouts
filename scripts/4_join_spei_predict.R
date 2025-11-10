### Script 4: Join SPEI values to Country & Admin1 names,  get loss and damage values from our functions, create dataframes with farm size

library(sf)
library(tidyverse)
library(glmmTMB)
library(lme4)
library(ggeffects)

### PART 1: Join SPEI values to Country & Admin1 names, get high/extreme values
## Read in shapefiles to get admin0/admin 1names
all_nwl0_countries <- readr::read_csv("inputdata/tai_allmodels_admin1.csv")
admin_lookup <- all_nwl0_countries %>% select(GID_0, NAME_0, NAME_1) %>% unique()
units_path  <- "inputdata/tests_shps.shp"  # polygons (any CRS)
units_sf <- st_read(units_path, quiet = TRUE) %>%
  select(-farm_sz, -frm2020, -NAME_0) %>% unique()
units_all <- units_sf %>% st_drop_geometry()
units_all <- units_all %>% select( GID_0, nm_slc0, NAME_1)
colnames(units_all) <- c("ISO", "Country", "Admin1_og")
units <- terra::vect(units_sf)
units$ID <- seq_len(nrow(units))

## ENSEMBLE
## Add historical SPEI
yearly_minmax <- readr::read_csv("intermediate/spei_12_ensemble_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

## Add 245 SPEI
yearly_minmax2 <- readr::read_csv("intermediate/spei_12_ensemble_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

## Add 585 SPEI
yearly_minmax3 <- readr::read_csv("intermediate/spei_12_ensemble_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

#Bind all
yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)


## Categorize high & extreme
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"

yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "ensemble"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_ensemble.csv")




## GFDLESM4
yearly_minmax <- readr::read_csv("intermediate/spei_12_gfdlesm4_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

yearly_minmax2 <- readr::read_csv("intermediate/spei_12_gfdlesm4_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

yearly_minmax3 <- readr::read_csv("intermediate/spei_12_gfdlesm4_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"
yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "gfdlesm4"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_gfdlesm4.csv")


## ecearth3
yearly_minmax <- readr::read_csv("intermediate/spei_12_ecearth3_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

yearly_minmax2 <- readr::read_csv("intermediate/spei_12_ecearth3_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

yearly_minmax3 <- readr::read_csv("intermediate/spei_12_ecearth3_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"
yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "ecearth3"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_ecearth3.csv")

## mpiesmhr
yearly_minmax <- readr::read_csv("intermediate/spei_12_mpiesmhr_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

yearly_minmax2 <- readr::read_csv("intermediate/spei_12_mpiesmhr_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

yearly_minmax3 <- readr::read_csv("intermediate/spei_12_mpiesmhr_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"
yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "mpiesmhr"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_mpiesmhr.csv")


## mriesm2
yearly_minmax <- readr::read_csv("intermediate/spei_12_mriesm2_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

yearly_minmax2 <- readr::read_csv("intermediate/spei_12_mriesm2_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

yearly_minmax3 <- readr::read_csv("intermediate/spei_12_mriesm2_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"
yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "mriesm2"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_mriesm2.csv")



## noresm2lm
yearly_minmax <- readr::read_csv("intermediate/spei_12_noresm2lm_historical_1995_2015_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)
yearly_minmax_full$spei <- "spei12"
yearly_minmax_full$model <- "historical"

yearly_minmax2 <- readr::read_csv("intermediate/spei_12_noresm2lm_ssp245_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full2 <- full_join(namesdf, yearly_minmax2, by="ID")
yearly_minmax_full2 <- yearly_minmax_full2 %>% select(-n_months_gs)
yearly_minmax_full2$spei <- "spei12"
yearly_minmax_full2$model <- "ssp245"

yearly_minmax3 <- readr::read_csv("intermediate/spei_12_noresm2lm_ssp585_2040_2060_admin1_gsminmax.csv")
namesdf <-data.frame(GID_0=admin_lookup$GID_0, NAME_0=admin_lookup$NAME_0, NAME_1=admin_lookup$NAME_1, ID=units$ID)
yearly_minmax_full3 <- full_join(namesdf, yearly_minmax3, by="ID")
yearly_minmax_full3 <- yearly_minmax_full3 %>% select(-n_months_gs)
yearly_minmax_full3$spei <- "spei12"
yearly_minmax_full3$model <- "ssp585"

yearly_minmax_full <- bind_rows(yearly_minmax_full, yearly_minmax_full2, yearly_minmax_full3)
yearly_minmax_full_flood <- yearly_minmax_full %>% select(-minspei_gs)
yearly_minmax_full_flood$variable <- "flood"
yearly_minmax_full_flood$high <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1, 1, 0)
yearly_minmax_full_flood$extreme <- ifelse(yearly_minmax_full_flood$maxspei_gs>=1.5, 1, 0)
yearly_minmax_full_flood <- yearly_minmax_full_flood %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, maxspei_gs, high, extreme)
colnames(yearly_minmax_full_flood)[which(names(yearly_minmax_full_flood) == "maxspei_gs")] <- "value"
yearly_minmax_full_drought <- yearly_minmax_full %>% select(-maxspei_gs)
yearly_minmax_full_drought$variable <- "drought"
yearly_minmax_full_drought$high <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.0, 0, 1)
yearly_minmax_full_drought$extreme <- ifelse(yearly_minmax_full_drought$minspei_gs>=-1.5, 0, 1)
yearly_minmax_full_drought <- yearly_minmax_full_drought %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, year, minspei_gs, high, extreme)
colnames(yearly_minmax_full_drought)[which(names(yearly_minmax_full_drought) == "minspei_gs")] <- "value"

all_joint <- bind_rows(yearly_minmax_full_flood, yearly_minmax_full_drought)
all_joint$gcm <- "noresm2lm"
all_joint <- all_joint %>% select(GID_0, NAME_0, NAME_1, spei, variable, model, gcm, year, value, high, extreme)
readr::write_csv(all_joint, "intermediate/finalclimate/spei_12_noresm2lm.csv")


### PART2: Get loss and damage values from our functions, create dataframes with farm size

## Step 1: HISTORICAL : Join by climate data to get loss by years & by number of farms
#load loss and damage predictions
all_fs_pred <- read_csv("input/fs_2020_predictions.csv")

#flood
waterlog <- read_csv("intermediate/spei_12_ensemble.csv")
waterlog <- waterlog %>% filter(variable=="flood")
waterlog <- waterlog %>% filter(model=="historical")
waterlog_joined <- full_join(all_fs_pred, waterlog, by=c("GID_0", "NAME_0", "NAME_1"))
waterlog_joined <- waterlog_joined %>% filter(Event=="Flood")
waterlog_joined <- waterlog_joined %>% drop_na(high)
waterlog_joined_high <- waterlog_joined %>% select(-extreme, -value)
write_csv(waterlog_joined_high, "intermediate/spei_12_ensemble_flood_historical_predictions_high.csv")

#drought
drought <- read_csv("intermediate/finalclimate/spei_12_ensemble.csv")
drought <- drought %>% filter(variable=="drought")
drought <- drought %>% filter(model=="historical")
drought_joined <- full_join(all_fs_pred, drought, by=c("GID_0", "NAME_0", "NAME_1"))
drought_joined <- drought_joined %>% filter(Event=="Drought")
drought_joined <- drought_joined %>% drop_na(high)
drought_joined_high <- drought_joined %>% select(-extreme, -value)
write_csv(drought_joined_high, "intermediate/spei_12_ensemble_drought_historical_predictions_high.csv")


## Step 2: SSP245 & SSP585: Join by climate data to get loss by years & by number of farms
#load loss and damage predictions
all_fs_pred <- read_csv("intermediate/fs_2050_predictions.csv")
#flood
waterlog <- read_csv("intermediate/spei_12_ensemble.csv")
waterlog <- waterlog %>% filter(variable=="flood")
waterlog <- waterlog %>% filter(model=="ssp585")
waterlog_joined <- full_join(all_fs_pred, waterlog, by=c("GID_0", "NAME_0", "NAME_1"))
waterlog_joined <- waterlog_joined %>% filter(Event=="Flood")
waterlog_joined <- waterlog_joined %>% drop_na(high)
waterlog_joined_high <- waterlog_joined %>% select(-extreme, -value)
write_csv(waterlog_joined_high, "spei_analysis/finalclimate/spei_12_ensemble_flood_ssp585_predictions_high.csv")

waterlog <- read_csv("intermediate/spei_12_ensemble.csv")
waterlog <- waterlog %>% filter(variable=="flood")
waterlog <- waterlog %>% filter(model=="ssp245")
waterlog_joined <- full_join(all_fs_pred, waterlog, by=c("GID_0", "NAME_0", "NAME_1"))
waterlog_joined <- waterlog_joined %>% filter(Event=="Flood")
waterlog_joined <- waterlog_joined %>% drop_na(high)
waterlog_joined_high <- waterlog_joined %>% select(-extreme, -value)
write_csv(waterlog_joined_high, "spei_analysis/finalclimate/spei_12_ensemble_flood_ssp245_predictions_high.csv")

#drought
drought <- read_csv("intermediate/spei_12_ensemble.csv")
drought <- drought %>% filter(variable=="drought")
drought <- drought %>% filter(model=="ssp585")
drought_joined <- full_join(all_fs_pred, drought, by=c("GID_0", "NAME_0", "NAME_1"))
drought_joined <- drought_joined %>% filter(Event=="Drought")
drought_joined <- drought_joined %>% drop_na(high)
drought_joined_high <- drought_joined %>% select(-extreme, -value)
write_csv(drought_joined_high, "intermediate/spei_12_ensemble_drought_ssp585_predictions_high.csv")

drought <- read_csv("intermediate/spei_12_ensemble.csv")
drought <- drought %>% filter(variable=="drought")
drought <- drought %>% filter(model=="ssp245")
drought_joined <- full_join(all_fs_pred, drought, by=c("GID_0", "NAME_0", "NAME_1"))
drought_joined <- drought_joined %>% filter(Event=="Drought")
drought_joined <- drought_joined %>% drop_na(high)
drought_joined_high <- drought_joined %>% select(-extreme, -value)
write_csv(drought_joined_high, "intermediate/spei_12_ensemble_drought_ssp245_predictions_high.csv")


### PART 3: Add shapefiles
## Step 1: load historical data
drought1_joined <- read_csv("intermediate/spei_12_ensemble_drought_historical_predictions_high.csv")
drought1_joined <- drought1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2020, Event, model, gcm, year, high) %>% unique()
drought1_joined$high_fs <- drought1_joined$high*drought1_joined$frm2020
drought1_joined <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
drought1_joined <- drought1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
drought1_joined <- bind_rows(drought1_joined, allfarms_drt)
drought1_joined <- drought1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
drought1_joined <- bind_rows(drought1_joined, ssa_drt)

#flood
flood1_joined <- read_csv("intermediate/spei_12_ensemble_flood_historical_predictions_high.csv")
flood1_joined <- flood1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2020, Event, model, gcm, year, high) %>% unique()
flood1_joined$high_fs <- flood1_joined$high*flood1_joined$frm2020
flood1_joined <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
flood1_joined <- flood1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
flood1_joined <- bind_rows(flood1_joined, allfarms_drt)
flood1_joined <- flood1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
flood1_joined <- bind_rows(flood1_joined, ssa_drt)

#join and add shape file
all_joint <- bind_rows(drought1_joined, flood1_joined)
fs_shp <- st_read("intermediate/tests2020_shps.shp")
fs_shp <- fs_shp %>% select(-farm_sz, -frm2020) %>% unique()

all_joint_shp <- left_join(all_joint, fs_shp, by=c("GID_0", "nm_slc0", "NAME_0", "NAME_1"))
all_joint_shp$model <- "baseline"
st_write(all_joint_shp, "spei_analysis/final/fig3_map_spei12_baseline_high.shp", delete_dsn = T)


## Step 2: SSP245/585: Add to shapefile to get number of farms affect by type of extreme event and farm size
#drought 585
drought1_joined <- read_csv("intermediate/spei_12_ensemble_drought_ssp585_predictions_high.csv")
drought1_joined <- drought1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2050, Event, model, gcm, year, high) %>% unique()
drought1_joined$high_fs <- drought1_joined$high*drought1_joined$frm2050
drought1_joined <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
drought1_joined <- drought1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
drought1_joined <- bind_rows(drought1_joined, allfarms_drt)
drought1_joined <- drought1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
drought1_joined <- bind_rows(drought1_joined, ssa_drt)

#flood 585
flood1_joined <- read_csv("intermediate/spei_12_ensemble_flood_ssp585_predictions_high.csv")
flood1_joined <- flood1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2050, Event, model, gcm, year, high) %>% unique()
flood1_joined$high_fs <- flood1_joined$high*flood1_joined$frm2050
flood1_joined <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
flood1_joined <- flood1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
flood1_joined <- bind_rows(flood1_joined, allfarms_drt)
flood1_joined <- flood1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
flood1_joined <- bind_rows(flood1_joined, ssa_drt)

#join and add shape file
all_joint <- bind_rows(drought1_joined, flood1_joined)
#fs_shp <- st_read("fsfix/tests_shps.shp")
fs_shp <- st_read("intermediate/tests2020_shps.shp")
fs_shp <- fs_shp %>% select(-farm_sz, -frm2020) %>% unique()

all_joint_shp <- left_join(all_joint, fs_shp, by=c("GID_0", "nm_slc0", "NAME_0", "NAME_1"))
all_joint_shp$model <- "ssp585"
st_write(all_joint_shp, "intermediate/fig3_map_spei12_ssp585_high.shp", delete_dsn = T)

#drought 245
drought1_joined <- read_csv("intermediate/spei_12_ensemble_drought_ssp245_predictions_high.csv")
drought1_joined <- drought1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2050, Event, model, gcm, year, high) %>% unique()
drought1_joined$high_fs <- drought1_joined$high*drought1_joined$frm2050
drought1_joined <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
drought1_joined <- drought1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
drought1_joined <- bind_rows(drought1_joined, allfarms_drt)
drought1_joined <- drought1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- drought1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
drought1_joined <- bind_rows(drought1_joined, ssa_drt)

#flood
flood1_joined <- read_csv("intermediate/spei_12_ensemble_flood_ssp245_predictions_high.csv")
flood1_joined <- flood1_joined %>% select(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, frm2050, Event, model, gcm, year, high) %>% unique()
flood1_joined$high_fs <- flood1_joined$high*flood1_joined$frm2050
flood1_joined <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, farm_sz, Event, model, gcm) %>% summarize(high_fs=mean(high_fs, na.rm=TRUE))
flood1_joined <- flood1_joined %>% filter(farm_sz!="All Farms")
allfarms_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, NAME_0, NAME_1, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
allfarms_drt$farm_sz <- "All Farms"
flood1_joined <- bind_rows(flood1_joined, allfarms_drt)
flood1_joined <- flood1_joined %>% filter(nm_slc0!="Sub-Saharan Africa")
ssa_drt <- flood1_joined %>% group_by(GID_0, nm_slc0, farm_sz, Event, model, gcm) %>%  summarize(high_fs=sum(high_fs, na.rm=TRUE))
ssa_drt$NAME_0 <- ssa_drt$nm_slc0
ssa_drt$nm_slc0 <- "Sub-Saharan Africa"
ssa_drt$NAME_1 <- NA
flood1_joined <- bind_rows(flood1_joined, ssa_drt)

#join and add shape file
all_joint <- bind_rows(drought1_joined, flood1_joined)
#fs_shp <- st_read("fsfix/tests_shps.shp")
fs_shp <- st_read("intermediate/tests2020_shps.shp")
fs_shp <- fs_shp %>% select(-farm_sz, -frm2020) %>% unique()

all_joint_shp <- left_join(all_joint, fs_shp, by=c("GID_0", "nm_slc0", "NAME_0", "NAME_1"))
all_joint_shp$model <- "ssp245"
st_write(all_joint_shp, "intermediate/fig3_map_spei12_ssp245_high.shp", delete_dsn = T)



#all joint shps
all_joint1 <- st_read("intermediate/fig3_map_spei12_baseline_high.shp")
all_joint2 <- st_read("intermediate/fig3_map_spei12_ssp245_high.shp")
all_joint3 <- st_read("intermediate/fig3_map_spei12_ssp585_high.shp")

all_joint_fin <- bind_rows(all_joint1, all_joint2, all_joint3)

all_joint_fin$Event <- ifelse(all_joint_fin$Event=="Drought", "Very Dry", "Very Wet")
all_joint_fin$farm_sz2 <- all_joint_fin$farm_sz
all_joint_fin$farm_sz2 <- ifelse(all_joint_fin$farm_sz2=="N0_1", "Under 1 HA",
                                 ifelse(all_joint_fin$farm_sz=="N1_2", "1-2 HA",
                                        ifelse(all_joint_fin$farm_sz=="N2_5","2-5 HA",
                                               ifelse(all_joint_fin$farm_sz=="N5_10", "5-10 HA",
                                                      ifelse(all_joint_fin$farm_sz=="N10_20", "10-20 HA",
                                                             ifelse(all_joint_fin$farm_sz=="N20_", "20+ HA", "All Farms"))))))
all_joint_fin$model <- ifelse(all_joint_fin$model=="ssp245", "SSP245",
                              ifelse(all_joint_fin$model=="ssp585", "SSP585", all_joint_fin$model))

st_write(all_joint_fin, "finaloutput/fig3_map_fin_spei12_high.shp", delete_dsn = T)
