## Script 0: Clean up prior analysis on farm size projections and models to estimate loss/lost revenue

#load libraries
library(tidyverse)
library(glmmTMB)
library(lme4)
library(ggeffects)
library(sf)



## Part 1: Create abridged shapefiles from farm projections analysis (https://github.com/Better-Planet-Laboratory/africafarmprojections)
frm <- st_read("inputdata/fsfix_corrected_2000_2060.shp")
fs_shp <- frm
fs_shp <- fs_shp %>% select(GID_0, NAME_0, geometry) %>% unique()

frm <- frm %>% select(-trtmnt_, -Mdn_smt, -Agar_kh)
frm <- frm %>% gather(N0_1:N20_, key="farm_sz", value="value")
frm <- frm %>% filter(Year %in% c(2020, 2050))
frm <- frm %>% st_drop_geometry()
frmshort <- frm %>% group_by(GID_0, NAME_0, farm_sz, Year) %>% summarize (value=sum(value, na.rm=TRUE)) 
frmshort$NAME_1 <- NA
frmshort$nm_slc0 <- "Sub-Saharan Africa"
frmshort <- frmshort %>% select(GID_0, NAME_0, NAME_1, farm_sz, Year, value, nm_slc0)
frm$nm_slc0 <- frm$NAME_0
frm$NAME_0 <- frm$NAME_1
frm <- frm %>% select(GID_0, NAME_0, NAME_1, farm_sz, Year, value, nm_slc0)

all_frm <- bind_rows(frm, frmshort)
all_frm_fin <- left_join(all_frm, fs_shp, by=c("GID_0", "NAME_0"))
names(all_frm_fin)[names(all_frm_fin) == 'Year'] <- 'year'
all_frm_fin <- all_frm_fin %>% select(GID_0, NAME_0, NAME_1, nm_slc0, year, farm_sz, value, geometry)

st_write(all_frm50_fin, "inputdata/africa_2020_2050sizes.shp")




#Part 2: Create 2020 and 2050 predictions of loss/lost revenue from farm size models (https://github.com/Better-Planet-Laboratory/farm-loss-farmsize)
#load models
model_binary_con <- readRDS("inputdata/model_binary_th.RDS")
model_con <- readRDS("inputdata/model_con_th.RDS")

#Load 2020 and 2050 farm sizes
fs_shp <- st_read("inputdata/africa_2020_2050sizes.shp")
fs_dat <- fs_shp %>% st_drop_geometry()
fs_dat$farm_size <- ifelse(fs_dat$farm_sz=="N0_1", 0.5,
                           ifelse(fs_dat$farm_sz=="N1_2", 1.5,
                                  ifelse(fs_dat$farm_sz=="N2_5",3.5,
                                         ifelse(fs_dat$farm_sz=="N5_10", 7.5,
                                                ifelse(fs_dat$farm_sz=="N10_20",15,
                                                       ifelse(fs_dat$farm_sz=="N20_", 35, NA))))))

fs_dat$LogFSIZE <- log(fs_dat$farm_size)

#flood
fs_dat_flood <- fs_dat
fs_dat_flood$Event <- "Flood"
fs_dat_flood$pred_loss <- predict(model_binary_con, fs_dat_flood, type="response",re.form=NA)
fs_dat_flood$pred_rev <- predict(model_con, fs_dat_flood, type="response", re.form=NA)

#drought
fs_dat_drought <- fs_dat
fs_dat_drought$Event <- "Drought"
fs_dat_drought$pred_loss <- predict(model_binary_con, fs_dat_drought, type="response",re.form=NA)
fs_dat_drought$pred_rev <- predict(model_con, fs_dat_drought, type="response", re.form=NA)

all_fs_pred <- bind_rows(fs_dat_flood, fs_dat_drought)
all_fs_pred <- all_fs_pred %>% select(GID_0, nm_slc0, NAME_0, NAME_1, year, farm_sz, value, farm_size, LogFSIZE, Event, pred_loss, pred_rev)
write_csv(all_fs_pred, "inputdata/farmsize_predictions.csv")


