### Script 3: Get Extreme SPEI values for each admin 1 unit, check historical against EMDAT

library(sf)
library(tidyverse)


#load admin units data and get ids
units_path  <- "inputdata/tests_shps.shp"  # polygons (any CRS)
units_sf <- st_read(units_path, quiet = TRUE) %>%
  #filter(nm_slc0 != "Sub-Saharan Africa") %>%
  select(-farm_sz, -frm2020, -NAME_0) %>% unique()
units_all <- units_sf %>% st_drop_geometry()
units_all <- units_all %>% select( GID_0, nm_slc0, NAME_1)
colnames(units_all) <- c("ISO", "Country", "Admin1_og")
units <- terra::vect(units_sf)
units$ID <- seq_len(nrow(units))

#add spei
yearly_minmax <- readr::read_csv("intermediatedata/spei_12_ensemble_historical_1995_2015_admin1_gsminmax.csv")

#add EMDAT data 
emdat <- read_csv("inputdata/emdat_flood_drought_1995_2015.csv")

#Join together
namesdf <-data.frame(admin1_name = units_all$Admin1_og, ID = units$ID,  ISO = units_all$ISO)
yearly_minmax_full <- full_join(namesdf, yearly_minmax, by="ID")
yearly_minmax_full <- yearly_minmax_full %>% select(-n_months_gs)

minmaxtrue <- full_join(emdat, yearly_minmax_full, by=c("ISO", "admin1_name", "year"))
minmaxtrue <- minmaxtrue %>% filter(!is.na(minspei_gs))
minmaxtrue <- minmaxtrue %>% filter(!is.na(maxspei_gs))

#filter only countries with events
iso_true <- emdat %>% select(ISO) %>% unique()
minmaxtrue <- minmaxtrue %>% filter(ISO %in% iso_true$ISO)

#Compute flood values
tai <- minmaxtrue
tai <- tai %>% filter(`Disaster Type`=="Flood" | is.na(`Disaster Type`))
tai$high <- ifelse(tai$maxspei_gs>=1, 1, 0)
tai$extreme <- ifelse(tai$maxspei_gs>=1.5, 1, 0)

test <- tai %>% group_by(high, `Disaster Type`) %>% 
  summarize(count=n())
colnames(test) <- c("value", "EMDAT", "count")
test$variable2 <- "spei6"
test$variable3 <- "moderately wet"

test2 <- tai %>% group_by(extreme, `Disaster Type`) %>% 
  summarize(count=n())
colnames(test2) <- c("value", "EMDAT", "count")
test2$variable2 <- "spei6"
test2$variable3 <- "very wet"

all_c_tai <- bind_rows(test, test2)
all_c_tai$variable1 <- "max"
all_c_tai <-all_c_tai %>%  select(variable1, variable2, variable3, value, EMDAT, count)
all_c_tai <- all_c_tai %>% filter(!(value==0 & is.na(EMDAT)))
all_c_tai$percent_emdat <- NA
all_c_tai$percent_emdat <- ifelse(all_c_tai$EMDAT=="Flood" & all_c_tai$value==1, all_c_tai$count/753, NA)


#Compute drought vaulues
taid <- minmaxtrue
taid <- taid %>% filter(`Disaster Type`=="Drought" | is.na(`Disaster Type`))
taid$high <- ifelse(taid$minspei_gs>=-1.0, 0, 1)
taid$extreme <- ifelse(taid$minspei_gs>=-1.5, 0, 1)

testd <- taid %>% group_by(high, `Disaster Type`) %>% 
  summarize(count=n())
colnames(testd) <- c("value", "EMDAT", "count")
testd$variable2 <- "spei6"
testd$variable3 <- "moderately dry"

test2d <- taid %>% group_by(extreme, `Disaster Type`) %>% 
  summarize(count=n())
colnames(test2d) <- c("value", "EMDAT", "count")
test2d$variable2 <- "spei6"
test2d$variable3 <- "very dry"

all_c_taid <- bind_rows(testd, test2d)
all_c_taid$variable1 <- "min"
all_c_taid <-all_c_taid %>%  select(variable1, variable2, variable3, value, EMDAT, count)
all_c_taid <- all_c_taid %>% filter(!(value==0 & is.na(EMDAT)))
#all_c_taid$percent_all <- all_c_taid$count/361
all_c_taid$percent_emdat <- NA
all_c_taid$percent_emdat <- ifelse(all_c_taid$EMDAT=="Drought" & all_c_taid$value==1, all_c_taid$count/345, NA)

all_newmthod <- bind_rows(all_c_tai, all_c_taid)

#EMDAT captures between 15-20% of all high wet/ high dry events
