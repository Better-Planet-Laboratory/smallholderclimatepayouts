# Climate payouts to smallholder farmers
## Overview
This repository contains statistical analysis to develop the “Climate payouts to smallholder farmers” notebook for the African Adaptation Atlas. The analysis builds on [work]( https://github.com/Better-Planet-Laboratory/africafarmprojections) from Zia Mehrabi that developed African farm size distributions from 2000-2050 as well as [analysis]( https://github.com/Better-Planet-Laboratory/farm-loss-farmsize) from Zia Mehrabi, Julie Fortin and Navin Ramankutty that harmonizes household survey data to examine the relationship between farm size and climate shock impacts.
For the notebook, we have developed methods to estimate the number of farms experiencing losses due to extreme events as well as the percentage of revenue lost, while also considering the relationship of farm size and type of climate shock.

The key insights derived from this analysis are:
- The change in the number of farms by farm size class between 2020 and 2050
- The number extreme events (very wet or very dry) estimated both historically (1995-2015) and in 2040-2060 under SSP 245 and SSP 585
- The number of farms that will experience loss to very wet or very dry events as well as the percentage of revenue that will be lost by these farms by farm size class

Below the data and scripts need to create the final dataproduct to accompany the notebook are outlined.

## Dataset Description
Below is a description of the underlying datasets used in this analysis. 

#### Table 1: Datasets Used
| Dataset           | Description|
|-----------------|---------------------------|
Farm sizes | The estimates for the number of farms by farm size for Africa were estimated by Zia Mehrabi and more explanation on the methodology and underlying data sources can be found [here]( https://github.com/Better-Planet-Laboratory/africafarmprojections).|
|Loss and damage functions| The functions estimated to explain the relationship between farm size and either production loss or proportion of lost revenue were developed by Zia Mehrabi and the methodology and underlying datasets can be found [here](https://github.com/Better-Planet-Laboratory/farm-loss-farmsize). This work builds on previous dataset developed by Zia Mehrabi, Julie Fortin and Navin Ramankutty, which harmonized household survey data across 17 countries to examine the relationship between farm size and climate shock impacts.|
|Historical and CMIP6-derived SPEI| SPEI data was obtained from [Araujo et al. 2025]( https://www.nature.com/articles/s41597-025-04612-w) and included SPEI-12/SPEI-6 estimates for the historical period (1995-2015) as well as for 2040-2060 for SSP245 and SSP585. Five CMIP6 GCMs were selected for [improved performance in simulating precipitation and temperature over Africa]( https://doi.org/10.1007/s40808-025-02560-3) to develop an ensemble average, and included GFDL-ESM4, EC-Earth3, MPI-ESM1-HR, MRI-ESM2-0 and NorESM2-LM.|
|EM-DAT disaster database | The EM-DAT database contained data on over 27,000 disasters from 1900 to the present day and can be obtained [here]( https://www.emdat.be).|
|Cropland Maps | Cropland area fraction obtained from [Ramankutty et al. 2008](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2007GB002952) with data available [here](http://www.earthstat.org/cropland-pasture-area-2000/).
|Crop Calendars | Crop calendars were obtained from [Sacks et al. 2010](https://onlinelibrary.wiley.com/doi/10.1111/j.1466-8238.2010.00551.x) with data available [here](https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/).


## Methodology
We estimate the number of extreme climate events and their impacts on African farms by integrating climate, disaster, and agricultural datasets. Annual growing-season extremes of the Standardized Precipitation Evapotranspiration Index (SPEI) were computed at the subnational level across Africa for the historical period (1995–2015) and for future projections (2040–2060) under SSP245 and SSP585 scenarios, using ensemble means derived from five CMIP6 global climate models (GFDL-ESM4, EC-Earth3, MPI-ESM1-HR, MRI-ESM2-0, NorESM2-LM). Extreme events were defined as years when SPEI values exceeded ±1, corresponding to very wet or very dry conditions. These modeled events were validated against the EM-DAT disaster database to confirm that the SPEI-based approach captured the timing and spatial distribution of major historical droughts and floods. Once validated, the frequency of extreme events was used in combination with empirically derived loss functions and 2020-2050 farm projections linking farm size to production and revenue losses. For each administrative unit and farm size class, the probability of farms experiencing loss was estimated for each year and climate scenario, and corresponding revenue losses were calculated as proportional reductions in farm income. The outputs include both yearly averages and cumulative totals of farms and % of revenue affected, allowing comparison of historical and future exposure to extreme climate events across Africa.

#### Table 2: Scripts

|File | Description|
|----  | -----------|
|00_data_prep | Prepare data from [farm projections](https://github.com/Better-Planet-Laboratory/africafarmprojections) and [farm loss](https://github.com/Better-Planet-Laboratory/farm-loss-farmsize) repositories. |
|01_combine_spei | Combine monthly SPEI data for historical, ssp245 and ssp585 for all GCMs, get ensemble averages.|
|02_extract_spei | Extract growing season min/max SPEI from GCMs and ensemble average.|
|03_compare_spei_emdat | Get extreme SPEI values for each admin 1 unit, check historical ensemble averages against EMDAT to estimate coverage.|
|04_join_spei_predict | Join SPEI values to shapefiles, predict loss and lost revenue from loss and damage functions.|
|05_compute_loss_rev | Estimate loss and lost proportion of revenue by farm size for extreme events, calculate cumulative sums and yearly averages.|
|06_makedata_fig1_fig3 | Create additional files needed for figure 1 and figure 3 of the notebook.|

## Outputs
The outputs of this work include a [R markdown notebook “Climate payouts to smallholder farmers”](notebook/notebook_lossanddamage_spei.Rmd) as well as the supporting files needed, which can be found [here](https://o365coloradoedu-my.sharepoint.com/:f:/r/personal/zime6804_colorado_edu/Documents/BetterPlanetLab/Projects/Loss_and_damage/notebook_files?csf=1&web=1&e=OFWw5i).

This work will soon be featured as part of the African Adaptation Atlas, on which more detail can be found [here](https://adaptationatlas.cgiar.org). We will update once the notebook is live on that platform.

An [accompanying article on Medium]( https://medium.com/@ziamehrabi/b4fe5465a8a9) also delves further into the insights from this analysis.


![](test3.gif)

## Citation

If you use this dataset, please cite the DOI assigned to it from where you download it:

The general citation prior to DOI assignment was:

```
Mehrabi, Z., and Braich., B. 2025. Climate payouts. Better Planet Laboratory. 
```

## Acknowledgements

This work was supported by our ongoing partnership with the [CGIAR](https://www.cgiar.org/), and an interactive version will be distributed through the [CGIAR’s Africa Agriculture Adaptation Atlas](https://adaptationatlas.cgiar.org) in coming months.
