# Climate payouts to smallholder farmers
## Overview
This repository contains statistical analysis to develop the “Climate payouts to smallholder farmers” notebook for African Adaptation Atlas. The analysis builds on [work]( https://github.com/Better-Planet-Laboratory/africafarmprojections) from Zia Mehrabi that developed African farm size distributions from 2000-2050 as well as [analysis]( https://github.com/Better-Planet-Laboratory/lossdamage) from Zia Mehrabi, Julie Fortin and Navin Ramankutty that harmonizes household survey data to examine the relationship between farm size and climate shock impacts.
For the notebook, we have developed methods to estimate the number of farms experiencing losses due to extreme events as well as the percentage of revenue lost, while also considering the relationship of farm size and type of climate shock.

The key insights derived from this analysis are:
- The change in the number of farms by farm size class between 2020 and 2050
- The number extreme events (very wet or very dry) estimated both historically (1995-2015) and in 2040-2060 under SSP 245 and SSP 585
- The number of farms that will experience loss to very wet or very dry events as well as the percentage of revenue that will be lost by these farms by farm size class

Below the data and scripts need to create the final dataproduct to accompany the notebook are outlined.

## Dataset Description
Below is a description of the underlying datasets used in this analysis. 

Table 1: Datasets Used
| Dataset           | Description|
|-----------------|---------------------------|
Farm sizes | The estimates for the number of farms by farm size for Africa were estimated by Zia Mehrabi and more explanation on the methodology and underlying data sources can be found [here]( https://github.com/Better-Planet-Laboratory/africafarmprojections).|
|Loss and damage functions| The functions estimated to explain the relationship between farm size and either production loss or proportion of lost revenue were developed by Zia Mehrabi and the methodology and underlying datasets can be found [here]( https://github.com/Better-Planet-Laboratory/lossdamage). This work builds on previous [analysis]( https://github.com/Better-Planet-Laboratory/microshock) developed by Zia Mehrabi, Julie Fortin and Navin Ramankutty, which harmonized household survey data across 17 countries to examine the relationship between farm size and climate shock impacts.|
|Historical and CMIP-derived SPEI| SPEI data was obtained from [Araujo et al. 2025]( https://www.nature.com/articles/s41597-025-04612-w) and included SPEI-12 estimates for the historical period (1995-2015) as well as for 2040-2060 for SSP245 and SSP585. Five CMIP6 GCMs were selected for [improved performance in simulating precipitation and temperature over Africa]( https://doi.org/10.1007/s40808-025-02560-3) to develop an ensemble average, and included GFDL-ESM4, EC-Earth3, MPI-ESM1-HR, MRI-ESM2-0 and NorESM2-LM.|
|EM-DAT disaster database | The EM-DAT database contained data on over 27,000 disasters from 1900 to the present day and can be obtained [here]( https://www.emdat.be).

## Methodology
To estimate loss and proportion of lost revenue from farms, both currently and into the future we first developed yearly growing season estimate of SPEI at the admin1 level in Africa for the historical, SSP245 and SSP585 scenarios.
The number of estimated events was compared for both the GCMs and ensemble average to the EM-DAT disaster database to confirm that the derived seasonal SPEI values were capturing at least some major historical flood and droughts in Africa.
The total number of extreme events (SPEI>1 or SPEI<-1) were computed for each year in each admin 1 unit for the historical, SSP245 and SSP585 scenarios, both for ensemble averages and all GCMs. 
The number of farms likely to experience loss was calculated using the loss and damage functions to estimate the % of loss by farm size class when that location had an extreme event. Similarly, proportion of lost revenue was calculated using the loss and damage function and the previously estimated farms likely experiencing a loss. These resulting insights are both a yearly average and cumulative sum of the either the number of farms experiencing a loss or the proportion of their revenue that is lost for each farm size class and admin 1 unit in Africa under either a historical, SSP245 or SSP585 scenario.
Table 2: Scripts
File | Description
----  | -----------
1. Combine_spei | Combine monthly SPEI data for historical, ssp245 and ssp585 for all GCMs, get ensemble averages
2. Extract_spei | Extract growing season min/max SPEI from GCMs and ensemble average
3. Compare_spei_emdat | Get extreme SPEI values for each admin 1 unit, check historical ensemble averages against EMDAT to estimate coverage
4. Join_spei_predict | Join SPEI values to shapefiles, predict loss and lost revenue from loss and damage functions
5. Compute_loss_rev | Estimate loss and lost proportion of revenue by farm size for extreme events, calculate cumulative sums and yearly averages

## Outputs
The outputs of this work include a R markdown notebook “Climate payouts to smallholder farmers” as well as the supporting files needed, which can be found here.
This work will soon be featured as part of the African Adaptation Atlas, on which more detail can be found [here](https://adaptationatlas.cgiar.org). We will update once the notebook is live on that platform.
An [accompanying article on Medium]( https://medium.com/@ziamehrabi/b4fe5465a8a9) also delves further into the insights from this analysis.


