### lassaRiskVac
Code for a model-based evaluation of risk-targeted Lassa vaccination campaigns in endemic regions of West Africa.

This code accompanies the article "Burden of Lassa fever and health-economic benefits of risk-targeted vaccination in endemic regions of Nigeria, Guinea, Liberia and Sierra Leone: a vaccine impact modelling study." A pre-print is available at <>.

This repository contains 10 directories and 18 R files, detailed below. URLs are provided for data inputs that exceed Github's file size limitations and could not be uploaded.

### Directories

# (D1) /Raster
- to store rasterised (.tif) geospatial gridded population estimates for included countries by age and sex, available at <https://hub.worldpop.org/geodata/listing?id=87>, and a subfolder with shapefiles from GADM

# (D2) /RasterOutput
- contains final estimates (.csv) of each country's population by age and sex and the proportion of each country's residents inhabiting in each district

# (D3) /parameters_data
- contains model input data; World Population Prospects data (projections of population size and life expectancy by sex and single year age for each country) are available at <https://population.un.org/wpp/>. Additional datasets generated in subsequent code should also be stored here, including the final Monte Carlo parameter set (df_params_econ_final.Rdata) and projections of future productive life years by age and sex (list_age_distr_employed_expected.Rdata).

# (D4) /demography
- contains population data for various years and stratified in various ways (e.g. by age group and sex)
 
# (D5) /infections
- contains 2019 zoonosis estimates in each district from Smith et al. (list_Lassa_00_byDistrict_annual.Rdata") and draws of FOI (foi_posterior_draws.csv) and seroprevalence (seroprevalence_posterior_draws.csv) from our serocatalytic modelling, as well as subfolders to be filled with the various infection projections calculated using infection R scripts

# (D6) /risk

# (D7) /vaccination

# (D8) /outcomes

# (D9) /plots

# (D10) /tables


### R files

# (R1) housekeeping.R
- loads packages, variable definitions, colour palettes and other code bases used throughout this repository

# (R2) raster_worldpop.R
- generates UN-adjusted population data from gridded WorldPop data

# (R3) age_sex_projections.R
- generates population projections across the study setting and time horizon

# (R4) infections_foi.R
- generates age- and sex-stratified infection projections 

# (R5) .R

# (R6) .R

# (R7) .R

# (R8) .R

# (R9) .R

# (R10) .R

# (R11) .R

# (R12) .R

# (R13) .R

# (R14) .R

# (R15) .R

# (R16) .R

# (R17) .R

# (R18) .R
