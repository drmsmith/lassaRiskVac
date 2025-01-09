# lassaRiskVac
Code for a model-based evaluation of risk-targeted Lassa vaccination campaigns in endemic regions of West Africa.

This code accompanies the article "Burden of Lassa fever and health-economic benefits of risk-targeted vaccination in endemic regions of Nigeria, Guinea, Liberia and Sierra Leone: a vaccine impact modelling study." A pre-print is available at <>.

This repository contains 8 directories and 18 R files, detailed below. URLs are provided for external data inputs that exceed Github's file size limitations and could not be uploaded. Datasets generated here that were too large to be uploaded can be reproduced by the user using the code below.

## Directories

### (D1) /Raster
- to store rasterised (.tif) geospatial gridded population estimates for included countries by age and sex, available at <https://hub.worldpop.org/geodata/listing?id=87>
- contains a subfolder with shapefiles from GADM

### (D2) /RasterOutput
- contains final estimates (.csv) of each country's population by age and sex and the proportion of each country's residents inhabiting in each district

### (D3) /parameters_data
- contains model input data; World Population Prospects data (projections of population size and life expectancy by sex and single year age for each country) are available at <https://population.un.org/wpp/>
- to store additional datasets generated in subsequent code, including the final Monte Carlo parameter set (df_params_econ_final.Rdata) and projections of future productive life years by age and sex (list_age_distr_employed_expected.Rdata).

### (D4) /demography
- contains population data for various years and stratified in various ways (e.g. by age group and sex)
 
### (D5) /vaccination
- contains vaccine demand forecasts (target population and # of doses required) and weekly dynamics of vaccine uptake in our rollout scenarios

### (D6) /infections
- contains 2019 zoonosis estimates in each district from Smith et al. (list_Lassa_00_byDistrict_annual.Rdata") and draws of FOI (foi_posterior_draws.csv) and seroprevalence (seroprevalence_posterior_draws.csv) from our serocatalytic modelling
- create subfolders to store the various infection projections calculated using infection R scripts
  
### (D7) /risk
- contains final estimates of symptom risk, IHR and CFR stratified by age, sex and pregnancy status

### (D8) /outcomes
- create subfolders to store: "raw" model output data (/raw_dat_ag) and output data stratified by different groupings (/organised_data_grouped)
- contains final summarised output data expressed as means and quantiles (/summarised_data) 


# R files

### (R1) housekeeping.R
- loads packages, variable definitions, colour palettes and other code bases used throughout this repository

### (R2) raster_worldpop.R
- generates UN-adjusted population data from gridded WorldPop data

### (R3) age_sex_projections.R
- generates population projections across the study setting and time horizon

### (R4) demography.R
- generates final demography datasets for model input

### (R5) vaccine_demand_forecast.R
- generates vaccine doses required for different vaccine rollout scenarios

### (R6) vaccine_scenarios.R
- generates weekly vaccine uptake data by region, age and sex for different vaccine rollout scenarios

### (R7) params_montecarlo.R
- generates parameter vectors for all health-related parameters varied in Monte Carlo simulation and used as model inputs

### (R8) params_econ.R
- generates economic parameter vectors across included countries for all economic parameters used as model inputs

### (R9) infections_foi.R
- generates age- and sex-stratified infection projections 

### (R10) infections_seasonal.R
- estimates infection seasonality and distributes projected infections at the weekly level

### (R11) infections_vaccine.R
- determines which infections are occurring in immunised vs. non-immunised individuals 

### (R12) infections_vaccine_plots.R
- visualises impacts of vaccination at the district level in one exemplar district

### (R13) risk_stratify.R
- estimates symptom risk, IHR and CFR stratified by age, sex and pregnancy status

### (R14) outcomes_econ.R
- main model code: loads in and processes all data sources and calculates model outcomes

### (R15) outcomes_econ_organise.R
- loads in "raw" outcomes one district at a time, re-structures the data to reduce dimensionality and saves by different stratifications

### (R16) outcomes_econ_summarise.R
- re-attributes neonatal outcomes to <2s, conducts multivariate sensitivity analysis (PRCC), and generates final summarised model outputs expressed as means and quantiles

### (R17) outcomes_econ_tables.R
- generates tables to display final summarised model outputs

### (R18) outcomes_econ_plots.R
- generates plots to visualise final summarised model outputs
