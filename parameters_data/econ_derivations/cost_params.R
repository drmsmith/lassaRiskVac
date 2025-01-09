#################################
### PREPARING COST PARAMETERS ###
#################################

####################
### SET FILEPATH ###
####################

filepath = "C:/Users/davidsm/Desktop/lassa_vaccination_extension/model/"
setwd(filepath)

########################
### RUN HOUSEKEEPING ###
########################

source("housekeeping.R")

render_plots = T

#######################
### TREATMENT COSTS ###
#######################

############################
### ESTIMATES FROM IRRUA ###
############################

# Background
# Study: Medical cost of lassa fever treatment in Irrua specialist teaching hospital, Nigeria
# Asogun et al., 2016
# Cost estimates provided in local currency 2016 values
# Average total Lassa treatment costs: N 205,559
# Average total Lassa out-of-pocket costs: N 86,803

treat_lassa_hospital_naira_2016 = 205559
treat_lassa_hospital_naira_2016_OOP = 86803

# Method for Cost Scaling to get country-specific cost-estimates
# Method based on tradable resources (see method 3: Turner et al, 2019; VALUE HEALTH. 2019; 22(9):1026???1032)
# 1. Take original value expressed in local currency
# --> Avg Lassa treatment cost: N 205,559
# 2. Convert to I$ with the exchange rate at the time of the costing (2016)
# Note: WB data used (file name: PPP_conv_factor.xlsx)
PPP_conv_factor <- read.csv("parameters_data/PPP_conv_factor.csv")
names(PPP_conv_factor)
PPP_conv_factor_NGA = PPP_conv_factor %>% filter(Country.Code == "NGA")%>%dplyr::select(X2016..YR2016.)%>%as.numeric()

# --> PPP I$ conversion factor 2016 for NGA: 104.5
# --> 205559 / 104.5 = 1967.07 I$

treat_lassa_hospital_dollar_2016 = treat_lassa_hospital_naira_2016/PPP_conv_factor_NGA
treat_lassa_hospital_dollar_2016_OOP = treat_lassa_hospital_naira_2016_OOP/PPP_conv_factor_NGA


# 3. Inflate using the US$ inflation rate (divide GDP deflator)
# Note: WB data used 
GDP_deflator <- read.csv("parameters_data/GDP_deflator.csv", skip = 4)
GDP_deflator_USA = GDP_deflator %>% filter(Country.Code == "USA")

# --> GDP_2023 / GDP_2016 = 125.6456  / 100.9503  = 1.244628
GDP_deflator_dollars_2016_2023 = GDP_deflator_USA["X2023"]/GDP_deflator_USA["X2016"]

### Calculate treatment cost in 2023
# --> 1967.07 I$ * 1.244628 InflRate = 2,448.273 $I
treat_lassa_hospital_dollar_2023 <- treat_lassa_hospital_dollar_2016*GDP_deflator_dollars_2016_2023
treat_lassa_hospital_dollar_2023_OOP <- treat_lassa_hospital_dollar_2016_OOP*GDP_deflator_dollars_2016_2023

# WB data on healthcare expenditures in Nigeria:
# 1) Information on current per capita healthcare expenditure
df_expend_healthcare_PPP = read.csv("parameters_data/current_exp_healthcare_PPP.csv", skip = 4)  
df_expend_healthcare_PPP_NGA = df_expend_healthcare_PPP%>%filter(Country.Code == "NGA")

# Per Capita Healthcare Expenditure I$ PPP in 2016: 188.2079 I$
# 2) Information on share of OOP expenditure

df_expend_healthcare_proportion = read.csv("parameters_data/current_exp_healthcare_proportion.csv", skip = 4)
df_expend_healthcare_proportion_NGA = as.numeric(df_expend_healthcare_proportion%>%filter(Country.Code == "NGA")%>%dplyr::select("X2021"))

# Proportion of per Capita Healthcare Exp paid OOP I$ PPP in 2021: 76.24337     (latest available estimate)
# 3) OOP expenditure adjustment factor:
# 1) What is the proportion of OOP paid in Nigeria study?
# 926.2541 I$ / 2193.471 $I = 0.4222778 (42.2%)

prop_oop_irrua = treat_lassa_hospital_dollar_2016_OOP/treat_lassa_hospital_dollar_2016

# 2) Develop an adjustment factor using the study OOP proportion and the WB OOP proportion
# 42.2% / 76.24337 % = 0.5538551
adjustment_factor <- prop_oop_irrua/(df_expend_healthcare_proportion_NGA/100)


############################################
### Scaling of costs to other countries #### 
############################################

# Share of OOP payments of total current healthcare expenditure WB data

# Only keep latest estimate (2019)
scaling_master <- df_expend_healthcare_proportion %>% dplyr::select(Country.Name, Country.Code, X2021)%>%
  rename(Country = Country.Name, 
         GID_0 = Country.Code,
         OOP2021 = X2021)%>%
  filter(Country %in% countries_final)%>%
  # Adding estimated values from section (4)
  mutate(treat_lassa_hospital_dollar_2023 = as.numeric(treat_lassa_hospital_dollar_2023),
         treat_lassa_hospital_dollar_2023_OOP = as.numeric(treat_lassa_hospital_dollar_2023_OOP),
         adjustment_factor = adjustment_factor)%>%
  # Estimating country-specific OOP expenditures
  mutate(countr_specific_OOP_2023IntD = treat_lassa_hospital_dollar_2023 * (OOP2021/100) * adjustment_factor)


# Saving data set #### 
save(scaling_master, file='scaling_master.Rda')


#######################
### MONETISED DALYS ###
#######################

### GDP per Capita (2023 current US$) and creation of master linkage data ####

# GDP per capita (current US$) data from World Bank
# https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
gdp_pc_2023 <- read.csv("parameters_data/GDP_pC.csv", skip = 4)%>%
  rename(Country = Country.Name,
         GID_0 = Country.Code,
         GDP_pc2023 = X2023)%>%
  filter(Country %in% countries_final)%>%
  dplyr::select(Country, GID_0, GDP_pc2023)

# DALY value as percentage of GDP per capita based on Method 4 of:
# Ochalek, Jessica, James Lomas, and Karl Claxton.
# "Estimating health opportunity costs in low-income and middle-income countries:
# a novel approach and evidence from cross-country data." BMJ global health 3.6 (2018): e000964.


daly_estimates <- read_excel("parameters_data/DALY_for_R.xlsx")%>%
  rename(Country = NAME_0)%>%
  filter(Country %in% countries_final)%>%
  mutate(across(US_2015, ~gsub("\\$", "", .)))%>%
  mutate(US_2015 = as.numeric(US_2015))

# For missing data (Liberia)
### this is the median from the full dataset from previous analysis
median_perc <- 0.36

daly_master = gdp_pc_2023%>%
  left_join(., daly_estimates, by = c("Country"))%>%
  mutate(daly_value = case_when(Country == "Liberia" ~ median_perc * GDP_pc2023,
                                T ~ perc_of_GDPpc * GDP_pc2023))

### SAVE DATA ###

save(daly_master, file='daly_master.rda')


#################################
### VALUE OF STATISTICAL LIFE ###
#################################

### Take baseline VSL IN USA (12,300,000) (where does this come from?)
vsl_usa = 12300000

### MULTIPLY BY QUOTIENT OF TARGET COUNTRY GNI TO USA GNI, AND FURTHER DIVIDE BY 1.5 TO ACCOUNT FOR 
### INCOME ELASTICITY IN LOW INCOME COUNTRISE

### LOAD GNI
df_GNI = read.csv("parameters_data/GNI_PPP.csv", skip = 4, header = T)%>%
  filter(Country.Name %in% countries_final)%>%
  dplyr::select(Country.Name, Country.Code, X2023)%>%
  rename(Country = Country.Name,
         GID_0 = Country.Code,
         GNI = X2023)%>%
  mutate(GNI_daily = GNI/365.25)

### need this for productivity as well, save data
save(df_GNI, file='df_GNI.rda')

### USA GNI
df_GNI_USA = read.csv("parameters_data/GNI_PPP.csv", skip = 4, header = T)%>%
  filter(Country.Name %in% "United States")%>%
  dplyr::select(Country.Name, Country.Code, X2023)%>%
  rename(Country = Country.Name,
         GID_0 = Country.Code,
         GNI = X2023)%>%
  mutate(GNI_daily = GNI/365.25)

GNI_USA = df_GNI_USA$GNI

### VSL for target countries
vsl_master = df_GNI%>%
  mutate(vsl_estimate_2023_IntDoll = vsl_usa * (GNI/GNI_USA) /1.5 )

### SAVE DATA ###
save(vsl_master, file='vsl_master.rda')


##########################################################
### RISK OF CATASTROPHIC AND IMPOVERISHING EXPENDITURE ###
##########################################################

### national poverty lines 2017 PPP from here: World Bank Data 2022, Jolliffe et al (2022). 
### Assessing the Impact of the 2017 PPPs on the International Poverty Line and Global Poverty. Washington, DC: World Bank

### proportion of population below national poverty line, using https://pip.worldbank.org/

### Guinea:
nat_pov_line_gin = 3.40
prop_below_nat_pov_line_gin = 0.4370 # in 2018

### Sierra Leone: 
nat_pov_line_sle = 3.24
prop_below_nat_pov_line_sle = 0.5680 # in 2018

### Liberia: 
nat_pov_line_lbr = 3.13
prop_below_nat_pov_line_lbr = 0.5090

### Nigeria:
nat_pov_line_nga = 2.52
prop_below_nat_pov_line_nga = 0.4010 #in 2018


### WHAT IS CATASTROPHIC HEALTH EXPENDITURE IN EACH COUNTRY? ###


# Example Nigeria:
# OOP is $1034 in 2021 PPP$
# Catastrophic OOP HC expenditure defined as >10% if income
# This means an assumed income of 10 * $1034 = $10340, or $28.3 per day (10340 / 365.25)
# Use World Bank PIP to look up number of people living below $28.3 a day

### Income distributions on WorldBank PIP from 2017 so deflate OOP expenditure to 2017

poverty_master = scaling_master%>%
  mutate(countr_specific_OOP_2017IntD = countr_specific_OOP_2023IntD)%>%
  dplyr::select(Country, GID_0, countr_specific_OOP_2017IntD)%>%
  mutate(income_threshold = countr_specific_OOP_2017IntD*10/365.25)

### Guinea threshold: $16.2 (PIP rounds to $15),  99.35% of the population
prop_cat_gin = 0.9935

### Liberia threshold: $7.5, 94.91% of the population
prop_cat_lbr = 0.9491

### Nigeria threshold: $23.2 (PIP rounds to $25),  99.89% of the population
prop_cat_nga = 0.9989

### Sierra Leone threshold: $15.6 (PIP rounds to $15), 98.34% of the population 
prop_cat_sle = 0.9834

# Estimating country-specific dat rate
poverty_master = poverty_master%>%
  mutate(prop_at_risk_catastrp_HCexp = case_when(GID_0 == "GIN" ~ prop_cat_gin,
                                                 GID_0 == "LBR" ~ prop_cat_lbr,
                                                 GID_0 == "NGA" ~ prop_cat_nga,
                                                 GID_0 == "SLE" ~ prop_cat_sle,
                                                 T ~ NA),
         prop_below_nat_pov_line = case_when(GID_0 == "GIN" ~ prop_below_nat_pov_line_gin,
                                                 GID_0 == "LBR" ~ prop_below_nat_pov_line_lbr,
                                                 GID_0 == "NGA" ~ prop_below_nat_pov_line_nga,
                                                 GID_0 == "SLE" ~ prop_below_nat_pov_line_sle,
                                                 T ~ NA),
         prop_at_risk_impoverishing = prop_at_risk_catastrp_HCexp - prop_below_nat_pov_line)


### SAVE DATA ###
save(poverty_master, file='poverty_master.rda')
