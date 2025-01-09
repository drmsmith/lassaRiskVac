##################################
### LASSA VACCINE HOUSEKEEPING ###
##################################

################
### PACKAGES ###
################

library(tidyverse)
library(readxl)
library(scales)
library(ggsci)
library(cowplot)
library(ggpubr)
library(meta)
library(hesim)
library(metafor)
library(boot)
library(fitdistrplus)
library(metRology)
library(raster)
library(terra)         # For handling SpatVector objects
library(sf)            # For converting to sf objects
library(exactextractr) 
library(dplyr) 
library(tibble) 
library(purrr)
library(geodata)   
library(data.table)
library(mgcv)
library(openxlsx)
library(officer)
library(flextable)
library(purrr)
library(epiR)
library(corrplot)




###############################################
### DISCOUNT RATE AND NUMBER OF SIMULATIONS ###
###############################################

discRate = 0.035
n_draws_montecarlo = 500

####################
### FINAL GROUPS ###
####################

### Age FINAL
age_groups_final = c("<2",
                     "2-14",
                     "15-24",
                     "25-34",
                     "35-49",
                     "50+")

age_group_mins = c(0, 2, 15, 25, 35, 50)

### Age ENABLE
age_groups_enable = c("<5",
                     "5-17",
                     "18-24",
                     "25-34",
                     "35-49",
                     "50+")

age_group_enable_mins = c(0, 5, 18, 25, 35, 50)


### GID_0 (countries)
GID_0_final = c("NGA", "LBR", "SLE", "GIN")
countries_final = c("Nigeria", "Liberia", "Sierra Leone", "Guinea")

countries_cols = c("#008550", "#bf0a30", "#0072c6", "#fcd116")
countries_cols_map = c("Nigeria" = countries_cols[1], "Liberia" = countries_cols[2], 
                       "Sierra Leone" = countries_cols[3], "Guinea" = countries_cols[4])

GID_1_final = c("NGA.5_1", "NGA.7_1", "NGA.10_1", "NGA.11_1", "NGA.12_1", "NGA.14_1", 
                "NGA.16_1", "NGA.19_1", "NGA.23_1", "NGA.26_1", "NGA.29_1", "NGA.31_1", 
                "NGA.32_1", "NGA.35_1", "LBR.2_1", "LBR.5_1", "LBR.12_1", "GIN.8_1", 
                "SLE.1_1")

###########################
### GROUPS OF DISTRICTS ###
###########################

# medium and high burden districts
vec_GID_1_medDistricts = c("NGA.12_1", "NGA.29_1", "NGA.5_1", "NGA.11_1", "NGA.35_1")

# only high burden districts
vec_GID_1_highDistricts = c("NGA.12_1", "NGA.29_1")

###########################
### COLOURS AND FACTORS ###
###########################

### Sex cols
sex_cols = c("#377eb8", "#e41a1c")
sex_levels = c("male", "female")
sex_labels = c("Male", "Female")
sex_cols_map = c("Male" = sex_cols[1], "Female" = sex_cols[2])

### Sex cols with pregnant status
sex_preg_cols = c("#377eb8", "#e41a1c", "#ff7f00")
sex_preg_levels = c("Male", "Female_NotPreg", "Female_Preg")
sex_preg_labels = c("Male", "Female (not pregnant)", "Female (pregnant)")
sex_preg_cols_map = c("Male" = sex_preg_cols[1], 
                      "Female (not pregnant)" = sex_preg_cols[2], 
                      "Female (pregnant)" = sex_preg_cols[3])


### Age group cols
# ag_cols = pal_lancet()(9)
# ag_cols = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')
ag_cols = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628', '#f781bf')
ag_levels = age_groups_final
ag_cols_map = c("<2" = ag_cols[1], "2-14" = ag_cols[2], 
                "15-24" = ag_cols[3], "25-34" = ag_cols[4], 
                "35-49" = ag_cols[5], "50+" = ag_cols[6])

### Pregnany status cols
preg_cols = c("#a6761d", "#e7298a")
preg_levels = c("WCBA", "PW")
preg_cols_map = c("WCBA" = preg_cols[1], "PW" = preg_cols[2])


### Outcome/DALY colours
#outcome_cols = c("#4c72ae", "#e89600", "#c73e32", "#8f0e4d", "#5f2457", "#007492", "#e3cb6e")
outcome_cols = c("#8ca8d4", "#ffc354", "#e78279", "#bd5e8d", "#9a6f94", "#54aabf", "#fbeaa5")
outcome_levels = c("N_infection", "N_cases", "N_snhl", "N_hospital", "N_death", "N_fl", "N_nnd")
outcome_labels = c("Infection", "Acute symptoms", "Hearing loss", "Hospitalisation", "Death", "Foetal loss", "Neonatal death")
outcome_cols_map = c("Infection" = outcome_cols[1],
                     "Acute symptoms" = outcome_cols[2],
                     "Hearing loss" = outcome_cols[5],
                     "Hospitalisation" = outcome_cols[4],
                     "Death" = outcome_cols[3],
                     "Foetal loss" = outcome_cols[6],
                     "Neonatal death" = outcome_cols[7])

daly_levels = c("DALY_acute", "DALY_snhl", "DALY_death", "DALY_total")
daly_labels = c("Acute symptoms", "Hearing loss", "Death", "Total")
daly_cols_map = c("Acute symptoms" = outcome_cols[2],
                  "Hearing loss" = outcome_cols[5],
                  "Death" = outcome_cols[3],
                  "Foetal loss" = outcome_cols[6],
                  "Total" = outcome_cols[1])

### Costs

### Societal costs
soc_cost_cols = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')
soc_cost_levels = c("Cost_care_gvt", "Cost_care_oop", 
                "Cost_prod_fever", "Cost_prod_hospital", "Cost_prod_snhl", "Cost_prod_death",
                "Cost_societal")
soc_cost_labels = c("Healthcare costs\n(reimbursed)", "Healthcare costs\n(OOP)", 
                "Productivity losses\n(mild Lassa fever)", "Productivity losses\n(severe Lassa fever)", 
                "Productivity losses\n(hearing loss)", "Productivity losses\n(death)",
                "Total societal costs")

soc_cost_cols_map = c("Healthcare costs\n(reimbursed)" = soc_cost_cols[1], 
                  "Healthcare costs\n(OOP)" = soc_cost_cols[2], 
                  "Productivity losses\n(mild Lassa fever)" = soc_cost_cols[3],
                  "Productivity losses\n(severe Lassa fever)" = soc_cost_cols[4],
                  "Productivity losses\n(hearing loss)"= soc_cost_cols[5],
                  "Productivity losses\n(death)"  = soc_cost_cols[6],
                  "Total societal costs" = soc_cost_cols[7])


### Vaccine strategy
strategy_cols = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3')
strategy_levels = c("children", "wcba", "adults", "elderly")
strategy_labels = c("Children (2-14)", "WCBA (15-49)", "Adults (15-49)", "Older adults (50+)")
strategy_cols_maps = c("Children (2-14)" = strategy_cols[1],
                       "WCBA (15-49)" = strategy_cols[2],
                       "Adults (15-49)" = strategy_cols[3],
                       "Older adults (50+)" = strategy_cols[4])

vacc_first_day = 91
vacc_last_day = 304

### Cost totals
cost_total_disc_cols = c('#1f78b4','#a6cee3','#33a02c','#b2df8a','#e31a1c','#fb9a99')
cost_total_disc_levels = c("Cost_DALY_total_noFL", "Cost_DALY_total_noFL_disc", "Cost_societal", "Cost_societal_disc", "Cost_VSL", "Cost_VSLY")
cost_total_disc_labels = c("Monetised DALYs", "Monetised DALYs (discounted)", "Societal costs", "Societal costs (discounted)", "VSL", "VSLY")
cost_total_disc_map = c("Monetised DALYs" = cost_total_disc_cols[1],
                        "Monetised DALYs (discounted)" = cost_total_disc_cols[2],
                        "Societal costs" = cost_total_disc_cols[3],
                        "Societal costs (discounted)" = cost_total_disc_cols[4],
                        "VSL" = cost_total_disc_cols[5],
                        "VSLY" = cost_total_disc_cols[6])

### Vaccine efficacy
VE_cols = c('#9ebcda','#8c96c6','#8c6bb1','#810f7c','#4d004b')
VE_cols = c('#ffffcc','#c7e9b4','#7fcdbb','#41b6c4','#225ea8')
VE_levels = 1:5
VE_labels = c("50% mild, 50% severe", "50% mild, 70% severe", "70% mild, 70% severe", "70% mild, 90% severe", "90% mild, 90% severe")
VE_cols_map = c("50% mild, 50% severe" = VE_cols[1], 
           "50% mild, 70% severe" = VE_cols[2], 
           "70% mild, 70% severe" = VE_cols[3], 
           "70% mild, 90% severe" = VE_cols[4], 
           "90% mild, 90% severe" = VE_cols[5])

#############################
### VACCINE TARGET GROUPS ###
#############################

# time horizon of vaccination
n_years_horizon = 13
year_start = 2025
year_end = 2037

vec_wcba_sex = c("Female")
vec_wcba_ages = 15:49

vec_adults_sex = c("Male", "Female")
vec_adults_ages = 15:49

vec_children_sex = c("Male", "Female")
vec_children_ages = 2:14

vec_elderly_sex = c("Male", "Female")
vec_elderly_ages = 50:80

#######################
### HANDY FUNCTIONS ###
#######################

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#################
### PREGNANCY ###
#################

### what share of the year is a woman pregnant (for determining outcomes from annual pregnancy proportion)
duration_pregnant = 40/52
