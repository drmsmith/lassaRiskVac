###########################
### ECONOMIC PARAMETERS ###
###########################

### NB: need to run params_montecarlo.R first
### as years left in labour force given snhl depends on duration of snhl

####################
### SET FILEPATH ###
####################

filepath = this.path::here()
setwd(filepath)

########################
### RUN HOUSEKEEPING ###
########################

source("housekeeping.R")

render_plots = T

generate_employment_data = F

######################
### LOAD SNHL DATA ###
######################

df_params_snhl = loadRData("parameters_data/params_montecarlo.Rdata")%>%
  dplyr::select(c(n_draw, dur_snhl))

##################################################
### COUNTRY-, AGE- AND SEX-SPECIFIC PARAMETERS ###
##################################################

# demography
files_demography = list.files("demography/")
for(file_i in files_demography){load(paste0("demography/",file_i))}


##################################
### LABOUR FORCE PARTICIPATION ###
##################################

# https://ilostat.ilo.org/data/

### LOAD AND CALCULATE COUNTRY-AGE-SEX PARTICIPATION RATES ###
vec_ag_labour = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+")
vec_age_bands_labour = paste0("Age (5-year bands): ", vec_ag_labour)

vec_sex_bands_labour = paste0("Sex: ", c("Male", "Female"))

df_labour_force = read.csv("parameters_data/labour_force.csv")

df_labour_force%>%filter(ref_area.label == "Nigeria")

df_labour_force_filtered = df_labour_force%>%
  filter(ref_area.label %in% countries_final,
         classif1.label %in% vec_age_bands_labour,
         sex.label %in% vec_sex_bands_labour)%>%
  rename(Country = ref_area.label,
         ag_ilo = classif1.label,
         Sex = sex.label,
         EmploymentRate = obs_value)%>%
  mutate(Sex = factor(Sex,
                      levels = vec_sex_bands_labour,
                      labels = c("Male", "Female")),
         ag_ilo = factor(ag_ilo, 
                         levels = vec_age_bands_labour,
                         labels = vec_ag_labour),
         EmploymentRate = EmploymentRate/100)

### Filter specific data for each country and add non-participation for youngest group
df_labour_force_LBR = df_labour_force_filtered%>%filter(Country == "Liberia", time == 2017)%>%
  dplyr::select(Country, ag_ilo , Sex, EmploymentRate)%>%
  bind_rows(data.frame(Country = "Liberia", ag_ilo = "<15", Sex = c("Male", "Female"), EmploymentRate = 0))
df_labour_force_GIN = df_labour_force_filtered%>%filter(Country == "Guinea", time == 2019)%>%
  dplyr::select(Country, ag_ilo , Sex, EmploymentRate)%>%
  bind_rows(data.frame(Country = "Guinea", ag_ilo = "<15", Sex = c("Male", "Female"), EmploymentRate = 0))
df_labour_force_NGA = df_labour_force_filtered%>%filter(Country == "Nigeria", time == 2019)%>%
  dplyr::select(Country, ag_ilo , Sex, EmploymentRate)%>%
  bind_rows(data.frame(Country = "Nigeria", ag_ilo = "<15", Sex = c("Male", "Female"), EmploymentRate = 0))
df_labour_force_SLE = df_labour_force_filtered%>%filter(Country == "Sierra Leone", time == 2018)%>%
  dplyr::select(Country, ag_ilo , Sex, EmploymentRate)%>%
  bind_rows(data.frame(Country = "Sierra Leone", ag_ilo = "<15", Sex = c("Male", "Female"), EmploymentRate = 0))

df_labour_force_final = bind_rows(df_labour_force_LBR,
                                  df_labour_force_GIN,
                                  df_labour_force_NGA,
                                  df_labour_force_SLE)

### UPATE AGE GROUPS ###

# Define the mapping of hospital age groups to individual ages
age_mapping_working <- list(
  "<15"  = 0:14,
  "15-19" = 15:19,
  "20-24" = 20:24,
  "25-29" = 25:29,
  "30-34" = 30:34,
  "35-39" = 35:39,
  "40-44" = 40:44,
  "45-49" = 45:49,
  "50-54" = 50:54,
  "55-59" = 55:59,
  "60-64" = 60:64,
  "65+" = 65:80  # 80 is max in underlying population data
)

# Define minimum age for each cfr age bracket
working_group_mins = c(age_mapping_working[[1]][1],
                       age_mapping_working[[2]][1],
                       age_mapping_working[[3]][1],
                       age_mapping_working[[4]][1],
                       age_mapping_working[[5]][1],
                       age_mapping_working[[6]][1],
                       age_mapping_working[[7]][1],
                       age_mapping_working[[8]][1],
                       age_mapping_working[[9]][1],
                       age_mapping_working[[10]][1],
                       age_mapping_working[[11]][1],
                       age_mapping_working[[12]][1])

# Define full population age groups following working age brackets
df_population_finalAges_adjustWorking = df_population%>%
  filter(Year == 2025)%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, Age, UN_scaled_subnational )%>%
  filter(Sex %in% c("Male", "Female"))%>%
  mutate(ag_ilo = case_when(Age >= working_group_mins[1] & Age < working_group_mins[2] ~ names(age_mapping_working[1]),
                            Age >= working_group_mins[2] & Age < working_group_mins[3] ~ names(age_mapping_working[2]),
                            Age >= working_group_mins[3] & Age < working_group_mins[4] ~ names(age_mapping_working[3]),
                            Age >= working_group_mins[4] & Age < working_group_mins[5] ~ names(age_mapping_working[4]),
                            Age >= working_group_mins[5] & Age < working_group_mins[6] ~ names(age_mapping_working[5]),
                            Age >= working_group_mins[6] & Age < working_group_mins[7] ~ names(age_mapping_working[6]),
                            Age >= working_group_mins[7] & Age < working_group_mins[8] ~ names(age_mapping_working[7]),
                            Age >= working_group_mins[8] & Age < working_group_mins[9] ~ names(age_mapping_working[8]),
                            Age >= working_group_mins[9] & Age < working_group_mins[10] ~ names(age_mapping_working[9]),
                            Age >= working_group_mins[10] & Age < working_group_mins[11] ~ names(age_mapping_working[10]),
                            Age >= working_group_mins[11] & Age < working_group_mins[12] ~ names(age_mapping_working[11]),
                            Age >= working_group_mins[12] ~ names(age_mapping_working[12])))

### Calculate years left to work as the sum of the probability of working each remaining year of life
df_population_working_allAges = df_population_finalAges_adjustWorking%>%
  left_join(., df_labour_force_final, by = c("Country", "Sex", "ag_ilo"))%>%
  dplyr::select(-UN_scaled_subnational)%>%
  cross_join(data.frame(Year = as.character(2025:2037)))


#####################################
### Life expectancy distributions ###
#####################################

### LOAD MALE AND FEMALE PROJECTED LIFE EXPECTANCY
df_age_distr = loadRData(paste0("demography/df_life_exp_age_sex_year.Rdata"))%>%
  rename(age_numeric = Age_num)%>%
  dplyr::select(Country, GID_0, Year, Sex, age_numeric, life_exp_at_age_x)%>%
  rename(Age = age_numeric)%>%
  filter(Age <= 80)

# add life expectancy at birth variable (for neonatal death losses)
df_age_distr_nnd = df_age_distr%>%
  filter(Age == 0)%>%
  rename(life_exp_at_birth = life_exp_at_age_x)%>%
  dplyr::select(-c(Age))



#########################################################################################
### COMBINE COUNTRY, AGE- AND SEX-SPECIFIC PARAMETERS, AND CALCULATE YLL AND YLL_disc ###
#########################################################################################

### Employment data is at national level, so extract for one district from each country
# and treat this as national data

### Update dur_snhl so that it never exceeds remaining life expectancy

df_age_distr_employed = df_age_distr%>%
  left_join(df_population_working_allAges,
            by = c("Country", "GID_0", "Year", "Sex", "Age"))%>%
  left_join(., df_age_distr_nnd, by = c("Country", "GID_0", "Year", "Sex"))%>%
  filter(GID_1 %in% c("GIN.8_1", "LBR.12_1", "NGA.10_1", "SLE.1_1"))%>%
  dplyr::select(-c(Region, GID_1))%>%
  cross_join(., df_params_snhl)%>%
  mutate(dur_snhl = ifelse(dur_snhl > life_exp_at_age_x, life_exp_at_age_x, dur_snhl),
         future_life_years = life_exp_at_age_x,
         future_life_years_disc = (1/discRate)*(1-exp(-discRate*life_exp_at_age_x)),
         future_life_years_nnd = life_exp_at_birth,
         future_life_years_nnd_disc = (1/discRate)*(1-exp(-discRate*life_exp_at_birth)),
         future_life_years_snhl = dur_snhl,
         future_life_years_snhl_disc = (1/discRate)*(1-exp(-discRate*dur_snhl)))%>%
  as.data.table()


### check dur_snhl never exceeds life expectancy, discounted and undiscounted
stopifnot(sum(df_age_distr_employed$dur_snhl > df_age_distr_employed$future_life_years) == 0)
stopifnot(sum(df_age_distr_employed$future_life_years_snhl_disc > df_age_distr_employed$future_life_years_disc) == 0)

####################################################################
### Expected participation in labour force until life expectancy ###
####################################################################

### Use purrr to mutate in a function where we calculate future work years by,
### at each age (row), extracting the remaining life expectancy and defining the corresponding max row,
### and then summing the employment rates over valid rows
### (accounting for sub-annual remaining life expectancy, i.e. employment over 2.4 years, not rounding to 2)
### then also incorporate discrete-time discounting for future years of work relative to time of event

list_age_distr_employed_expected = list()

if(generate_employment_data){
  
  for(draw_i in 1:n_draws_montecarlo){
    
    print(paste0("currently on draw ", draw_i, " of ", n_draws_montecarlo))
    
    df_age_distr_employed_expected_i = df_age_distr_employed%>%
      filter(n_draw == draw_i)%>%
      group_by(Country, GID_0, Sex, Year, n_draw)%>%
      mutate(future_work_years = map(row_number(), ~ {
        current_row <- .x
        current_rle <- floor(life_exp_at_age_x[current_row])
        current_er <- EmploymentRate[current_row]
        max_row <- current_row + current_rle - 1
        current_rle_remainder = abs(life_exp_at_age_x[current_row] - floor(life_exp_at_age_x[current_row]))
        
        valid_rows <- current_row:min(max_row, n())
        
        ### Future work years
        future_work_years <- ifelse(max_row > n(), 
                                    current_er*(current_rle+current_rle_remainder), 
                                    sum(EmploymentRate[valid_rows]) + current_rle_remainder*EmploymentRate[max_row])
        
        ### Future work years with discrete time discounting
        df_future_work_years_disc <- data.frame(num = 1:length(valid_rows),
                                                er = EmploymentRate[valid_rows])%>%
          mutate(future_work_years_disc = er/(1+discRate)^(num-1))
        
        er_disc <- sum(df_future_work_years_disc$future_work_years_disc)
        er_disc_remainder <- current_rle_remainder*EmploymentRate[max_row]/(1+discRate)^(length(valid_rows))
        
        future_work_years_disc = ifelse(max_row > n(), 
                                        (1/discRate)*current_er*(1-exp(-(current_rle+current_rle_remainder)*discRate)), 
                                        er_disc + er_disc_remainder)
        
        ### Future work years with SNHL
        current_snhl <- floor(dur_snhl[current_row])
        
        max_row_snhl <- current_row + current_snhl - 1
        current_snhl_remainder = abs(dur_snhl[current_row] - floor(dur_snhl[current_row]))
        
        valid_rows_snhl <- current_row:min(max_row_snhl, n())
        
        future_work_years_snhl = ifelse(max_row_snhl > n(), 
                                        current_er*(current_snhl+current_snhl_remainder), 
                                        sum(EmploymentRate[valid_rows_snhl]) + current_snhl_remainder*EmploymentRate[max_row_snhl])
        
        ### Future work years with SNHL, discounted
        df_future_work_years_snhl_disc <- data.frame(num = 1:length(valid_rows_snhl),
                                                     er_snhl = EmploymentRate[valid_rows_snhl])%>%
          mutate(future_work_years_snhl_disc = er_snhl/(1+discRate)^(num-1))
        
        er_snhl_disc <- sum(df_future_work_years_snhl_disc$future_work_years_snhl_disc)
        er_snhl_disc_remainder <- current_snhl_remainder*EmploymentRate[max_row_snhl]/(1+discRate)^(length(valid_rows_snhl))
        
        future_work_years_snhl_disc = ifelse(max_row > n(), 
                                             (1/discRate)*current_er*(1-exp(-(current_snhl+current_snhl_remainder)*discRate)), 
                                             er_snhl_disc + er_snhl_disc_remainder)
        
        ### Combine final results in list
        tibble(future_work_years = future_work_years, 
               future_work_years_disc = future_work_years_disc,
               future_work_years_snhl = future_work_years_snhl,
               future_work_years_snhl_disc = future_work_years_snhl_disc)
      }))%>%
      unnest_wider(future_work_years)%>%
      ungroup()
    
    list_age_distr_employed_expected[[draw_i]] <- df_age_distr_employed_expected_i
    
  }
  
  save(list_age_distr_employed_expected, file = "list_age_distr_employed_expected.Rdata")
  
}else{
  
  load("parameters_data/list_age_distr_employed_expected.Rdata")
  
}

#########################
### BIND ROWS OF LIST ###
#########################

df_age_distr_employed_expected = bind_rows(list_age_distr_employed_expected)

### CHECK CASES WHERE DUR_SNHL EXCEEDS REMAINING LIFE EXPECTANCY
sum(df_age_distr_employed_expected$life_exp_at_age_x < df_age_distr_employed_expected$dur_snhl)

########################################################
### PLOT BASE EXAMPLE OF YEARS OF FUTURE WORK BY AGE ###
########################################################

if(render_plots){
  ### Evaluate difference in countries
  p_future_work_years_by_country = df_age_distr_employed_expected%>%
    filter(n_draw == 1,
           Year == 2025,
           Age %in% c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))%>%
    dplyr::select(c("Country", "Age", "Sex", "future_work_years"))%>%
    mutate(Age = factor(Age))%>%
    ggplot(., aes(x = Age, y = future_work_years, fill = Country))+
    geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.75)+
    facet_wrap(facets = vars(Sex), nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")+
    geom_hline(yintercept = 0, colour = "grey")+
    ylab("Average per-death YPPLL, 2025")+
    scale_fill_manual(values = countries_cols_map)
  
  ggsave(p_future_work_years_by_country, file = "p_future_work_years_by_country.png", width = 14, height = 10, units = "cm")
  
  ### Evaluate change over the years
  p_future_work_years_by_year = df_age_distr_employed_expected%>%
    filter(n_draw == 1,
           GID_0 == "NGA",
           Year %in% c(2025, 2031, 2037),
           Age %in% c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))%>%
    dplyr::select(c("Year", "Age", "Sex", "future_work_years"))%>%
    mutate(Age = factor(Age))%>%
    ggplot(., aes(x = Age, y = future_work_years, fill = Year))+
    geom_bar(stat = "identity", position = position_dodge(), colour = "black", alpha = 0.75)+
    facet_wrap(facets = vars(Sex), nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")+
    geom_hline(yintercept = 0, colour = "grey")+
    ylab("Average per-death YPPLL, Nigeria")
  
  ggsave(p_future_work_years_by_year, file = "p_future_work_years_by_year.png", width = 14, height = 10, units = "cm")
  
  
  ### Evalute totals, impact of discounting
  p_future_work_years_by_discounting = df_age_distr_employed_expected%>%
    filter(n_draw == 1,
           GID_0 == "NGA",
           Year == 2025,
           Age %in% c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))%>%
    dplyr::select(c("Age", "Sex", "future_work_years", "future_work_years_disc"))%>%
    pivot_longer(-c(Age, Sex), names_to = "measure", values_to = "value")%>%
    mutate(`Annual discount rate` = factor(measure, 
                                           levels = c("future_work_years", "future_work_years_disc"),
                                           labels = c("0%", "3.5%")))%>%
    mutate(Age = factor(Age))%>%
    ggplot(., aes(x = Age, y = value, fill = `Annual discount rate`))+
    geom_bar(stat = "identity", alpha = 0.75, position = position_dodge(), colour = "black")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")+
    facet_wrap(facets = vars(Sex), nrow = 2)+
    geom_hline(yintercept = 0, colour = "grey")+
    ylab("Average per-death YPPLL, Nigeria, 2025")+
    scale_fill_manual("Annual\ndiscount\nrate", values = c("#FFA500", "#005AFF"))
  
  ggsave(p_future_work_years_by_discounting, file = "p_future_work_years_by_discounting.png", width = 14, height = 10, units = "cm")
  
  
  ### Evaluate SNHL, impact of discounting
  p_future_work_years_snhl_by_discounting = df_age_distr_employed_expected%>%
    filter(n_draw == 1,
           GID_0 == "NGA",
           Year == 2025,
           Age %in% c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))%>%
    dplyr::select(c("Age", "Sex", "future_work_years_snhl", "future_work_years_snhl_disc"))%>%
    pivot_longer(-c(Age, Sex), names_to = "measure", values_to = "value")%>%
    mutate(`Annual discount rate` = factor(measure, 
                                           levels = c("future_work_years_snhl", "future_work_years_snhl_disc"),
                                           labels = c("0%", "3.5%")))%>%
    mutate(Age = factor(Age))%>%
    ggplot(., aes(x = Age, y = value*0.18, fill = `Annual discount rate`))+
    geom_bar(stat = "identity", alpha = 0.75, position = position_dodge(), colour = "black")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")+
    facet_wrap(facets = vars(Sex), nrow = 2)+
    geom_hline(yintercept = 0, colour = "grey")+
    ylab("Average per-case YPPLL due to SNHL, Nigeria, 2025")+
    scale_fill_manual("Annual\ndiscount\nrate", values = c("#FFA500", "#005AFF"))
  
  ggsave(p_future_work_years_snhl_by_discounting, file = "p_future_work_years_snhl_by_discounting.png", width = 14, height = 10, units = "cm")
  
  ### Evaluate SNHL, impact of discounting, with uncertainty (boxplots)
  p_future_work_years_snhl_by_discounting_boxplot = df_age_distr_employed_expected%>%
    filter(GID_0 == "NGA",
           Year == 2025,
           Age %in% c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))%>%
    dplyr::select(c("Age", "Sex", "future_work_years_snhl", "future_work_years_snhl_disc"))%>%
    pivot_longer(-c(Age, Sex), names_to = "measure", values_to = "value")%>%
    mutate(`Annual discount rate` = factor(measure, 
                                           levels = c("future_work_years_snhl", "future_work_years_snhl_disc"),
                                           labels = c("0%", "3.5%")))%>%
    mutate(Age = factor(Age))%>%
    ggplot(., aes(x = Age, y = value*0.18, fill = `Annual discount rate`, colour = `Annual discount rate`))+
    geom_point(alpha = 0.2, position = position_jitterdodge(jitter.height = 0, jitter.width = 0.5), size = 0.25)+
    geom_boxplot(alpha = 0.5, position = position_dodge(), colour = "black", outlier.shape = NA)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")+
    facet_wrap(facets = vars(Sex), nrow = 2)+
    geom_hline(yintercept = 0, colour = "grey")+
    ylab("Average per-case YPPLL due to SNHL, Nigeria, 2025")+
    scale_colour_manual("Annual\ndiscount\nrate", values = c("#FFA500", "#005AFF"))+
    scale_fill_manual("Annual\ndiscount\nrate", values = c("#FFA500", "#005AFF"))
  
  ggsave(p_future_work_years_snhl_by_discounting_boxplot, file = "p_future_work_years_snhl_by_discounting_boxplot.png", width = 14, height = 10, units = "cm")

}




###################################
### COUNTRY-SPECIFIC PARAMETERS ###
###################################

########################
### PPP-ADJUSTED GNI ###
########################

# https://data.worldbank.org/indicator/NY.GNP.PCAP.PP.CD

### Load 2023 data in International Dollars for 4 target countries
df_GNI = loadRData(paste0("parameters_data/df_GNI.rda"))

### USA value (needed later for VSL calculation)
GNI_USA = 82190

#######################
### TREATMENT COSTS ###
#######################

### These have been udpated to 2023

### hospital treatment costs including the share that are out of pocket expenses
df_scaling = loadRData(paste0("parameters_data/scaling_master.rda"))%>%
  rename(cost_per_hosp = treat_lassa_hospital_dollar_2023 ,
         cost_per_hosp_oop = countr_specific_OOP_2023IntD)%>%
  mutate(cost_per_hosp_gvt = cost_per_hosp - cost_per_hosp_oop)%>%
  dplyr::select(GID_0, Country, cost_per_hosp, cost_per_hosp_oop, cost_per_hosp_gvt)

### Costs of outpatient visits
# Taken from estimated national unit cost of outpatient visits, in international dollars 2017
# https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(18)30213-5/fulltext#%20 

### adjusted for inflation using GDP deflator from World Bank Open Data
# https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.DEFL.ZS&country=USA

df_cost_outpatient_visit = read.csv(paste0("parameters_data/cost_outpatient.csv"))%>%
  mutate(GDP_deflator_2017_2023 = 125.6/102.8)%>%
  mutate(Cost_USD_2023 = Cost_USD_2017*GDP_deflator_2017_2023)%>%
  rename(Country = COUNTRY,
         cost_per_outpatientvisit = Cost_USD_2023)%>%
  filter(Country %in% countries_final)%>%
  dplyr::select(GID_0, Country, cost_per_outpatientvisit)


#######################
### MONETISED DALYs ###
#######################

### These have been updated using 2023 per capita GDP 
### against same estimates from Ochalek 2015 of % of GDP per DALY

### daly value estimates
df_daly = loadRData(paste0("parameters_data/daly_master.rda"))%>%
  rename(cost_per_daly = daly_value)%>%
  dplyr::select(GID_0, Country, cost_per_daly)



#################################
### VALUE OF STATISTICAL LIFE ###
#################################

### Start with base USA value from 2023: 13.2 million USD
# https://www.transportation.gov/office-policy/transportation-policy/revised-departmental-guidance-on-valuation-of-a-statistical-life-in-economic-analysis

VSL_USA = 13200000

### Adjust this base value using 2023 estimates of per capita GNI
# use formula VSL_c = VSL_usa * (GNI_c / GNI_usa)^1.5

df_VSL = df_GNI%>%
  mutate(VSL = VSL_USA * (GNI/GNI_USA)^1.5)%>%
  rename(cost_per_vsl = VSL)%>%
  dplyr::select(GID_0, Country, cost_per_vsl)

################################
### CATASTROPHIC EXPENDITURE ###
################################

#### probability of catastrophic expenses
df_catastrophe = loadRData(paste0("parameters_data/poverty_master.rda"))%>%
  rename(prob_catastrophic = prop_at_risk_catastrp_HCexp,
         prob_impoverishment = prop_at_risk_impoverishing)%>%
  filter(Country %in% countries_final)




################################################
### COMBINE COUNTRY-SPECIFIC ECON PARAMETERS ###
################################################

df_params_econ_country = df_GNI%>%
  left_join(., df_scaling, by = c("Country", "GID_0"))%>%
  left_join(., df_cost_outpatient_visit, by = c("Country", "GID_0"))%>%
  left_join(., df_daly, by = c("Country", "GID_0"))%>%
  left_join(., df_VSL, by = c("Country", "GID_0"))%>%
  left_join(., df_catastrophe, by = c("Country", "GID_0"))



###########################################
### FINAL ECONOMIC PARAMETERS DATAFRAME ###
###########################################

df_params_econ_final = df_age_distr_employed_expected%>%
  dplyr::select(-c(life_exp_at_age_x, ag_ilo, life_exp_at_birth))%>%
  left_join(., df_params_econ_country, by = c("Country", "GID_0"))%>%
  mutate(ag = case_when(Age >= age_group_mins[1] & Age < age_group_mins[2] ~ age_groups_final[1],
                        Age >= age_group_mins[2] & Age < age_group_mins[3] ~ age_groups_final[2],
                        Age >= age_group_mins[3] & Age < age_group_mins[4] ~ age_groups_final[3],
                        Age >= age_group_mins[4] & Age < age_group_mins[5] ~ age_groups_final[4],
                        Age >= age_group_mins[5] & Age < age_group_mins[6] ~ age_groups_final[5],
                        Age >= age_group_mins[6] ~ age_groups_final[6]))

### Save as RData
save(df_params_econ_final, file = "df_params_econ_final.Rdata")

#write.csv(df_params_econ_final, "df_params_econ_final.csv")
