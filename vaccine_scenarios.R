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

# duration of immunity (in years)
duration_immunity = 10


############################################
### LOAD BASE DEMOGRAPHY FOR CONSISTENCY ###
############################################

### Load
load("demography/df_population_finalAges.Rdata")
load("demography/df_population_ag.Rdata")

### Identify distinct combinations of Country, GID_0, Region and GID_1

df_regions_final = unique(df_population_finalAges[c("Country", "GID_0", "Region", "GID_1")])


#################################
### DEFINE TARGET POPULATIONS ###
#################################

### ALL ###
df_population_ag_MF = df_population_ag%>%filter(Sex %in% c("Male", "Female"))
df_population_groups_full = unique(df_population_ag_MF[c("Sex", "Age")])

### WCBA ###
df_population_ag_wcba = df_population_ag_MF%>%
  filter(Sex %in% vec_wcba_sex, Age %in% vec_wcba_ages)
df_population_groups_wcba = unique(df_population_ag_wcba[c("Sex", "Age")])

### ADULTS ###
df_population_ag_adults = df_population_ag_MF%>%
  filter(Sex %in% vec_adults_sex, Age %in% vec_adults_ages)
df_population_groups_adults = unique(df_population_ag_adults[c("Sex", "Age")])

### CHILDREN ###
df_population_ag_children = df_population_ag_MF%>%
  filter(Sex %in% vec_children_sex, Age %in% vec_children_ages)
df_population_groups_children = unique(df_population_ag_children[c("Sex", "Age")])

### ELDERLY ###
df_population_ag_elderly = df_population_ag_MF%>%
  filter(Sex %in% vec_elderly_sex, Age %in% vec_elderly_ages)
df_population_groups_elderly = unique(df_population_ag_elderly[c("Sex", "Age")])


##################################
### LOAD VACCINE STRATEGY DATA ###
##################################

### Vaccine proportions extracted from Women of Childbearing Age, but applied to any group
df_vacc_low_daily = read_excel("vaccination/Lassa_Vx_Scenarios_1_Dose_Long_Form.xlsx", sheet = "WCBA_Low")%>%
  rename(Country = country,
         GID_0 = iso,
         Region = admin_1,
         uptake_prop_annual = `total uptake (% of total pop per year)`,
         uptake_prop_daily = `daily uptake (% of preg pop per day outside of season)`,
         uptake_doses_daily = `demand (doses)`)%>%
  left_join(., df_regions_final, by = c("Country", "GID_0", "Region"))%>%
  mutate(reach = "low")%>%
  dplyr::select(-uptake_doses_daily)

df_vacc_med_daily = read_excel("vaccination/Lassa_Vx_Scenarios_1_Dose_Long_Form.xlsx", sheet = "WCBA_Medium")%>%
  rename(Country = country,
         GID_0 = iso,
         Region = admin_1,
         uptake_prop_annual = `total uptake (% of total pop per year)`,
         uptake_prop_daily = `daily uptake (% of preg pop per day outside of season)`,
         uptake_doses_daily = `demand (doses)`)%>%
  left_join(., df_regions_final, by = c("Country", "GID_0", "Region"))%>%
  mutate(reach = "med")%>%
  dplyr::select(-uptake_doses_daily)

df_vacc_high_daily = read_excel("vaccination/Lassa_Vx_Scenarios_1_Dose_Long_Form.xlsx", sheet = "WCBA_High")%>%
  rename(Country = country,
         GID_0 = iso,
         Region = admin_1,
         uptake_prop_annual = `total uptake (% of total pop per year)`,
         uptake_prop_daily = `daily uptake (% of preg pop per day outside of season)`,
         uptake_doses_daily = `demand (doses)`)%>%
  left_join(., df_regions_final, by = c("Country", "GID_0", "Region"))%>%
  mutate(reach = "high")%>%
  dplyr::select(-uptake_doses_daily)


### Combine vaccine strategies
df_vacc_strategies_daily = bind_rows(df_vacc_low_daily,
                                     df_vacc_med_daily,
                                     df_vacc_high_daily)


####################################
### CUSTOM VACCINE STRATEGY DATA ###
####################################

### April 1st, day = 91
vacc_start = vacc_first_day

### November 1st, day = 335
vacc_end = vacc_last_day + 1

### Target proportion annually
vacc_prop_annual = 0.25

### What proportion per day to reach 0.25 by end of vacc period
vacc_prop_daily = vacc_prop_annual/(vacc_end-vacc_start)

vacc_prop_daily_annual = c(rep(0, vacc_start-1),
                           rep(vacc_prop_daily, vacc_end-vacc_start),
                           rep(0, 365 - (vacc_end-1)))

### Check it works as desired
vacc_prop_daily_annual[c(vacc_start - 1, vacc_start, vacc_last_day, vacc_end)]
sum(vacc_prop_daily_annual)

### repeat for all 3 years
df_vacc_prop_daily_annual = data.frame(day = 1:(365*3),
                                       uptake_prop_daily = vacc_prop_daily_annual)

df_vacc_low_daily_custom = unique(df_vacc_low_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_prop_daily_annual)

df_vacc_med_daily_custom = unique(df_vacc_med_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_prop_daily_annual)

df_vacc_high_daily_custom = unique(df_vacc_high_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_prop_daily_annual)

### Combine vaccine strategies

df_vacc_strategies_daily_custom = bind_rows(df_vacc_low_daily_custom,
                                            df_vacc_med_daily_custom,
                                            df_vacc_high_daily_custom)

#####################
### IMMUNE WANING ###
#####################

df_vacc_waning_daily_annual = data.frame(day = (1+10*365):(365*13),
                                         uptake_prop_daily = -vacc_prop_daily_annual)

df_vacc_waning_low_daily_custom = unique(df_vacc_low_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_waning_daily_annual)

df_vacc_waning_med_daily_custom = unique(df_vacc_med_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_waning_daily_annual)

df_vacc_waning_high_daily_custom = unique(df_vacc_high_daily[c("Country", "GID_0", "Region", "GID_1", "burden", "reach")])%>%
  cross_join(., df_vacc_waning_daily_annual)

df_vacc_waning_daily_custom = bind_rows(df_vacc_waning_low_daily_custom,
                                        df_vacc_waning_med_daily_custom,
                                        df_vacc_waning_high_daily_custom)

#################################################
### Regions specific to each vaccine strategy ###
#################################################

df_regions_low = unique(df_vacc_low_daily[c("Country", "GID_0", "Region", "GID_1", "burden")])
df_regions_med = unique(df_vacc_med_daily[c("Country", "GID_0", "Region", "GID_1", "burden")])
df_regions_high = unique(df_vacc_high_daily[c("Country", "GID_0", "Region", "GID_1", "burden")])
df_regions_all = unique(df_vacc_strategies_daily[c("Country", "GID_0", "Region", "GID_1", "burden")])

vec_GID_1_low = df_regions_low$GID_1
vec_GID_1_med = df_regions_med$GID_1
vec_GID_1_high = df_regions_high$GID_1
vec_GID_1_all = df_regions_all$GID_1

#########################
### Generic time data ###
#########################

if(year_start + 12 != year_end){warning("Vaccine horizon not adding up to years simulated")}

df_vacc_time_horizon = data.frame(Year = c(rep(year_start, 52), 
                                           rep(year_start+1, 52),
                                           rep(year_start+2, 52), 
                                           rep(year_start+3, 52), 
                                           rep(year_start+4, 52), 
                                           rep(year_start+5, 52), 
                                           rep(year_start+6, 52), 
                                           rep(year_start+7, 52), 
                                           rep(year_start+8, 52), 
                                           rep(year_start+9, 52),
                                           rep(year_start+10, 52), 
                                           rep(year_start+11, 52), 
                                           rep(year_start+12, 52)),
                                  week_of_year = rep((1:52), n_years_horizon),
                                  week = 1:(52*n_years_horizon))


#######################################################################
### CREATE BASE DATA STRUCTURE ACROSS ALL POPULATION-TIME VARIABLES ###
#######################################################################

df_regions_population_time_full = df_regions_all%>%
  cross_join(., df_population_groups_full)%>%
  cross_join(., df_vacc_time_horizon)

########################################################################################
### Translate vaccine data to weekly and cross with strategy-specific age-sex groups ###
########################################################################################

### Define lag to immunogenicity ###
lag_immune_weeks = 2 


#################
### LOW REACH ###
#################

### Count weekly proportion of target population vaccinated over 3 years of campaign
df_vacc_low_weekly_campaign = df_vacc_low_daily_custom%>%
  mutate(week = ceiling(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = sum(uptake_prop_daily),
            waning_prop_weekly = 0)%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year < 2028)

### Make empty dataset for post-campaign, pre-waning years
df_vacc_low_weekly_postcampaign = df_regions_low%>%
  cross_join(df_vacc_time_horizon%>%filter(Year>2027 & Year < 2035))%>%
  mutate(uptake_prop_weekly = 0,
         waning_prop_weekly = 0)

### Make decreasing dataset for waning immunity
df_vacc_low_weekly_waning = df_vacc_waning_low_daily_custom%>%
  mutate(week = floor(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = 0,
            waning_prop_weekly = sum(uptake_prop_daily))%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year >= 2035)

df_vacc_low_weekly = bind_rows(df_vacc_low_weekly_campaign,
                               df_vacc_low_weekly_postcampaign,
                               df_vacc_low_weekly_waning)


####################
### MEDIUM REACH ###
####################

### Count weekly proportion of target population vaccinated over 3 years of campaign
df_vacc_med_weekly_campaign = df_vacc_med_daily_custom%>%
  mutate(week = ceiling(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = sum(uptake_prop_daily),
            waning_prop_weekly = 0)%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year < 2028)

### Make empty dataset for post-campaign, pre-waning years
df_vacc_med_weekly_postcampaign = df_regions_med%>%
  cross_join(df_vacc_time_horizon%>%filter(Year>2027 & Year < 2035))%>%
  mutate(uptake_prop_weekly = 0,
         waning_prop_weekly = 0)

### Make decreasing dataset for waning immunity
df_vacc_med_weekly_waning = df_vacc_waning_med_daily_custom%>%
  mutate(week = floor(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = 0,
            waning_prop_weekly = sum(uptake_prop_daily))%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year >= 2035)

df_vacc_med_weekly = bind_rows(df_vacc_med_weekly_campaign,
                               df_vacc_med_weekly_postcampaign,
                               df_vacc_med_weekly_waning)


##################
### HIGH REACH ###
##################

### Count weekly proportion of target population vaccinated over 3 years of campaign
df_vacc_high_weekly_campaign = df_vacc_high_daily_custom%>%
  mutate(week = ceiling(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = sum(uptake_prop_daily),
            waning_prop_weekly = 0)%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year < 2028)

### Make empty dataset for post-campaign, pre-waning years
df_vacc_high_weekly_postcampaign = df_regions_high%>%
  cross_join(df_vacc_time_horizon%>%filter(Year>2027 & Year < 2035))%>%
  mutate(uptake_prop_weekly = 0,
         waning_prop_weekly = 0)

### Make decreasing dataset for waning immunity
df_vacc_high_weekly_waning = df_vacc_waning_high_daily_custom%>%
  mutate(week = floor(day/7))%>%
  group_by(reach, Country, GID_0, Region, GID_1, burden, week)%>%
  summarise(uptake_prop_weekly = 0,
            waning_prop_weekly = sum(uptake_prop_daily))%>%
  left_join(., df_vacc_time_horizon, by = "week")%>%
  filter(Year >= 2035)

df_vacc_high_weekly = bind_rows(df_vacc_high_weekly_campaign,
                               df_vacc_high_weekly_postcampaign,
                               df_vacc_high_weekly_waning)



########################################################
### SAVE THESE REACH TARGETS FOR VACCINE FORECASTING ###
########################################################

### Save final campaign data for vaccine demand forecasting
df_vacc_weekly_campaign_DF = bind_rows(df_vacc_low_weekly_campaign,
                                    df_vacc_med_weekly_campaign,
                                    df_vacc_high_weekly_campaign)

write.xlsx(df_vacc_weekly_campaign_DF, file = "df_vacc_weekly_campaign_DF.xlsx")


#####################################################################
### APPLY LOW, MED AND HIGH REACH TO DIFFERENT TARGET POPULATIONS ###
#####################################################################

############
### WCBA ###
############

### Low, vaccine and waning
df_vacc_low_weekly_wcba = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_low")

df_waning_low_weekly_wcba = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_low")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_low_weekly_wcba = df_vacc_low_weekly_wcba%>%
  left_join(., df_waning_low_weekly_wcba, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### Medium, vaccine and waning
df_vacc_med_weekly_wcba = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_med")

df_waning_med_weekly_wcba = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_med")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_med_weekly_wcba = df_vacc_med_weekly_wcba%>%
  left_join(., df_waning_med_weekly_wcba, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### High, vaccine and waning
df_vacc_high_weekly_wcba = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_high")

df_waning_high_weekly_wcba = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_wcba_sex & Age %in% vec_wcba_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "wcba_high")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_high_weekly_wcba = df_vacc_high_weekly_wcba%>%
  left_join(., df_waning_high_weekly_wcba, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

##############
### ADULTS ###
##############

### Low, vaccine and waning
df_vacc_low_weekly_adults = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_low")

df_waning_low_weekly_adults = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_low")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_low_weekly_adults = df_vacc_low_weekly_adults%>%
  left_join(., df_waning_low_weekly_adults, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### Medium, vaccine and waning
df_vacc_med_weekly_adults = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_med")

df_waning_med_weekly_adults = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_med")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_med_weekly_adults = df_vacc_med_weekly_adults%>%
  left_join(., df_waning_med_weekly_adults, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### High, vaccine and waning
df_vacc_high_weekly_adults = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_high")

df_waning_high_weekly_adults = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_adults_sex & Age %in% vec_adults_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "adults_high")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_high_weekly_adults = df_vacc_high_weekly_adults%>%
  left_join(., df_waning_high_weekly_adults, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)


################
### CHILDREN ###
################

### Low, vaccine and waning
df_vacc_low_weekly_children = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_low")

df_waning_low_weekly_children = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_low")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_low_weekly_children = df_vacc_low_weekly_children%>%
  left_join(., df_waning_low_weekly_children, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### Medium, vaccine and waning
df_vacc_med_weekly_children = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_med")

df_waning_med_weekly_children = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_med")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_med_weekly_children = df_vacc_med_weekly_children%>%
  left_join(., df_waning_med_weekly_children, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### High, vaccine and waning
df_vacc_high_weekly_children = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_high")

df_waning_high_weekly_children = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_children_sex & Age %in% vec_children_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "children_high")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_high_weekly_children = df_vacc_high_weekly_children%>%
  left_join(., df_waning_high_weekly_children, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)


###############
### ELDERLY ###
###############

### Low, vaccine and waning
df_vacc_low_weekly_elderly = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_low")

df_waning_low_weekly_elderly = df_vacc_low_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_low")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_low_weekly_elderly = df_vacc_low_weekly_elderly%>%
  left_join(., df_waning_low_weekly_elderly, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### Medium, vaccine and waning
df_vacc_med_weekly_elderly = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_med")

df_waning_med_weekly_elderly = df_vacc_med_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_med")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_med_weekly_elderly = df_vacc_med_weekly_elderly%>%
  left_join(., df_waning_med_weekly_elderly, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)

### High, vaccine and waning
df_vacc_high_weekly_elderly = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(waning_prop_weekly))%>%
  mutate(uptake_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ uptake_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_high")

df_waning_high_weekly_elderly = df_vacc_high_weekly%>%
  cross_join(df_population_groups_full)%>%
  dplyr::select(-c(uptake_prop_weekly))%>%
  mutate(waning_prop_weekly = case_when(Sex %in% vec_elderly_sex & Age %in% vec_elderly_ages ~ waning_prop_weekly,
                                        T ~ 0))%>%
  mutate(strategy = "elderly_high")%>%
  mutate(Age = Age + duration_immunity)

df_vacc_waning_high_weekly_elderly = df_vacc_high_weekly_elderly%>%
  left_join(., df_waning_high_weekly_elderly, 
            by = c("reach", "Country", "GID_0", "Region", "GID_1", "burden", "week", "Year", "week_of_year", "Sex", "Age", "strategy"))%>%
  mutate_if(is.numeric,coalesce,0)



###################################
### COMBINED VACCINE STRATEGIES ###
###################################

df_vacc_weekly = bind_rows(df_vacc_waning_low_weekly_wcba,
                           df_vacc_waning_med_weekly_wcba,
                           df_vacc_waning_high_weekly_wcba,
                           df_vacc_waning_low_weekly_adults,
                           df_vacc_waning_med_weekly_adults,
                           df_vacc_waning_high_weekly_adults,
                           df_vacc_waning_low_weekly_children,
                           df_vacc_waning_med_weekly_children,
                           df_vacc_waning_high_weekly_children,
                           df_vacc_waning_low_weekly_elderly,
                           df_vacc_waning_med_weekly_elderly,
                           df_vacc_waning_high_weekly_elderly)%>%
  ungroup()%>%
  dplyr::select(-reach)%>%
  mutate(net_prop_vacc_weekly = uptake_prop_weekly + waning_prop_weekly)

#####################################################################
### VACCINE COHORTS: AGE VACCINATED INDIVIDUALS THROUGH THE MODEL ###
#####################################################################

################################
### Define first year cohort ###
################################



df_vacc_weekly_cohort = df_vacc_weekly%>%filter(Year == 2025)%>%
  group_by(strategy, Country, GID_0, Region, GID_1, burden, Year, Sex, Age)%>%
  mutate(uptake_prop_weekly_cumul = cumsum(uptake_prop_weekly),
         waning_prop_weekly_cumul = cumsum(waning_prop_weekly),
         net_prop_vacc_weekly_cumul = cumsum(net_prop_vacc_weekly),
         immune_prop_weekly_cumul = dplyr::lag(uptake_prop_weekly_cumul, lag_immune_weeks, default = 0),
         waned_prop_weekly_cumul = dplyr::lag(waning_prop_weekly_cumul, lag_immune_weeks, default = 0),
         net_prop_immune_weekly_cumul = dplyr::lag(net_prop_vacc_weekly_cumul, lag_immune_weeks, default = 0))%>%
  ungroup()%>%
  dplyr::select(-c(uptake_prop_weekly, waning_prop_weekly, net_prop_vacc_weekly))



####################################################
### Define empty lists to save cohorts each year ###
####################################################

list_vacc_weekly_cohort = list()
list_vacc_weekly_cohort[[1]] = df_vacc_weekly_cohort

#######################################################################
### LOOP THROUGH YEARS TO GRADUALLY AGE COHORTS AND ACCRUE IMMUNITY ###
#######################################################################


for(Year_i in (year_start+1):year_end){
  
  print(paste0("generating vaccination coverage year ", Year_i))
  
  ### PREVIOUS YEAR: carry the previously vaccinated population to the next year and age them
  df_vacc_weekly_carried_forward = df_vacc_weekly_cohort%>%
    group_by(strategy, Country, GID_0, Region, GID_1, burden, Sex, Age)%>%
    dplyr::slice_tail(n=1)%>%
    dplyr::select(strategy, Country, GID_0, Region, GID_1, burden, Sex, Age, 
                  uptake_prop_weekly_cumul, immune_prop_weekly_cumul,
                  waning_prop_weekly_cumul, waned_prop_weekly_cumul, 
                  net_prop_vacc_weekly_cumul, net_prop_immune_weekly_cumul)%>%
    mutate(Age = Age+1, Year = Year_i)%>%
    rename(uptake_prop_weekly_cumul_pre = uptake_prop_weekly_cumul,
           immune_prop_weekly_cumul_pre = immune_prop_weekly_cumul,
           waning_prop_weekly_cumul_pre = waning_prop_weekly_cumul,
           waned_prop_weekly_cumul_pre = waned_prop_weekly_cumul,
           net_prop_vacc_weekly_cumul_pre = net_prop_vacc_weekly_cumul,
           net_prop_immune_weekly_cumul_pre = net_prop_immune_weekly_cumul
           )
  
  ### THIS YEAR: filter out year i of vaccination, and apply new vaccine doses in target population and determine cumulative amount vaccinated
  df_vacc_weekly_i = df_vacc_weekly%>%filter(Year == Year_i)%>%
    group_by(strategy, Country, GID_0, Region, GID_1, burden, Year, Sex, Age)%>%
    mutate(uptake_prop_weekly_cumul = cumsum(uptake_prop_weekly),
           immune_prop_weekly_cumul = dplyr::lag(uptake_prop_weekly_cumul, lag_immune_weeks, default = 0),
           waning_prop_weekly_cumul = cumsum(waning_prop_weekly),
           waned_prop_weekly_cumul = dplyr::lag(waning_prop_weekly_cumul, lag_immune_weeks, default = 0),
           net_prop_vacc_weekly_cumul = cumsum(net_prop_vacc_weekly),
           net_prop_immune_weekly_cumul = dplyr::lag(net_prop_vacc_weekly_cumul, lag_immune_weeks, default = 0))%>%
    ungroup()%>%
    dplyr::select(-c(uptake_prop_weekly, waning_prop_weekly, net_prop_vacc_weekly))
  
  ### COMBINE YEARS: join the previous cumulative protection to the current batch and add cumulative totals together
  df_vacc_weekly_joined_i = df_vacc_weekly_i%>%
    full_join(., df_vacc_weekly_carried_forward, by = c("strategy", "Country", "GID_0", "Region", "GID_1", "burden", "Sex", "Age", "Year")) %>%
    mutate_if(is.numeric,coalesce,0)%>%
    #group_by(Country, GID_0, Region, GID_1, burden, year, Sex, Age)%>%
    mutate(uptake_prop_weekly_cumul = uptake_prop_weekly_cumul + uptake_prop_weekly_cumul_pre,
           immune_prop_weekly_cumul = immune_prop_weekly_cumul + immune_prop_weekly_cumul_pre,
           waning_prop_weekly_cumul = waning_prop_weekly_cumul + waning_prop_weekly_cumul_pre,
           waned_prop_weekly_cumul = waned_prop_weekly_cumul + waned_prop_weekly_cumul_pre,
           net_prop_vacc_weekly_cumul = net_prop_vacc_weekly_cumul + net_prop_vacc_weekly_cumul_pre,
           net_prop_immune_weekly_cumul = net_prop_immune_weekly_cumul + net_prop_immune_weekly_cumul_pre)%>%
    dplyr::select(-c(uptake_prop_weekly_cumul_pre, immune_prop_weekly_cumul_pre, waning_prop_weekly_cumul_pre, waned_prop_weekly_cumul_pre,
                     net_prop_vacc_weekly_cumul_pre, net_prop_immune_weekly_cumul_pre))
  
  
  ### update cohort for next year to end of Year_i
  df_vacc_weekly_cohort = df_vacc_weekly_joined_i
  
  ### save this year's output
  list_vacc_weekly_cohort[[Year_i]] = df_vacc_weekly_cohort
  
}

df_vacc_scenarios_weekly = do.call(rbind, list_vacc_weekly_cohort)%>%
  mutate(net_prop_vacc_weekly_cumul = signif(net_prop_vacc_weekly_cumul, 6),
         net_prop_immune_weekly_cumul = signif(net_prop_immune_weekly_cumul, 6))


######################
### PLOT SCENARIOS ###
######################

df_vacc_scenarios_weekly%>%filter(week_of_year == 2, Age == 16, Sex == "Female", Region == "Edo", strategy == "adults_high")%>%
  dplyr::select(-c(burden, Country, GID_0))%>%View()


##################################
### DEMONSTRATE VACCINE COHORT ###
##################################

p_vaccine_cohort_example = df_vacc_scenarios_weekly%>%
  filter(strategy %in% c("adults_low", "children_low", "elderly_low"),
         Region == "Edo", Sex == "Female", Age %in% c(10, 20, 30, 40, 50, 60))%>%
  mutate(strategy = factor(strategy,
                           levels = c("children_low", "adults_low", "elderly_low"),
                           labels = c("Vaccinate 2-14 year-olds", "Vaccinate 15-49 year-olds", "Vaccinate 50+ year-olds")),
         Age = factor(Age,
                      levels = c(10, 20, 30, 40, 50, 60),
                      labels = c("10-year-olds", "20-year-olds",
                                 "30-year-olds", "40-year-olds",
                                 "50-year-olds", "60-year-olds")))%>%
  group_by(strategy, Region, Age, Year)%>%
  dplyr::slice_tail(n=1)%>%
  ggplot(., aes(x = Year, y = net_prop_immune_weekly_cumul, fill = factor(Age)))+
  geom_rect(aes(xmin = 2024.5, xmax = 2027.5, ymin = 0, ymax = 1), fill = "grey", alpha = 0.3)+
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  facet_wrap(facets = vars(strategy), nrow = 3)+
  theme_bw()+
  scale_fill_manual("Age band", values = ag_cols)+
  xlab("Year")+ylab("Cumulative proportion of age band\nimmunised by December 31st")+
  scale_x_continuous(limits = c(NA,NA), breaks = c(2025, 2027, 2029, 2031, 2033, 2035, 2037))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p_vaccine_cohort_example
  
ggsave(p_vaccine_cohort_example, file = "p_vaccine_cohort_example.png", width = 20, height = 14, units = "cm")
  

####################################
### SAVE FINAL VACCINE SCENARIOS ###
####################################

# save(df_vacc_scenarios_weekly, file = "df_vacc_scenarios_weekly.Rdata")
