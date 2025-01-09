#######################
### DEMAND FORECAST ###
#######################

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

##########################################
### LOAD ANNUAL POPULATION DATA BY AGE ###
##########################################

df_population_DF = loadRData("demography/df_population.Rdata")%>%
  filter(Year <= 2027, Sex %in% c("Male", "Female"))%>%
  dplyr::select(-c(proportion_subnational, UN_scaled, PregProp))%>%
  dplyr::select(c(Country, GID_0, Region, GID_1, Year, Sex, Age, UN_scaled_subnational))%>%
  arrange(GID_0, GID_1, Year, Sex, Age)

# write.xlsx(df_population_DF, file = "df_population_DF.xlsx")


##############################
### LOAD VACCINE SCENARIOS ###
##############################


#########################################
### LOAD VACCINE FORECAST (FROM ANDY) ###
##########################################

discRate = 0.035

df_df_elderly = read.xlsx("vaccination/demand_forecast/Andy/df_vacc_weekly_campaign_VF.xlsx", sheet = "elderly")%>%
  filter(reach == "high")%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(doses = sum(wastage_adjusted_demand))%>%
  mutate(year_disc = Year - 2025,
         doses_disc = doses*(1/(1+discRate)^year_disc))%>%
  dplyr::select(-c(year_disc))

df_df_children = read.xlsx("vaccination/demand_forecast/Andy/df_vacc_weekly_campaign_VF.xlsx", sheet = "children")%>%
  filter(reach == "high")%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(doses = sum(wastage_adjusted_demand))%>%
  mutate(year_disc = Year - 2025,
         doses_disc = doses*(1/(1+discRate)^year_disc))%>%
  dplyr::select(-c(year_disc))

df_df_wcba = read.xlsx("vaccination/demand_forecast/Andy/df_vacc_weekly_campaign_VF.xlsx", sheet = "wcba")%>%
  filter(reach == "high")%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(doses = sum(wastage_adjusted_demand))%>%
  mutate(year_disc = Year - 2025,
         doses_disc = doses*(1/(1+discRate)^year_disc))%>%
  dplyr::select(-c(year_disc))

df_df_adults = read.xlsx("vaccination/demand_forecast/Andy/df_vacc_weekly_campaign_VF.xlsx", sheet = "adults")%>%
  filter(reach == "high")%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(doses = sum(wastage_adjusted_demand))%>%
  mutate(year_disc = Year - 2025,
         doses_disc = doses*(1/(1+discRate)^year_disc))%>%
  dplyr::select(-c(year_disc))

df_df_combined = bind_rows(df_df_elderly,
                           df_df_children,
                           df_df_adults)%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))



#########################################
### VALIDATE DOSES AGAINST POPULATION ###
#########################################

df_pop_validate_elderly = df_population_DF%>%
  dplyr::filter(Age >= 50)%>%
  group_by(Country, GID_0, Region, Year)%>%
  summarise(popSize = sum(UN_scaled_subnational))%>%
  left_join(., df_df_elderly, by = c("Country", "GID_0", "Region", "Year"))%>%
  mutate(prop_vacc = doses/popSize)

df_pop_validate_children = df_population_DF%>%
  dplyr::filter(Age >= 2 & Age < 15)%>%
  group_by(Country, GID_0, Region, Year)%>%
  summarise(popSize = sum(UN_scaled_subnational))%>%
  left_join(., df_df_children, by = c("Country", "GID_0", "Region", "Year"))%>%
  mutate(prop_vacc = doses/popSize)

df_pop_validate_wcba = df_population_DF%>%
  dplyr::filter(Age >= 15 & Age < 50,
                Sex == "Female")%>%
  group_by(Country, GID_0, Region, Year)%>%
  summarise(popSize = sum(UN_scaled_subnational))%>%
  left_join(., df_df_wcba, by = c("Country", "GID_0", "Region", "Year"))%>%
  mutate(prop_vacc = doses/popSize)

df_pop_validate_adults = df_population_DF%>%
  dplyr::filter(Age >= 15 & Age < 50)%>%
  group_by(Country, GID_0, Region, Year)%>%
  summarise(popSize = sum(UN_scaled_subnational))%>%
  left_join(., df_df_adults, by = c("Country", "GID_0", "Region", "Year"))%>%
  mutate(prop_vacc = doses/popSize)

df_pop_validate_combined = df_population_DF%>%
  dplyr::filter(Age >= 2)%>%
  group_by(Country, GID_0, Region, Year)%>%
  summarise(popSize = sum(UN_scaled_subnational))%>%
  left_join(., df_df_combined, by = c("Country", "GID_0", "Region", "Year"))%>%
  mutate(prop_vacc = doses/popSize)


###############################################
### PREPARE AND EXPORT FINAL DOSES DATASETS ###
###############################################

### Individual districts
df_doses_i_annual = bind_rows(df_df_elderly%>%mutate(strategy = "elderly"),
                             df_df_children%>%mutate(strategy = "children"),
                             df_df_wcba%>%mutate(strategy = "wcba"),
                             df_df_adults%>%mutate(strategy = "adults"),
                             df_df_combined%>%mutate(strategy = "combined"))

df_doses_i_cumul = df_doses_i_annual%>%
  group_by(Country, GID_0, Region, GID_1, strategy)%>%
  summarise(doses = sum(doses), doses_disc = sum(doses_disc))

### By country combined
df_doses_byCountry_annual = df_doses_i_annual%>%
  group_by(Country, Year, strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

df_doses_byCountry_cumul = df_doses_i_annual%>%
  group_by(Country, strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

### All districts combined
df_doses_allDistricts_annual = df_doses_i_annual%>%
  group_by(Year, strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

df_doses_allDistricts_cumul = df_doses_i_cumul%>%
  group_by(strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

### Medium and high burden districts
df_doses_medDistricts_annual = df_doses_i_annual%>%
  filter(GID_1 %in% vec_GID_1_medDistricts)%>%
  group_by(Year, strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

df_doses_medDistricts_cumul = df_doses_i_cumul%>%
  filter(GID_1 %in% vec_GID_1_medDistricts)%>%
  group_by(strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))



### High burden districts only
df_doses_highDistricts_annual = df_doses_i_annual%>%
  filter(GID_1 %in% vec_GID_1_highDistricts)%>%
  group_by(Year, strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

df_doses_highDistricts_cumul = df_doses_i_cumul%>%
  filter(GID_1 %in% vec_GID_1_highDistricts)%>%
  group_by(strategy)%>%
  summarise(doses = sum(doses),
            doses_disc = sum(doses_disc))

### SAVE FILES
save(df_doses_i_annual, file = "df_doses_i_annual.Rdata")
save(df_doses_i_cumul, file = "df_doses_i_cumul.Rdata")

save(df_doses_allDistricts_annual, file = "df_doses_allDistricts_annual.Rdata")
save(df_doses_allDistricts_cumul, file = "df_doses_allDistricts_cumul.Rdata")

save(df_doses_medDistricts_annual, file = "df_doses_medDistricts_annual.Rdata")
save(df_doses_medDistricts_cumul, file = "df_doses_medDistricts_cumul.Rdata")

save(df_doses_highDistricts_annual, file = "df_doses_highDistricts_annual.Rdata")
save(df_doses_highDistricts_cumul, file = "df_doses_highDistricts_cumul.Rdata")
