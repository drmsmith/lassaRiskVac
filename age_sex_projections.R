####################
### SET FILEPATH ###
####################

filepath = this.path::here()
setwd(filepath)

########################
### RUN HOUSEKEEPING ###
########################

source("housekeeping.R")

render_plots = F

#############################################################################
### UN POPULATION SIZES IN 2019 FOR CALCULATION OF AGE-SPECIFIC INCIDENCE ###
#############################################################################

df_pop_historic_male = read_excel("parameters_data/WPP2024_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx", 
                                     skip = 16, col_types = "text", sheet = "Estimates")
df_pop_historic_female = read_excel("parameters_data/WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx", 
                                       skip = 16, col_types = "text", sheet = "Estimates")

### Filter countries and rename variables
df_pop_2019_male_vacc = df_pop_historic_male%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2019)

df_pop_2019_female_vacc = df_pop_historic_female%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2019)

### Reorganize ages
df_pop_2019_male_vacc_grouped = df_pop_2019_male_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "UN_scaled")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         UN_scaled_num = as.numeric(UN_scaled)*1000)%>%
  mutate(ag = case_when(Age_num >= 80 ~ "80",
                        T ~ Age))%>%
  group_by(Country, GID_0, Year, ag)%>%
  summarise(UN_scaled = sum(UN_scaled_num))%>%
  ungroup()%>%
  mutate(Age = as.numeric(ag),
         Sex = "Male")%>%
  dplyr::select(-ag)


df_pop_2019_female_vacc_grouped = df_pop_2019_female_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "UN_scaled")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         UN_scaled_num = as.numeric(UN_scaled)*1000)%>%
  mutate(ag = case_when(Age_num >= 80 ~ "80",
                        T ~ Age))%>%
  group_by(Country, GID_0, Year, ag)%>%
  summarise(UN_scaled = sum(UN_scaled_num))%>%
  ungroup()%>%
  mutate(Age = as.numeric(ag),
         Sex = "Female")%>%
  dplyr::select(-ag)



df_population_age_sex_2019 = bind_rows(df_pop_2019_male_vacc_grouped,
                                       df_pop_2019_female_vacc_grouped)


save(df_population_age_sex_2019, file = "df_population_age_sex_2019.Rdata")

#################################################################
### Preparing UN projections of age and sex from 2025 to 2037 ###
#################################################################

### NB: WD, packages and country list already loaded from infection_weighting.R

### Load data
df_pop_projections_male = read_excel("parameters_data/WPP2024_POP_F01_2_POPULATION_SINGLE_AGE_MALE.xlsx", 
                                skip = 16, col_types = "text", sheet = "Medium variant")
df_pop_projections_female = read_excel("parameters_data/WPP2024_POP_F01_3_POPULATION_SINGLE_AGE_FEMALE.xlsx", 
                                     skip = 16, col_types = "text", sheet = "Medium variant")

### Filter countries and rename variables
df_pop_projections_male_vacc = df_pop_projections_male%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2025:2037)

df_pop_projections_female_vacc = df_pop_projections_female%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2025:2037)


### Reorganize ages
df_pop_projections_male_vacc_pregrouped = df_pop_projections_male_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "UN_scaled")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         UN_scaled_num = as.numeric(UN_scaled)*1000)%>%
  mutate(ag = case_when(Age_num >= 80 ~ "80",
                        T ~ Age))

df_pop_projections_male_vacc_grouped = df_pop_projections_male_vacc_pregrouped%>%
  group_by(Country, GID_0, Year, ag)%>%
  summarise(UN_scaled = sum(UN_scaled_num))%>%
  ungroup()%>%
  mutate(Age = as.numeric(ag),
         Sex = "Male")%>%
  dplyr::select(-ag)


df_pop_projections_female_vacc_pregrouped = df_pop_projections_female_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "UN_scaled")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         UN_scaled_num = as.numeric(UN_scaled)*1000)%>%
  mutate(ag = case_when(Age_num >= 80 ~ "80",
                        T ~ Age))

df_pop_projections_female_vacc_grouped = df_pop_projections_female_vacc_pregrouped%>%
  group_by(Country, GID_0, Year, ag)%>%
  summarise(UN_scaled = sum(UN_scaled_num))%>%
  ungroup()%>%
  mutate(Age = as.numeric(ag),
         Sex = "Female")%>%
  dplyr::select(-ag)



df_population_age_sex_year = bind_rows(df_pop_projections_male_vacc_grouped,
                                 df_pop_projections_female_vacc_grouped)


save(df_population_age_sex_year, file = "df_population_age_sex_year.Rdata")


#####################################################################
### Preparing UN projections of life expectancy from 2025 to 2040 ###
#####################################################################

### NB: WD, packages and country list already loaded from infection_weighting.R

### Load data
df_le_projections_male = read_excel("parameters_data/WPP2024_MORT_F05_2_LIFE_EXPECTANCY_BY_AGE_MALE.xlsx", 
                                     skip = 16, col_types = "text", sheet = "Medium variant")
df_le_projections_female = read_excel("parameters_data/WPP2024_MORT_F05_3_LIFE_EXPECTANCY_BY_AGE_FEMALE.xlsx", 
                                       skip = 16, col_types = "text", sheet = "Medium variant")


### Filter countries and rename variables
df_le_projections_male_vacc = df_le_projections_male%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2025:2037)

df_le_projections_female_vacc = df_le_projections_female%>%
  rename(GID_0 = `ISO3 Alpha-code`,
         Country = `Region, subregion, country or area *`)%>%
  dplyr::filter(GID_0 %in% GID_0_final,
                Year %in% 2025:2037)


### Reorganize ages, male
df_le_projections_male_vacc_pregrouped = df_le_projections_male_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "life_exp_at_age_x")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         life_exp_at_age_x = as.numeric(life_exp_at_age_x))

### Reorganize ages, male, 80 to 100 and merge with population size to determine weighted average life expectancy
df_le_projections_male_vacc_grouped_80plus = df_le_projections_male_vacc_pregrouped%>%
  filter(Age_num >= 80)%>%
  left_join(., df_pop_projections_male_vacc_pregrouped%>%
              filter(Age_num >=80),
            by = c("Country", "GID_0", "Year", "Age", "Age_num"))%>%
  group_by(Country, GID_0, Year)%>%
  summarise(life_exp_at_age_x = weighted.mean(life_exp_at_age_x, UN_scaled_num))%>%
  mutate(Age = "80",
         Age_num = 80)

df_le_projections_male_vacc_grouped = bind_rows(df_le_projections_male_vacc_pregrouped%>%
              filter(Age_num < 80), 
            df_le_projections_male_vacc_grouped_80plus)%>%
  mutate(Sex = "Male")%>%
  arrange(Country, GID_0, Year, Age_num)


### Reorganize ages, female
df_le_projections_female_vacc_pregrouped = df_le_projections_female_vacc%>%
  dplyr::select(-c("Index", "Variant", "Notes", "Location code", "ISO2 Alpha-code", "SDMX code**", "Type", "Parent code"))%>%
  pivot_longer(-c(Country, GID_0, Year), names_to = "Age", values_to = "life_exp_at_age_x")%>%
  mutate(Age = case_when(Age == "100+" ~ "101",
                         T ~ Age))%>%
  mutate(Age_num = as.numeric(Age),
         life_exp_at_age_x = as.numeric(life_exp_at_age_x))

### Reorganize ages, male, 80 to 100 and merge with population size to determine weighted average life expectancy
df_le_projections_female_vacc_grouped_80plus = df_le_projections_female_vacc_pregrouped%>%
  filter(Age_num >= 80)%>%
  left_join(., df_pop_projections_female_vacc_pregrouped%>%
              filter(Age_num >=80),
            by = c("Country", "GID_0", "Year", "Age", "Age_num"))%>%
  group_by(Country, GID_0, Year)%>%
  summarise(life_exp_at_age_x = weighted.mean(life_exp_at_age_x, UN_scaled_num))%>%
  mutate(Age = "80",
         Age_num = 80)

df_le_projections_female_vacc_grouped = bind_rows(df_le_projections_female_vacc_pregrouped%>%
                                                  filter(Age_num < 80), 
                                                df_le_projections_female_vacc_grouped_80plus)%>%
  mutate(Sex = "Female")%>%
  arrange(Country, GID_0, Year, Age_num)


df_life_exp_age_sex_year = bind_rows(df_le_projections_male_vacc_grouped,
                                     df_le_projections_female_vacc_grouped)


save(df_life_exp_age_sex_year, file = "df_life_exp_age_sex_year.Rdata")
