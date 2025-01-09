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

############################
### BASE POPULATION DATA ###
############################

################################
### Load population pyramids ###
################################

### 2023 POPULATION DATA: WorldPop_Scaled from worldpop rasters
df_population_worldpop = read_excel("parameters_data/Regional Population Pyramids LF No_Formulae.xlsx", sheet = 2)

df_GID_1_regions = unique(df_population_worldpop[c("Country", "GID_0", "Region", "GID_1")])

df_pregnancy_proportion = df_population_worldpop%>%
  filter(Sex == "Preg_Female")%>%
  dplyr::select(c(Country, GID_0, Region, GID_1, Age, `Pregnancy Proportion`))%>%
  rename(PregProp = `Pregnancy Proportion`)%>%
  mutate(PregProp = as.numeric(PregProp))

### CHOICE FOR POPULATION DATA: UN projections allowing for evolution through time

### BASELINE 2019 DATA (FOR CALCULATING INCIDENCE)
df_population_UN_2019 = loadRData("demography/df_population_age_sex_2019.Rdata") 

### POPULATION PROJECTIONS
df_population_UN = loadRData("demography/df_population_age_sex_year.Rdata")



#######################################
### SUBNATIONAL POPULATION PYRAMIDS ###
#######################################

################################################################
### PROPORTION OF PROJECTED POPULATION IN SELECTED DISTRICTS ###
################################################################

### UN population projections are not subnational
### So to scale from national to subnational, take WorldPop subnational estimates 
### (the proportion of each country's population in each district) and apply that
### proportion to UN country-level estimates to get subnational population projections
### Limitation: assuming the national-level age-sex pyramid is identical across a given country's districts

### The proportion of each country's population in each district (from rastered WorldPop data)
# limited to selected districts
df_pop_per_district = read.csv("RasterOutput/pop_per_district_UNAdj.csv")%>%
  rename(GID_0 = country)%>%
  filter(GID_1 %in% GID_1_final)%>%
  dplyr::select(GID_0, GID_1, proportion_subnational)


############
### 2019 ###
############

df_population_UN_subnational_2019 = df_population_UN_2019%>%
  full_join(., df_pop_per_district, by = "GID_0", relationship = "many-to-many")%>%
  arrange(Country, Year, Sex, Age)%>%
  mutate(UN_scaled_subnational = UN_scaled*proportion_subnational,
         Year = as.numeric(Year))%>%
  full_join(., df_GID_1_regions, by = c("Country", "GID_0", "GID_1"))

###################
### PROJECTIONS ###
###################

### Account for pregnancy status in these to carry forward

### Combine country-level UN data with available districts
df_population_UN_subnational_MF = df_population_UN%>%
  full_join(., df_pop_per_district, by = "GID_0", relationship = "many-to-many")%>%
  arrange(Country, Year, Sex, Age)%>%
  mutate(UN_scaled_subnational = UN_scaled*proportion_subnational,
         Year = as.numeric(Year))%>%
  full_join(., df_GID_1_regions, by = c("Country", "GID_0", "GID_1"))

df_population_UN_subnational_pregFemale = df_population_UN_subnational_MF%>%
  filter(Sex == "Female")%>%
  left_join(., df_pregnancy_proportion, by = c("Country", "GID_0", "Region", "GID_1", "Age"))%>%
  mutate(UN_scaled_subnational = UN_scaled_subnational*PregProp,
         Sex = "Preg_Female")

df_population_UN_subnational_notPregFemale = df_population_UN_subnational_MF%>%
  filter(Sex == "Female")%>%
  left_join(., df_pregnancy_proportion, by = c("Country", "GID_0", "Region", "GID_1", "Age"))%>%
  mutate(UN_scaled_subnational = UN_scaled_subnational*(1-PregProp),
         Sex = "Not_Preg_Female")

### check that sum of preg and not preg female equals total female
check_pop_female = sum(df_population_UN_subnational_MF%>%filter(Year == 2025, Sex == "Female")%>%dplyr::select(UN_scaled_subnational))
check_pop_notpreg = sum(df_population_UN_subnational_notPregFemale%>%filter(Year == 2025)%>%dplyr::select(UN_scaled_subnational))
check_pop_preg = sum(df_population_UN_subnational_pregFemale%>%filter(Year == 2025)%>%dplyr::select(UN_scaled_subnational))
stopifnot((check_pop_notpreg + check_pop_preg) - check_pop_female < 1)


df_population_UN_subnational = bind_rows(df_population_UN_subnational_MF,
                                         df_population_UN_subnational_pregFemale,
                                         df_population_UN_subnational_notPregFemale)

############################################################
### Sense check: are WorldPop and UN numbers comparable? ###
############################################################

totals_worldpop = df_population_worldpop%>%
  filter(Sex == "Combined")%>%
  group_by(Country)%>%
  summarise(total = sum(WorldPop_Scaled))

totals_UN = df_population_UN_subnational_MF%>%
  filter(Year == 2025)%>%
  group_by(Country)%>%
  summarise(total = sum(UN_scaled_subnational))

################################
### Plot population pyramids ###
################################

if(render_plots){
  
  for(GID_1_i in GID_1_final){

    ### 2025 ###
    # full pyramid
    p_pop_pyramids_full = df_population_UN_subnational%>%
      filter(Sex %in% c("Male", "Female"),
             GID_1 %in% GID_1_i,
             Year == 2025)%>%
      mutate(pop_adj = ifelse(Sex == "Male", UN_scaled_subnational, -UN_scaled_subnational),
             Region_Country = paste0(Region, ", ", Country, " (", GID_1,")"))%>%
      ggplot(., aes(y = pop_adj/1000, x = Age, fill = Sex))+
      geom_bar(stat = "identity", colour = "black")+
      facet_wrap(facets = vars(Region_Country), scales = "free")+
      theme_classic()+
      scale_y_continuous(labels = abs)+
      scale_fill_manual(values = sex_cols_map)+
      ylab("Population (thousands)")+xlab("Age")+
      coord_flip()
    
    ggsave(p_pop_pyramids_full, file = paste0("p_pop_pyramids_full_", GID_1_i,".png"),
           width = 10, height = 10, units = "cm")
    
    # change through time
    p_pop_change = df_population_UN_subnational%>%
      filter(Sex %in% c("Male", "Female"),
             GID_1 %in% GID_1_i,
             Age %in% seq(0, 90, by = 15))%>%
      mutate(Age = factor(Age),
             Region_Country = paste0(Region, ", ", Country, " (", GID_1,")"))%>%
      ggplot(., aes(y = UN_scaled_subnational , x = Year, colour = Age))+
      geom_line()+
      facet_grid(rows = vars(Region_Country), cols = vars(Sex), scales = "free")+
      theme_bw()+
      scale_y_continuous(labels = comma, limits = c(0, NA))+
      scale_x_continuous(breaks = c(2025, 2029, 2033, 2037))+
      scale_colour_manual("Age band", values = ag_cols)+
      ylab("Population size")+xlab("Year")
    
    ggsave(p_pop_change, file = paste0("p_pop_change_", GID_1_i,".png"),
           width = 18, height = 8, units = "cm")
  }
}


#################################
### ENABLE POPULATION PYRAMID ###
#################################

############
### 2019 ###
############

df_population_ag_enable_2019 = df_population_UN_subnational_2019%>%
  mutate(ag = case_when(Age >= age_group_enable_mins[1] & Age < age_group_enable_mins[2] ~ age_groups_enable[1],
                        Age >= age_group_enable_mins[2] & Age < age_group_enable_mins[3] ~ age_groups_enable[2],
                        Age >= age_group_enable_mins[3] & Age < age_group_enable_mins[4] ~ age_groups_enable[3],
                        Age >= age_group_enable_mins[4] & Age < age_group_enable_mins[5] ~ age_groups_enable[4],
                        Age >= age_group_enable_mins[5] & Age < age_group_enable_mins[6] ~ age_groups_enable[5],
                        Age >= age_group_enable_mins[6] ~ age_groups_enable[6]))

### Calculate population by region
df_population_region_enable_2019 = df_population_ag_enable_2019%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(UN_scaled_subnational  = sum(UN_scaled_subnational))

### Calculate population sizes by region-age-sex groups
df_population_sex_ag_enable_summarised_2019 = df_population_ag_enable_2019%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Sex, Year, ag)%>%
  summarise(UN_scaled_subnational  = sum(UN_scaled_subnational))

###################
### PROJECTIONS ###
###################

df_population_ag_enable = df_population_UN_subnational%>%
  mutate(ag = case_when(Age >= age_group_enable_mins[1] & Age < age_group_enable_mins[2] ~ age_groups_enable[1],
                        Age >= age_group_enable_mins[2] & Age < age_group_enable_mins[3] ~ age_groups_enable[2],
                        Age >= age_group_enable_mins[3] & Age < age_group_enable_mins[4] ~ age_groups_enable[3],
                        Age >= age_group_enable_mins[4] & Age < age_group_enable_mins[5] ~ age_groups_enable[4],
                        Age >= age_group_enable_mins[5] & Age < age_group_enable_mins[6] ~ age_groups_enable[5],
                        Age >= age_group_enable_mins[6] ~ age_groups_enable[6]))

### Calculate population by region
df_population_region_enable = df_population_ag_enable%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(UN_scaled_subnational  = sum(UN_scaled_subnational ))

### Calculate population sizes by region-age-sex groups
df_population_sex_ag_enable_summarised = df_population_ag_enable%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Sex, Year, ag)%>%
  summarise(UN_scaled_subnational  = sum(UN_scaled_subnational))

################################
### FINAL POPULATION PYRAMID ###
################################

if(length(age_groups_final) != 6){warning("STOP! Need to update infection age allocation")}

############
### 2019 ###
############

### Add age groups to population
df_population_ag_2019 = df_population_UN_subnational_2019%>%
  mutate(ag = case_when(Age >= age_group_mins[1] & Age < age_group_mins[2] ~ age_groups_final[1],
                        Age >= age_group_mins[2] & Age < age_group_mins[3] ~ age_groups_final[2],
                        Age >= age_group_mins[3] & Age < age_group_mins[4] ~ age_groups_final[3],
                        Age >= age_group_mins[4] & Age < age_group_mins[5] ~ age_groups_final[4],
                        Age >= age_group_mins[5] & Age < age_group_mins[6] ~ age_groups_final[5],
                        Age >= age_group_mins[6] ~ age_groups_final[6]))

###################
### PROJECTIONS ###
###################

### Add age groups to population
df_population_ag = df_population_UN_subnational%>%
  mutate(ag = case_when(Age >= age_group_mins[1] & Age < age_group_mins[2] ~ age_groups_final[1],
                        Age >= age_group_mins[2] & Age < age_group_mins[3] ~ age_groups_final[2],
                        Age >= age_group_mins[3] & Age < age_group_mins[4] ~ age_groups_final[3],
                        Age >= age_group_mins[4] & Age < age_group_mins[5] ~ age_groups_final[4],
                        Age >= age_group_mins[5] & Age < age_group_mins[6] ~ age_groups_final[5],
                        Age >= age_group_mins[6] ~ age_groups_final[6]))


### Calculate population by region
df_population_region = df_population_ag%>%
  filter(Sex %in% c("Male", "Preg_Female", "Not_Preg_Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Year)%>%
  summarise(UN_scaled_subnational = sum(UN_scaled_subnational))

### Calculate population sizes by region-age-sex groups
df_population_sex_ag_summarised = df_population_ag%>%
  filter(Sex %in% c("Male", "Preg_Female", "Not_Preg_Female"))%>%
  group_by(Country, GID_0, Region, GID_1, Sex, ag, Year)%>%
  summarise(UN_scaled_subnational = sum(UN_scaled_subnational))%>%
  mutate(Sex = factor(Sex, levels = c("Male", "Not_Preg_Female", "Preg_Female"), labels = sex_preg_labels))

### Determine proportion of each age group constituted by each age
df_population_ag_propWithin = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"))%>%
  dplyr::select(Age, Country, GID_0, Region, GID_1, Sex, ag, Year, UN_scaled_subnational)%>%
  group_by(Country, GID_0, Region, GID_1, Sex, ag, Year)%>%
  mutate(ag_propWithin = UN_scaled_subnational/sum(UN_scaled_subnational))



##################################################
### Adjust population to account for pregnancy ###
##################################################

df_PregProp = df_population_ag%>%
  filter(Sex %in% c("Male", "Preg_Female"),
         Year == 2025)%>%
  mutate(PregProp = ifelse(is.na(PregProp), 0, PregProp))%>%
  dplyr::select(Age, Country, GID_0, Region, GID_1, Sex, PregProp)%>%
  mutate(Sex = case_when(Sex == "Preg_Female" ~ "Female",
                         T ~ "Male"),
         PregProp = as.numeric(PregProp))

df_population_ag_PregProp = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"))%>%
  dplyr::select("Country", "GID_0", "Region", "GID_1", "Age", "ag", "Sex", "Year", "UN_scaled_subnational")%>%
  left_join(., df_PregProp, by = c("Country", "GID_0", "Region", "GID_1", "Age", "Sex"))%>%
  mutate(N_pregnant = UN_scaled_subnational * PregProp)%>%
  filter(Sex %in% c("Male", "Female"))

df_population_finalAges = df_population_ag_PregProp%>%
  group_by(Country, GID_0, Region, GID_1, Sex, ag, Year)%>%
  summarise(UN_scaled_subnational  = sum(UN_scaled_subnational),
            N_pregnant = sum(N_pregnant),
            PropPregnant = N_pregnant/UN_scaled_subnational)%>%
  mutate(ag = factor(ag, levels = age_groups_final))%>%
  ungroup()


df_PropPregnant = df_population_finalAges%>%
  filter(Year == 2025)%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, ag, PropPregnant)


# check population totals match#
check_pop_pre = sum(df_population_UN_subnational%>%filter(Year == 2025, Sex %in% c("Male", "Female"))%>%dplyr::select(UN_scaled_subnational))
check_pop_post = sum(df_population_finalAges%>%dplyr::filter(Year == 2025)%>%dplyr::select(UN_scaled_subnational))
stopifnot(abs(check_pop_pre - check_pop_post) < 1)

# check sex totals match
check_male_pre = sum(df_population_UN_subnational%>%filter(Sex == "Male")%>%dplyr::select(UN_scaled_subnational))
check_male_post = sum(df_population_finalAges%>%filter(Sex == "Male")%>%ungroup()%>%dplyr::select(UN_scaled_subnational))
stopifnot(abs(check_male_pre - check_male_post) < 1)

check_female_pre = sum(df_population_UN_subnational%>%filter(Sex == "Female")%>%dplyr::select(UN_scaled_subnational))
check_female_post = sum(df_population_finalAges%>%filter(Sex == "Female")%>%ungroup()%>%dplyr::select(UN_scaled_subnational))
stopifnot(abs(check_female_pre - check_female_post) < 1)

# check pregnant numbers match
check_preg_pre = sum(df_population_UN_subnational%>%filter(Sex == "Preg_Female")%>%dplyr::select(UN_scaled_subnational))
check_preg_post = sum(df_population_finalAges$N_pregnant)
stopifnot(abs(check_preg_pre - check_preg_post) < 1)



#########################################
### Plot adjusted population pyramids ###
#########################################

if(render_plots){
  
  p_age_pyramids_adj = df_population_finalAges%>%
    filter(Sex %in% c("Male", "Female"),
           Year == 2025)%>%
    mutate(pop_adj = ifelse(Sex == "Male", UN_scaled_subnational, -UN_scaled_subnational),
           region_country = paste0(Region, ",\n", Country))%>%
    ggplot(., aes(y = pop_adj, x = ag, fill = Sex))+
    geom_hline(yintercept = 0)+
    geom_bar(stat = "identity", colour = "black")+
    facet_wrap(facets = vars(region_country))+
    theme_bw()+
    scale_y_continuous(labels = f <- function(x) paste0(abs(x)/1000000, " M"))+
    scale_fill_manual(values = sex_cols_map)+
    ylab("Population in 2025")+xlab("Age group")+
    coord_flip()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = c(4.5/5, 0.25/2))
  
  ggsave(p_age_pyramids_adj, filename = "p_age_pyramids_adjusted.png",
         width = 20, height = 18, units = "cm")
  
  p_age_pyramids_adj_free = df_population_finalAges%>%
    filter(Sex %in% c("Male", "Female"),
           Year == 2025)%>%
    mutate(pop_adj = ifelse(Sex == "Male", UN_scaled_subnational, -UN_scaled_subnational),
           region_country = paste0(Region, ",\n", Country))%>%
    ggplot(., aes(y = pop_adj, x = ag, fill = Sex))+
    geom_bar(stat = "identity", colour = "black")+
    facet_wrap(facets = vars(region_country), scales = "free")+
    theme_bw()+
    scale_y_continuous(labels = f <- function(x) paste0(abs(x)/1000000, " M"))+
    scale_fill_manual(values = sex_cols_map)+
    ylab("Population in 2025")+xlab("Age group")+
    coord_flip()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = c(4.5/5, 0.25/2))
  
  ggsave(p_age_pyramids_adj_free, filename = "p_age_pyramids_adjusted_free.png",
         width = 20, height = 18, units = "cm")
  
  
  p_prop_pregnant = df_PropPregnant%>%
    filter(GID_1 %in% c("GIN.8_1", "LBR.5_1", "NGA.5_1", "SLE.1_1"))%>%
    ggplot(., aes(x = ag, y = PropPregnant, fill = ag))+
    geom_bar(stat = "identity", colour = "black")+
    theme_bw()+
    xlab("Age group")+ylab("Proportion of females with live birth annually")+
    scale_fill_manual(values = ag_cols_map)+
    facet_wrap(facets = vars(Country), ncol = 4)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_prop_pregnant, filename = "p_prop_pregnant_adjusted.png",
         width = 16, height = 10, units = "cm")
  
}


#################
### SAVE DATA ###
#################

############
### 2019 ###
############

### FULL POPULATION DATA
df_population_2019 = df_population_UN_subnational_2019
save(df_population_2019, file = "df_population_2019.Rdata")

### POPULATION DATA, AGE-STRATIFIED TO ENABLE AGE GROUPS
save(df_population_sex_ag_enable_summarised_2019, file = "df_population_sex_ag_enable_summarised_2019.Rdata")

### POPULATION DATA AG: ALL AGES + FINAL AGE GROUPS
save(df_population_ag_2019, file = "df_population_ag_2019.Rdata")

###################
### PROJECTIONS ###
###################
### FULL POPULATION DATA
df_population = df_population_UN_subnational
save(df_population, file = "df_population.Rdata")

### FINAL DISTRICTS INCLUDED IN FULL POPULATION
save(GID_1_final, file = "GID_1_final.Rdata")

### POPULATION DATA, AGE-STRATIFIED TO ENABLE AGE GROUPS
save(df_population_sex_ag_enable_summarised, file = "df_population_sex_ag_enable_summarised.Rdata")

### POPULATION DATA AG: ALL AGES + FINAL AGE GROUPS
save(df_population_ag, file = "df_population_ag.Rdata")

### POPULATION DATA AG + PROPORTION OF FINAL AGE GROUPS REPRESENTED BY EACH AGE
save(df_population_ag_propWithin, file = "df_population_ag_propWithin.Rdata")

### POPULATION DATA AG + PREGNANCY PROPORTION AND NUMBER
save(df_population_ag_PregProp, file = "df_population_ag_PregProp.Rdata")

### POPULATION DATA IN FINAL AGE GROUPS
save(df_population_finalAges, file = "df_population_finalAges.Rdata")

### PREGNANCY PROPORTION AMONG FINAL AGE GROUPS
save(df_PropPregnant, file = "df_PropPregnant.Rdata")

