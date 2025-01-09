
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
render_tables = F

#############################
### LOAD DEMOGRAPHIC DATA ###
#############################

files_demography = list.files("demography/")
for(file_i in files_demography){load(paste0("demography/", file_i))}

######################################################
### LOAD AGE-STRUCTURED SEROPREVALENCE FROM ENABLE ###
######################################################

df_seroprevalence_enable = read_excel("parameters_data/ENABLE_seroprevalence.xlsx", range = "A1:I7")


##############################################################################################
### Load in posterior draws of age-specific FOI calculated from Enable seroprevalence data ###
##############################################################################################

df_foi = read.csv("infections/foi_posterior_draws.csv")%>%
  rename("<5" = "X.5",
         "5-17" = "X5.17",
         "18-24" = "X18.24",
         "25-34" = "X25.34",
         "35-49" = "X35.49",
         "50+" = "X50.")%>%
  mutate(n_draw = row_number())%>%
  pivot_longer(-n_draw, names_to = "ag_enable",
               values_to = "FOI")%>%
  mutate(ag_enable = factor(ag_enable,
                     levels = age_groups_enable))

### PLOT ###

if(render_plots){
  p_foi = df_foi%>%
    filter(n_draw <= 500)%>%
    ggplot(., aes(x = ag_enable, y = FOI, colour = ag_enable))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(outlier.shape = NA, alpha = 0.1, lwd = 0.3)+
    geom_jitter(height = 0, width = 0.25, alpha = 0.3, size = 0.3)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+ylim(0, NA)+
    ylab("Force of infection")+xlab("Age group")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p_foi
  
  ggsave(p_foi, file = "p_foi.png", width = 10, height = 12, units = "cm")
}


### SUMMARY TABLE ###
if(render_tables){
  
  t_foi = df_foi%>%
    group_by(ag_enable)%>%
    summarise(mean = mean(FOI),
              median = median(FOI),
              q025 = quantile(FOI, 0.025),
              q250 = quantile(FOI, 0.25),
              q750 = quantile(FOI, 0.75),
              q975 = quantile(FOI, 0.975))
  t_foi
  
  write.csv(t_foi, file = "t_foi.csv")
}

##########################################################################################
### Load in posterior draws of age-specific seroprevalence calculated from Enable data ###
##########################################################################################

df_seroprev = read.csv("infections/seroprevalence_posterior_draws.csv")%>%
  rename("<5" = "X.5",
         "5-17" = "X5.17",
         "18-24" = "X18.24",
         "25-34" = "X25.34",
         "35-49" = "X35.49",
         "50+" = "X50.")%>%
  mutate(n_draw = row_number())%>%
  pivot_longer(-n_draw, names_to = "ag_enable",
               values_to = "seroprev")%>%
  mutate(ag_enable = factor(ag_enable,
                     levels = age_groups_enable))

### PLOT ###
if(render_plots){
  p_seroprev = df_seroprev%>%
    filter(n_draw <= 500)%>%
    ggplot(., aes(x = ag_enable, y = seroprev, colour = ag_enable))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(outlier.shape = NA, alpha = 0.1, lwd = 0.3)+
    geom_jitter(height = 0, width = 0.25, alpha = 0.3, size = 0.3)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+ylim(0, NA)+
    ylab("Proportion LASV seropositive")+xlab("Age group")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p_seroprev
  
  ggsave(p_seroprev, file = "p_seroprev.png", width = 10, height = 12, units = "cm")
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  t_seroperv = df_seroprev%>%
    group_by(ag_enable)%>%
    summarise(mean = mean(seroprev),
              median = median(seroprev),
              q025 = quantile(seroprev, 0.025),
              q250 = quantile(seroprev, 0.25),
              q750 = quantile(seroprev, 0.75),
              q975 = quantile(seroprev, 0.975))
  t_seroperv
  
  write.csv(t_seroperv, file = "t_seroperv.csv")
}

#######################################
### JOINT SEROPREVALENCE / FOI PLOT ###
#######################################

if(render_plots){
  p_seroprev_foi = plot_grid(p_seroprev, p_foi,
                             ncol = 2, labels = c("A", "B"),
                             align = "hv", axis = "tblr")
  p_seroprev_foi
  
  ggsave(p_seroprev_foi, file = "p_seroprev_foi.png",
         width = 16, height = 10, units = "cm")
}


########################################################
### Bind each FOI draw to mean ENABLE seroprevalence ###
########################################################

df_foi_sero = df_foi%>%
  left_join(., df_seroprev, by = c("ag_enable", "n_draw"))
  

#############################################################################
### Bind to 2019 population to calculate infections in each age-sex group ###
#############################################################################

df_foi_sero_infec = df_population_sex_ag_enable_summarised_2019%>%
  rename(ag_enable = ag)%>%
  left_join(., df_foi_sero, by = "ag_enable", 
            relationship = "many-to-many")%>%
  mutate(N_susceptible = UN_scaled_subnational * (1-seroprev))%>%
  mutate(N_infections_foi = FOI * N_susceptible)%>%
  mutate(ag_enable = factor(ag_enable,
                            levels = age_groups_enable))


#################################################
### Load in Basinski map-predicted infections ###
#################################################

list_infections_total_annual = loadRData("infections/list_Lassa_00_byDistrict_annual.Rdata")

df_infections_total_annual = do.call(rbind, list_infections_total_annual)%>%
  rename(GID_1 = catchmentID)%>%
  filter(GID_1 %in% GID_1_final,
         realyear == 1, scenario == 1)

### for each district, resample number of infections from t-distributed centiles
### to match number of FOI posterior draws

vec_draws_posterior = unique(df_foi_sero_infec$n_draw)
n_draws_posterior = length(vec_draws_posterior)

# empty dataframe to fill
df_spillover_draws = data.frame()

### Loop through districts and resample 
for(GID_1_i in GID_1_final){
  
  df_infections_total_annual_i = df_infections_total_annual%>%
    filter(GID_1 == GID_1_i)
  
  vec_spillover = sample(x = df_infections_total_annual_i$spillover, 
                         size = n_draws_posterior, 
                         replace = T)
  
  df_spillover_draws_i = data.frame(GID_1 = GID_1_i,
                                    n_draw = vec_draws_posterior,
                                    spillover = vec_spillover)
  
  df_spillover_draws = bind_rows(df_spillover_draws, df_spillover_draws_i)
  
}


if(render_plots){
  
  ### example plot from one district (risk map)
  p_spillover_exampleDistrict =   df_infections_total_annual%>%
    filter(GID_1 == "GIN.8_1")%>%
    ggplot(., aes(x = spillover))+
    geom_histogram(fill = "white", colour = "black")+
    theme_bw()+
    scale_x_continuous(label = comma)
  
  ### example plot from one district (resampled)
  p_spillover_exampleDistrict_resampled = df_spillover_draws%>%
    filter(GID_1 == "GIN.8_1")%>%
    ggplot(., aes(x = spillover))+
    geom_histogram(fill = "white", colour = "black")+
    theme_bw()+
    scale_x_continuous(label = comma)
  
  p_spillover_allDistricts = df_infections_total_annual%>%
    ggplot(., aes(x = spillover, fill = GID_1))+
    geom_density(alpha = 0.1)+
    theme_bw()+
    scale_x_continuous(label = comma)+
    theme(legend.position = "none")
}



#################################################################################
### Bind seroprevalence fits to Basinski-predicted infections to scale totals ###
#################################################################################

### Determine proportion of infections in each age/sex group from FOI draws
df_foi_sero_infec_props = df_foi_sero_infec%>%
  group_by(Country, GID_0, Region, GID_1, Year, n_draw)%>%
  mutate(prop_infections_foi = N_infections_foi/sum(N_infections_foi))

### Test to make sure the proportion of infections across age/sex groups equals zero
df_foi_sero_infec_props_test = df_foi_sero_infec_props%>%group_by(Country, GID_0, Region, GID_1, n_draw, Year)%>%
  summarise(prop = sum(prop_infections_foi))
stopifnot(sum(df_foi_sero_infec_props_test$prop) == nrow(df_foi_sero_infec_props_test))


###################################################
### ENABLE age groups scaled to risk map totals ### 
###################################################

### Bind infection proportions from ENABLE crossed with demographic data 
### to district-specific infection totals from the risk map ###
df_foi_sero_infec_spillover = df_spillover_draws%>%
  left_join(., df_foi_sero_infec_props, by = c("GID_1", "n_draw"),
            relationship = "many-to-many")%>%
  mutate(N_infections_scaled = spillover*prop_infections_foi)%>%
  mutate(ag_enable = factor(ag_enable,
                            levels = age_groups_enable))

### Test to make sure the scaled infections match the Basinski infections
check_infections_spillover = sum(df_spillover_draws$spillover)
check_infections_scaled = sum(df_foi_sero_infec_spillover%>%filter(Year == 2019)%>%dplyr::select(N_infections_scaled))
stopifnot(check_infections_scaled == check_infections_spillover)


#######################################################
### SUMMARISE AND PLOT ENABLE vs. SCALED INFECTIONS ###
#######################################################

#########################
### ENABLE INFECTIONS ###
#########################

### Total infections by age across region
df_infections_enable_region_age = df_foi_sero_infec_spillover%>%
  filter(Year == 2019)%>%
  group_by(ag_enable, n_draw)%>%
  summarise(N_infections_foi = sum(N_infections_foi))%>%
  mutate(ag_enable = factor(ag_enable,
                            levels = age_groups_enable))

### Total infections across region
df_infections_enable_region = df_infections_enable_region_age%>%
  group_by(n_draw)%>%
  summarise(N_infections_foi = sum(N_infections_foi))

### PLOT ###
if(render_plots){
  p_foi_infections_enable_enableAges = df_infections_enable_region_age%>%
    mutate(ag_enable = factor(ag_enable, levels = age_groups_enable))%>%
    ggplot(., aes(x = ag_enable, y = N_infections_foi, colour = ag_enable, fill = ag_enable))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma)+
    xlab("Age group")+ylab("Annual LASV infections")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_foi_infections_enable_enableAges, file = "p_foi_infections_enable_enableAges.png", width = 10, height = 12, units = "cm")
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  ### By age
  t_foi_infections_enable_region_age = df_infections_enable_region_age%>%
    group_by(ag_enable)%>%
    summarise(mean = mean(N_infections_foi),
              median = median(N_infections_foi),
              q025 = quantile(N_infections_foi, 0.025),
              q250 = quantile(N_infections_foi, 0.25),
              q750 = quantile(N_infections_foi, 0.75),
              q975 = quantile(N_infections_foi, 0.975))
  
  ### Total
  t_foi_infections_enable_region = df_infections_enable_region%>%
    summarise(mean = mean(N_infections_foi),
              median = median(N_infections_foi),
              q025 = quantile(N_infections_foi, 0.025),
              q250 = quantile(N_infections_foi, 0.25),
              q750 = quantile(N_infections_foi, 0.75),
              q975 = quantile(N_infections_foi, 0.975))
  
  ### Combined
  t_foi_infections_enable = bind_rows(t_foi_infections_enable_region_age,
                                      t_foi_infections_enable_region%>%
                                        mutate(ag_enable = "Total"))
  
  write.csv(t_foi_infections_enable, file = "t_foi_infections_enable.csv")
}


#########################
### SCALED INFECTIONS ###
#########################

### Total infections by age across region
df_infections_scaled_region_age_2019 = df_foi_sero_infec_spillover%>%
  filter(Year == 2019)%>%
  group_by(ag_enable, n_draw)%>%
  summarise(N_infections_scaled = sum(N_infections_scaled))

### Total infections across region
df_infections_scaled_region_2019 = df_infections_scaled_region_age_2019%>%
  group_by(n_draw)%>%
  summarise(N_infections_scaled = sum(N_infections_scaled))

### PLOT ###
if(render_plots){
  p_foi_infections_scaled_enableAges = df_infections_scaled_region_age_2019%>%
    mutate(ag_enable = factor(ag_enable, levels = age_groups_enable))%>%
    ggplot(., aes(x = ag_enable, y = N_infections_scaled, colour = ag_enable, fill = ag_enable))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    #geom_jitter(width = 0.25, alpha = 0.1)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Age group")+ylab("Annual LASV infections (2019)")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_foi_infections_scaled_enableAges, file = "p_foi_infections_scaled_enableAges.png", width = 10, height = 12, units = "cm")
  
  
  ### Side-by-side compare ENABLE to Scaled infections ###
  p_foi_infections_enable_vs_scaled_enableAges = plot_grid(
    p_foi_infections_enable_enableAges,
    p_foi_infections_scaled_enableAges,
    ncol = 2, labels = c("A", "B"))
  
  ggsave(p_foi_infections_enable_vs_scaled_enableAges, file = "p_foi_infections_enable_vs_scaled_enableAges.png", width = 20, height = 12, units = "cm")
  
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  t_foi_infections_scaled_region_age = df_infections_scaled_region_age_2019%>%
    group_by(ag_enable)%>%
    summarise(mean = mean(N_infections_scaled),
              median = median(N_infections_scaled),
              q025 = quantile(N_infections_scaled, 0.025),
              q250 = quantile(N_infections_scaled, 0.25),
              q750 = quantile(N_infections_scaled, 0.75),
              q975 = quantile(N_infections_scaled, 0.975))
  
  t_foi_infections_scaled_region = df_infections_scaled_region_2019%>%
    summarise(mean = mean(N_infections_scaled),
              median = median(N_infections_scaled),
              q025 = quantile(N_infections_scaled, 0.025),
              q250 = quantile(N_infections_scaled, 0.25),
              q750 = quantile(N_infections_scaled, 0.75),
              q975 = quantile(N_infections_scaled, 0.975))
  
  ### Combined
  t_foi_infections_scaled = bind_rows(t_foi_infections_scaled_region_age,
                                      t_foi_infections_scaled_region%>%
                                        mutate(ag_enable = "Total"))
  
  write.csv(t_foi_infections_scaled, file = "t_foi_infections_scaled.csv")
}



##################################################
### SCALED INFECTIONS AS INCIDENCE PER 100,000 ###
##################################################

population_total_region_2019 = sum(df_population_sex_ag_enable_summarised_2019%>%ungroup()%>%filter(Year == 2019)%>%dplyr::select(UN_scaled_subnational))

df_incidence_scaled_region_age_2019 = df_infections_scaled_region_age_2019%>%
  left_join(., df_population_sex_ag_enable_summarised_2019%>%
              filter(Year == 2019)%>%
              group_by(ag)%>%
              summarise(UN_scaled_subnational = sum(UN_scaled_subnational))%>%
              rename(ag_enable = ag),
            by = "ag_enable")%>%
  mutate(infection_per100000 = N_infections_scaled/UN_scaled_subnational * 100000)%>%
  mutate(ag_enable = factor(ag_enable,
                            levels = age_groups_enable))

if(render_plots){
  p_foi_incidence_scaled_enableAges = df_incidence_scaled_region_age_2019%>%
    mutate(ag_enable = factor(ag_enable, levels = age_groups_enable))%>%
    ggplot(., aes(x = ag_enable, y = infection_per100000, colour = ag_enable, fill = ag_enable))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    #geom_jitter(width = 0.25, alpha = 0.1)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Age group")+ylab("Annual LASV infections per 100,000 (2019)")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_foi_incidence_scaled_enableAges, file = "p_foi_incidence_scaled_enableAges.png", 
         width = 10, height = 12, units = "cm")
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  t_foi_incidence_scaled_region_age = df_incidence_scaled_region_age_2019%>%
    group_by(ag_enable)%>%
    summarise(mean = mean(infection_per100000),
              median = median(infection_per100000),
              q025 = quantile(infection_per100000, 0.025),
              q250 = quantile(infection_per100000, 0.25),
              q750 = quantile(infection_per100000, 0.75),
              q975 = quantile(infection_per100000, 0.975))
  
  t_foi_incidence_scaled_region = df_incidence_scaled_region_age_2019%>%
    group_by(n_draw)%>%
    summarise(N_infections_scaled = sum(N_infections_scaled),
              UN_scaled_subnational  = sum(UN_scaled_subnational))%>%
    ungroup()%>%
    mutate(infection_per100000 = N_infections_scaled/UN_scaled_subnational * 100000)%>%
    summarise(mean = mean(infection_per100000),
              median = median(infection_per100000),
              q025 = quantile(infection_per100000, 0.025),
              q250 = quantile(infection_per100000, 0.25),
              q750 = quantile(infection_per100000, 0.75),
              q975 = quantile(infection_per100000, 0.975))
  
  ### Combined
  t_foi_incidence_scaled = bind_rows(t_foi_incidence_scaled_region_age,
                                     t_foi_incidence_scaled_region%>%
                                        mutate(ag_enable = "Total"))
  
  write.csv(t_foi_incidence_scaled, file = "t_foi_incidence_scaled.csv")
}


##############################################
### PLOT COMPARING INFECTIONS vs INCIDENCE ###
##############################################

if(render_plots){
  p_foi_infections_incidence_scaled_enableAges = plot_grid(p_foi_infections_scaled_enableAges, 
                                    p_foi_incidence_scaled_enableAges,
                                    ncol = 2, labels = c("A", "B"))
  
  ggsave(p_foi_infections_incidence_scaled_enableAges, file = "p_foi_infections_incidence_scaled_enableAges.png",
         width = 20, height = 12, units = "cm")
}


#############################################
### ADJUST INFECTIONS TO FINAL AGE GROUPS ###
#############################################

#############################################
### SIMPLIFIED DATAFRAME TO CARRY FORWARD ###
#############################################

df_infections_2019_enableAges = df_foi_sero_infec_spillover%>%
  dplyr::select(Country, GID_0, Region, GID_1, Year, Sex, ag_enable, n_draw, N_infections_scaled)

##########################################
### ADJUST SCALED INFECTION AGE GROUPS ###
##########################################

# Define the mapping of infection age groups to individual ages
age_mapping_infections_enable <- list(
  "<5"  = 0:4,
  "5-17"  = 5:17,
  "18-24" = 18:24,
  "25-34" = 25:34,
  "35-49" = 35:49,
  "50+" = 50:80  # 80 is max in underlying population data
)

age_mapping_infections_final <- list(
  "<2"  = 0:1,
  "2-14"  = 2:14,
  "15-24" = 15:24,
  "25-34" = 25:34,
  "35-49" = 35:49,
  "50+" = 50:80  # 80 is max in underlying population data
)

##########################################################################
### Determine proportion of population in each infection-age-sex group ###
##########################################################################

infection_group_mins_enable = c(age_mapping_infections_enable[[1]][1],
                                age_mapping_infections_enable[[2]][1],
                                age_mapping_infections_enable[[3]][1],
                                age_mapping_infections_enable[[4]][1],
                                age_mapping_infections_enable[[5]][1],
                                age_mapping_infections_enable[[6]][1])

infection_group_mins_final = c(age_mapping_infections_final[[1]][1],
                               age_mapping_infections_final[[2]][1],
                               age_mapping_infections_final[[3]][1],
                               age_mapping_infections_final[[4]][1],
                               age_mapping_infections_final[[5]][1],
                               age_mapping_infections_final[[6]][1])


### Append age groups to population data
df_population_ag_2019_adjustAges = df_population_2019%>%
  dplyr::select(Country, GID_0, Region, GID_1, Year, Sex, Age, UN_scaled_subnational)%>%
  filter(Sex %in% c("Male", "Female"))%>%
  mutate(ag_enable = case_when(Age >= infection_group_mins_enable[1] & Age < infection_group_mins_enable[2] ~ names(age_mapping_infections_enable[1]),
                               Age >= infection_group_mins_enable[2] & Age < infection_group_mins_enable[3] ~ names(age_mapping_infections_enable[2]),
                               Age >= infection_group_mins_enable[3] & Age < infection_group_mins_enable[4] ~ names(age_mapping_infections_enable[3]),
                               Age >= infection_group_mins_enable[4] & Age < infection_group_mins_enable[5] ~ names(age_mapping_infections_enable[4]),
                               Age >= infection_group_mins_enable[5] & Age < infection_group_mins_enable[6] ~ names(age_mapping_infections_enable[5]),
                               Age >= infection_group_mins_enable[6] ~ names(age_mapping_infections_enable[6])))%>%
  mutate(ag_final = case_when(Age >= infection_group_mins_final[1] & Age < infection_group_mins_final[2] ~ names(age_mapping_infections_final[1]),
                              Age >= infection_group_mins_final[2] & Age < infection_group_mins_final[3] ~ names(age_mapping_infections_final[2]),
                              Age >= infection_group_mins_final[3] & Age < infection_group_mins_final[4] ~ names(age_mapping_infections_final[3]),
                              Age >= infection_group_mins_final[4] & Age < infection_group_mins_final[5] ~ names(age_mapping_infections_final[4]),
                              Age >= infection_group_mins_final[5] & Age < infection_group_mins_final[6] ~ names(age_mapping_infections_final[5]),
                              Age >= infection_group_mins_final[6] ~ names(age_mapping_infections_final[6])))

### Calculate proportion of each one-year age group belonging to each Enable age group
df_population_ag_adjustAges_2019_propWithinAg = df_population_ag_2019_adjustAges%>%
  group_by(Country, GID_0, Region, GID_1, Year, Sex, ag_enable)%>%
  mutate(propAg_Enable = UN_scaled_subnational/sum(UN_scaled_subnational))%>%
  ungroup()

### stop if proportions of age in each age group don't equal one
df_population_ag_adjustAges_2019_propWithinAg_propSum = df_population_ag_adjustAges_2019_propWithinAg%>%
  group_by(Country, GID_0, Region, GID_1, Year, Sex, ag_enable)%>%
  summarise(propSum = sum(propAg_Enable))
stopifnot(sum(df_population_ag_adjustAges_2019_propWithinAg_propSum$propSum) == nrow(df_population_ag_adjustAges_2019_propWithinAg_propSum))


#############################
### FINAL 2019 INFECTIONS ###
#############################

### Multiply infections from Enable age groups to one-year age groups,
df_infections_2019_allAges=df_infections_2019_enableAges%>%
  left_join(., df_population_ag_adjustAges_2019_propWithinAg,
            by = c("Country", "GID_0", "Region", "GID_1", "Year", "Sex", "ag_enable"),
            relationship = "many-to-many")%>%
  mutate(N_infection = N_infections_scaled * propAg_Enable)

### Group infections into final age groups
df_infections_2019_finalAges=df_infections_2019_allAges%>%
  group_by(Country, GID_0, Region, GID_1, Year, Sex, n_draw, ag_final)%>%
  summarise(N_infection = sum(N_infection))%>%
  rename(ag = ag_final)%>%
  mutate(ag = factor(ag, levels = age_groups_final))

### stop if the post-processed totals do not match pre-processed totals
infTotalsPre = sum(df_infections_2019_enableAges$N_infections_scaled)
infTotalsPost = sum(df_infections_2019_finalAges$N_infection)
stopifnot(abs(infTotalsPost - infTotalsPre) < 0.001)

#########################################
### SAVE FINAL 2019 INFECTION NUMBERS ###
#########################################

save(df_infections_2019_allAges, file = "df_infections_2019_allAges.Rdata")
save(df_infections_2019_finalAges, file = "df_infections_2019_finalAges.Rdata")


##########################################
### SUMMARISE FINAL INFECTIONS IN 2019 ###
##########################################

### Total annual infections by age across region
df_infections_finalAges_region_age_2019 = df_infections_2019_finalAges%>%
  group_by(Year, ag, n_draw)%>%
  summarise(N_infection = sum(N_infection))

### Total annual infections across region
df_infections_finalAges_region_2019 = df_infections_2019_finalAges%>%
  group_by(Year, n_draw)%>%
  summarise(N_infection = sum(N_infection))

### PLOT ###
if(render_plots){
  p_foi_infections_scaled_finalAges = df_infections_finalAges_region_age_2019%>%
    filter(Year == 2019)%>%
    ggplot(., aes(x = ag, y = N_infection, colour = ag, fill = ag))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Age group")+ylab("Annual LASV infections (2019)")+
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p_foi_infections_scaled_finalAges
  
  ggsave(p_foi_infections_scaled_finalAges, file = "p_foi_infections_scaled_finalAges.png", width = 10, height = 12, units = "cm")
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  t_foi_infections_finalAges_region_age_2019 = df_infections_finalAges_region_age_2019%>%
    filter(Year == 2019)%>%
    group_by(ag)%>%
    summarise(mean = mean(N_infection),
              median = median(N_infection),
              q025 = quantile(N_infection, 0.025),
              q250 = quantile(N_infection, 0.25),
              q750 = quantile(N_infection, 0.75),
              q975 = quantile(N_infection, 0.975))
  
  t_foi_infections_finalAges_region_2019 = df_infections_finalAges_region_2019%>%
    filter(Year == 2019)%>%
    summarise(mean = mean(N_infection),
              median = median(N_infection),
              q025 = quantile(N_infection, 0.025),
              q250 = quantile(N_infection, 0.25),
              q750 = quantile(N_infection, 0.75),
              q975 = quantile(N_infection, 0.975))%>%
    dplyr::select(-Year)
  
  ### Combined
  t_foi_infections_finalAges_2019 = bind_rows(t_foi_infections_finalAges_region_age_2019,
                                         t_foi_infections_finalAges_region_2019%>%
                                        mutate(ag = "Total"))
  
  write.csv(t_foi_infections_finalAges_2019, file = "t_foi_infections_finalAges_2019.csv")
}


##################################
### AGE-SEX SPECIFIC INCIDENCE ###
##################################

####################################################################
### SCALED 2019 INFECTIONS AS INCIDENCE PER 100,000 FOR ALL AGES ###
####################################################################

### FINAL TOTALS IN EACH DISTRICT
df_incidence_2019_allAges = df_infections_2019_allAges%>%
  dplyr::select(-ag_enable)%>%
  rename(ag = ag_final)%>%
  mutate(incidence = N_infection/UN_scaled_subnational)

### FINAL TOTALS IN EACH DISTRICT BY AGE GROUP
df_incidence_2019_finalAges = df_infections_2019_allAges%>%
  dplyr::select(-ag_enable)%>%
  rename(ag = ag_final)%>%
  group_by(Country, GID_0, Region, GID_1, Year, Sex, ag, n_draw)%>%
  summarise(N_infection = sum(N_infection),
            UN_scaled_subnational = sum(UN_scaled_subnational))%>%
  mutate(incidence = N_infection/UN_scaled_subnational)%>%
  mutate(ag = factor(ag, levels = ag_levels))

### FINAL TOTALS ACROSS REGION BY AGE GROUP
df_incidence_2019_allAges_region_age = df_infections_2019_allAges%>%
  dplyr::select(-ag_enable)%>%
  rename(ag = ag_final)%>%
  group_by(Year, Sex, ag, n_draw)%>%
  summarise(UN_scaled_subnational = sum(UN_scaled_subnational),
            N_infection = sum(N_infection))%>%
  mutate(incidence = N_infection/UN_scaled_subnational)%>%
  mutate(ag = factor(ag, levels = ag_levels))


#################
### SAVE DATA ###
#################

save(df_incidence_2019_allAges, file = "df_incidence_2019_allAges.Rdata")
save(df_incidence_2019_finalAges, file = "df_incidence_2019_finalAges.Rdata")



###################################################
### PLOT BASELINE INCIDENCE FOR EACH DISTRICT ###
###################################################

### PLOT ###
if(render_plots){
  p_foi_incidence_scaled_finalAges_Districts = df_incidence_2019_finalAges%>%
    filter(Year == 2019)%>%
    mutate(Region_Country = paste0(Region, ", ", Country))%>%
    ggplot(., aes(x = Region_Country, y = incidence*100000, colour = ag, fill = ag))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2, position = position_dodge())+
    scale_colour_manual("Age group", values = ag_cols)+
    scale_fill_manual("Age group", values = ag_cols)+
    theme_classic()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("")+ylab("Annual LASV infections per 100,000 (2019)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_foi_incidence_scaled_finalAges_Districts, file = "p_foi_incidence_scaled_finalAges_Districts.png", 
         width = 32, height = 12, units = "cm")
  
}


###################################################
### PLOT BASELINE INCIDENCE ACROSS WHOLE REGION ###
###################################################

### PLOT ###
if(render_plots){
  p_foi_incidence_scaled_finalAges = df_incidence_2019_allAges_region_age%>%
    filter(Year == 2019)%>%
    ggplot(., aes(x = ag, y = incidence*100000, colour = ag, fill = ag))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    scale_colour_manual(values = ag_cols)+
    scale_fill_manual(values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Age group")+ylab("Annual LASV infections per 100,000 (2019)")+
    theme(legend.position = "none", 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_foi_incidence_scaled_finalAges, file = "p_foi_incidence_scaled_finalAges.png", 
         width = 10, height = 12, units = "cm")
  
}

### SUMMARY TABLE ###
if(render_tables){
  
  t_foi_incidence_finalAges_region_age = df_incidence_2019_allAges_region_age%>%
    filter(Year == 2019)%>%
    group_by(ag)%>%
    mutate(incidence_per100000 = incidence * 100000)%>%
    summarise(mean = mean(incidence_per100000),
              median = median(incidence_per100000),
              q025 = quantile(incidence_per100000, 0.025),
              q250 = quantile(incidence_per100000, 0.25),
              q750 = quantile(incidence_per100000, 0.75),
              q975 = quantile(incidence_per100000, 0.975))
  
  t_foi_incidence_finalAges_region = df_incidence_2019_allAges_region_age%>%
    filter(Year == 2019)%>%
    group_by(n_draw)%>%
    summarise(N_infection = sum(N_infection),
              UN_scaled_subnational  = sum(UN_scaled_subnational))%>%
    ungroup()%>%
    mutate(incidence_per100000 = N_infection/UN_scaled_subnational * 100000)%>%
    summarise(mean = mean(incidence_per100000),
              median = median(incidence_per100000),
              q025 = quantile(incidence_per100000, 0.025),
              q250 = quantile(incidence_per100000, 0.25),
              q750 = quantile(incidence_per100000, 0.75),
              q975 = quantile(incidence_per100000, 0.975))
  
  ### Combined
  t_foi_incidence_finalAges = bind_rows(t_foi_incidence_finalAges_region_age,
                                                  t_foi_incidence_finalAges_region%>%
                                                 mutate(ag = "Total"))
  
  write.csv(t_foi_incidence_finalAges, file = "t_foi_incidence_finalAges.csv")
}


##############################################
### PLOT COMPARING INFECTIONS vs INCIDENCE ###
##############################################

if(render_plots){
  p_foi_infections_incidence_scaled_finalAges = plot_grid(p_foi_incidence_scaled_finalAges,
                                                          p_foi_infections_scaled_finalAges,
                                    ncol = 2, labels = c("A", "B"))
  
  ggsave(p_foi_infections_incidence_scaled_finalAges, file = "p_foi_infections_incidence_scaled_finalAges.png",
         width = 20, height = 12, units = "cm")
}


#############################
### INFECTION PROJECTIONS ###
#############################

#################################################################################
### MULTIPLY AGE- SEX- AND REGION-SPECIFIC INCIDENCE TO PROJECTED POPULATIONS ###
#################################################################################

### Projected infections by district across all ages
df_infections_projected_allAges = df_population_ag%>%
  dplyr::select(-c(PregProp, proportion_subnational))%>%
  left_join(., df_incidence_2019_allAges%>%
              dplyr::select(-c(Country, GID_0, Region, Year, UN_scaled_subnational, propAg_Enable, N_infection, N_infections_scaled )),
            by = c("GID_1", "Sex", "Age", "ag"),
            relationship = "many-to-many")%>%
  mutate(N_infection = incidence*UN_scaled_subnational)%>%
  dplyr::select(-c(Country, GID_0, UN_scaled, UN_scaled_subnational, incidence))


### Projected infections by distrct across final age groups
df_infections_projected_finalAges = df_population_finalAges%>%
  dplyr::select(-c(PropPregnant))%>%
  left_join(., df_incidence_2019_finalAges%>%ungroup()%>%
              dplyr::select(-c(Country, GID_0, Region, Year, UN_scaled_subnational, N_infection)),
            by = c("GID_1", "Sex", "ag"),
            relationship = "many-to-many")%>%
  mutate(N_infection = incidence*UN_scaled_subnational)


### Projected infection totals
df_infections_projected_total = df_infections_projected_finalAges%>%
  ungroup()%>%
  group_by(Year, n_draw)%>%
  summarise(N_infection = sum(N_infection))

### Projected infection totals by age group
df_infections_projected_total_finalAges = df_infections_projected_finalAges%>%
  ungroup()%>%
  group_by(Year, ag, n_draw)%>%
  summarise(N_infection = sum(N_infection))




######################
### SAVE FULL DATA ###
######################

save(df_infections_projected_allAges, file = "df_infections_projected_allAges.Rdata")
save(df_infections_projected_finalAges, file = "df_infections_projected_finalAges.Rdata")

######################
### SUMMARISE DATA ###
######################

#############
### PLOTS ###
#############

if(render_plots){
  p_projected_infections_total_over_time = df_infections_projected_total%>%
    mutate(Year = factor(Year))%>%
    ggplot(., aes(x = Year, y = N_infection, colour = Year, fill = Year))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_boxplot(colour = "black", lwd = 0.2, outlier.size = 0.2)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Year")+ylab("Annual LASV infections")+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_projected_infections_total_over_time, file = "p_projected_infections_total_over_time.png", width = 14, height = 12, units = "cm")
  
  
  p_projected_infections_finalAges_total_over_time = df_infections_projected_total_finalAges%>%
    mutate(Year = factor(Year),
           ag = factor(ag, levels = ag_levels))%>%
    ggplot(., aes(x = Year, y = N_infection, colour = ag, fill = ag))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_violin(colour = "black", lwd = 0.2, alpha = 0.8, width = 6, position = position_dodge(0))+
    scale_colour_manual("Age group", values = ag_cols)+
    scale_fill_manual("Age group", values = ag_cols)+
    theme_bw()+
    scale_y_continuous(label = comma, limits = c(0, NA))+
    xlab("Year")+ylab("Annual LASV infections")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggsave(p_projected_infections_finalAges_total_over_time, file = "p_projected_infections_finalAges_total_over_time.png", width = 14, height = 12, units = "cm")
  
}

##############
### TABLES ###
##############

if(render_tables){
  
  ### TOTAL INFECTIONS BY YEAR ###
  t_projected_infections_totals_by_year = df_infections_projected_total%>%
    group_by(Year)%>%
    summarise(mean = mean(N_infection),
              median = median(N_infection),
              q025 = quantile(N_infection, 0.025),
              q250 = quantile(N_infection, 0.25),
              q750 = quantile(N_infection, 0.75),
              q975 = quantile(N_infection, 0.975))
  
  ### TOTAL INFECTIONS BY YEAR, CUMULATIVE ###
  t_projected_infections_totals_by_year_cumul = df_infections_projected_total%>%
    group_by(n_draw)%>%
    summarise(N_infection = sum(N_infection))%>%
    ungroup()%>%
    summarise(mean = mean(N_infection),
              median = median(N_infection),
              q025 = quantile(N_infection, 0.025),
              q250 = quantile(N_infection, 0.25),
              q750 = quantile(N_infection, 0.75),
              q975 = quantile(N_infection, 0.975))
  
  ### Combined
  t_projected_infections_by_year = bind_rows(t_projected_infections_totals_by_year%>%mutate(Year = as.character(Year)),
                                             t_projected_infections_totals_by_year_cumul%>%
                                          mutate(Year = "Total"))
  
  ### Save
  write.csv(t_projected_infections_by_year, file = "t_projected_infections_by_year.csv")
  
  ### TOTAL INFECTIONS BY AGE AND YEAR ###
  t_projected_infections_totals_by_age_year = df_infections_projected_total_finalAges%>%
    group_by(Year, ag)%>%
    summarise(mean = mean(N_infection),
              median = median(N_infection),
              q025 = quantile(N_infection, 0.025),
              q250 = quantile(N_infection, 0.25),
              q750 = quantile(N_infection, 0.75),
              q975 = quantile(N_infection, 0.975))
  
  ### Save
  write.csv(t_projected_infections_totals_by_age_year, file = "t_projected_infections_totals_by_age_year.csv")
  
}



####################################
### Infections in pregnant women ###
####################################

df_infections_projected_finalAges_female = df_infections_projected_finalAges%>%
  dplyr::select(-c(UN_scaled_subnational, N_pregnant, incidence))%>%
  left_join(., df_PropPregnant,
            by = c("Country", "GID_0", "Region", "GID_1", "Sex", "ag"))%>%
  filter(Sex != "Male")%>%
  mutate(Female_Preg = N_infection*duration_pregnant*PropPregnant,
         Female_NotPreg = N_infection - Female_Preg)%>%
  ungroup()%>%
  dplyr::select(-c(Sex, N_infection, PropPregnant))%>%
  pivot_longer(-c(Country, GID_0, Region, GID_1, Year, n_draw, ag),
               names_to = "Sex",
               values_to = "N_infection")

df_infections_projected_finalAges_male = df_infections_projected_finalAges%>%
  filter(Sex == "Male")


################################################################################
### FINAL ANNUAL INFECTION DATA ACROSS REGION, AGE, SEX AND PREGNANCY STATUS ###
################################################################################

df_infections_projected_finalAges_preg = bind_rows(df_infections_projected_finalAges_male,
                                                df_infections_projected_finalAges_female)


### check totals per age group still add up after stratifying pregnant women
check_inf_per_age = df_infections_projected_finalAges%>%
  group_by(ag)%>%
  summarise(N_infection = sum(N_infection))%>%
  dplyr::select(N_infection)

check_inf_per_age_preg = df_infections_projected_finalAges_preg%>%
  group_by(ag)%>%
  summarise(N_infection = sum(N_infection))%>%
  dplyr::select(N_infection)

stopifnot(abs(sum(check_inf_per_age$N_infection - check_inf_per_age_preg$N_infection)) < 0.02)

### number pregnant should be lower in these numbers than input data (which don't account for preg duration)
save(df_infections_projected_finalAges_preg, file = "df_infections_projected_finalAges_preg.Rdata")


############################################################################
### TABLE OF DISTRICT-SPECIFIC INFECTION AND INCIDENCE ESTIMATES IN 2019 ###
############################################################################

if(render_tables){
  
  ### Incidence by district
  t_incidence_by_district = df_infections_total_annual%>%
    filter(realyear == 1, scenario == 1)%>%
    left_join(., df_population_2019%>%
                filter(Year == 2019)%>%
                group_by(GID_1, Region)%>%
                summarise(Population = sum(UN_scaled_subnational)),
              by = "GID_1")%>%
    ungroup()%>%
    mutate(incidence_per100000 = spillover/Population * 100000)%>%
    group_by(country, GID_1, Region)%>%
    summarise(mean = mean(incidence_per100000),
              min = quantile(incidence_per100000, 0.025),
              max = quantile(incidence_per100000, 0.975))%>%
    mutate(incidence = paste0(round(mean), " (", round(min), " - ", round(max), ")"))%>%
    dplyr::select(country, GID_1, Region, incidence)
    
  
  ### Infections by district
  t_infections_by_district = df_infections_total_annual%>%
    filter(realyear == 1, scenario == 1)%>%
    group_by(country, GID_1)%>%
    summarise(mean = mean(spillover),
              min = quantile(spillover, 0.025),
              max = quantile(spillover, 0.975))%>%
    ungroup()%>%
    mutate(infections = paste0(round(mean), " (", round(min), " - ", round(max), ")"))%>%
    dplyr::select(GID_1, infections)
  
  ### Mixed incidence and infections by district
  t_incidence_infections_by_district = left_join(t_incidence_by_district, t_infections_by_district, by = c("GID_1"))
  
  write.csv(t_incidence_infections_by_district, file = "t_incidence_infections_by_district.csv")
  
}

