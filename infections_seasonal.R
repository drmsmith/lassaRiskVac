
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

#################################
### LOAD PROJECTED INFECTIONS ### 
#################################

# Total infections Male and Female, all ages
load("infections/projections_annual/df_infections_projected_allAges.Rdata")

# Total infections Male and Female, final age groups
load("infections/projections_annual/df_infections_projected_finalAges.Rdata")

# Total infections Male, Female (not pregnant) and Female (pregnant)
load("infections/projections_annual/df_infections_projected_finalAges_preg.Rdata")

####################################
### ADJUST INFECTIONS SEASONALLY ###
####################################

####################################
### SEASONAL CASE DATA FROM NCDC ###
####################################

df_ncdc_2019 = read.csv("parameters_data/NCDC_2019.csv")%>%
  rename(week = X1.0667304742493329,
         cases = X22.82528202192367)%>%
  mutate(year = 2019)
df_ncdc_2019[1,1] = 1

df_ncdc_2020 = read.csv("parameters_data/NCDC_2020.csv")%>%
  rename(week = X1.0642825624292716,
         cases = X13.860895050337916)%>%
  mutate(year = 2020)
df_ncdc_2020[1,1] = 1

df_ncdc_2021 = read.csv("parameters_data/NCDC_2021.csv")%>%
  rename(week = X1.0589596172145397 ,
         cases = X8.543022355856891)%>%
  mutate(year = 2021)
df_ncdc_2021[1,1] = 1
df_ncdc_2021[nrow(df_ncdc_2021),1] = 52

df_ncdc_2022 = read.csv("parameters_data/NCDC_2022.csv")%>%
  rename(week = X1.050747860489829,
         cases = X47.03409798469036)%>%
  mutate(year = 2022)
df_ncdc_2022[1,1] = 1

df_ncdc_2023 = read.csv("parameters_data/NCDC_2023.csv")%>%
  rename(week = X1.0408827299279277,
         cases = X28.9027186923077)%>%
  mutate(year = 2023)
df_ncdc_2023[1,1] = 1

### interpolated dataframes
df_interp_2019 = data.frame(week = 1:52, year = 2019, cases = approxfun(df_ncdc_2019$week, df_ncdc_2019$cases)(1:52))
df_interp_2020 = data.frame(week = 1:52, year = 2020, cases = approxfun(df_ncdc_2020$week, df_ncdc_2020$cases)(1:52))
df_interp_2021 = data.frame(week = 1:52, year = 2021, cases = approxfun(df_ncdc_2021$week, df_ncdc_2021$cases)(1:52))
df_interp_2022 = data.frame(week = 1:52, year = 2022, cases = approxfun(df_ncdc_2022$week, df_ncdc_2022$cases)(1:52))
df_interp_2023 = data.frame(week = 1:52, year = 2023, cases = approxfun(df_ncdc_2023$week, df_ncdc_2023$cases)(1:52))


df_interp = bind_rows(df_interp_2019, df_interp_2020, df_interp_2021, df_interp_2022, df_interp_2023)%>%
  mutate(year = factor(year))

df_interp_mean = df_interp%>%
  group_by(week)%>%
  summarise(cases = mean(cases, na.rm = T))%>%
  ungroup()%>%
  mutate(year = "mean")

df_interp_rollmean = df_interp_mean%>%
  mutate(cases = zoo::rollmean(cases, k = 7, fill = NA))

df_interp_rollmean_weeks_1_2_3 = bind_rows(df_interp_mean%>%filter(week %in% 50:52),
                                           df_interp_mean%>%filter(week %in% 1:7))%>%
  mutate(cases = zoo::rollmean(cases, k = 7, fill = NA))%>%
  dplyr::filter(week %in% 1:3)

df_interp_rollmean_weeks_50_51_52 = bind_rows(df_interp_mean%>%filter(week %in% 46:52),
                                              df_interp_mean%>%filter(week %in% 1:3))%>%
  mutate(cases = zoo::rollmean(cases, k = 7, fill = NA))%>%
  dplyr::filter(week %in% 50:52)

df_interp_rollmean_final = bind_rows(
  df_interp_rollmean_weeks_1_2_3,
  df_interp_rollmean%>%filter(week %in% 4:49),
  df_interp_rollmean_weeks_50_51_52)%>%
  mutate(year = "rolling mean")


### SHIFT CASES TO INFECTIONS ACCOUNTING FOR INCUBATION PERIOD AND DELAY TO PRESENTATION
# Value used in Smith et al. 
lasv_incubation = 10.3 #days
# From LASCOPE
lasv_onset_admission_delay = 9.33 # days
### So anticipate approximately 3 weeks from infection to hospitalisation

df_interp_rollmean_shift = df_interp_rollmean_final%>%
  mutate(week = week-3)%>%
  mutate(week = case_when(week == -2 ~ 50,
                          week == -1 ~ 51,
                          week == 0 ~ 52,
                          T ~ week))%>%
  mutate(year = "rolling mean (shifted)")%>%
  arrange(week)



df_interp_all = bind_rows(df_interp,
                          df_interp_mean,
                          df_interp_rollmean_final,
                          df_interp_rollmean_shift)

df_interp_all_annual = df_interp_all%>%group_by(year)%>%summarise(cases_annual = sum(cases, na.rm = T))

df_interp_all_proportion = df_interp_all%>%
  left_join(., df_interp_all_annual, by = "year")%>%
  mutate(case_proportion = cases/cases_annual)



### PLOT SEASONAL ADJUSTMENT ###

if(render_plots){
  p_interp = df_interp_all_proportion%>%
    ggplot(., aes(x = week, y = case_proportion, 
                  fill = year, colour = year, linetype = year, linewidth = year, alpha = year, shape = year, size = year))+
    geom_hline(yintercept = 0, colour = "grey")+
    theme_bw()+
    geom_point()+
    geom_line()+
    scale_shape_manual("", values = c(rep(21, 5), NA, NA, NA))+
    scale_size_manual("", values = c(rep(0.5, 5), NA, NA, NA))+
    scale_alpha_manual("", values = c(rep(0.6, 7), 1))+
    scale_linetype_manual("", values = c(rep(1, 5), 1, 1, 1))+
    scale_linewidth_manual("", values = c(rep(0.2, 5), 0.8, 0.8, 1))+
    scale_colour_manual("", values = c('#e7298a','#377eb8','#4daf4a','#984ea3','#ff7f00', "black", "brown", '#e41a1c'))+
    scale_fill_manual("", values = c('#e7298a','#377eb8','#4daf4a','#984ea3','#ff7f00', "black", "brown", '#e41a1c'))+
    ylab("Proportion of cases")+
    xlab("Epidemiological week")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p_interp
  
  ggsave(p_interp, file = "p_interpolate_cases.png", width = 16, height = 8, units = "cm")
}


### FINAL DATASET FOR SCALING ###

df_seasonal_scaling = df_interp_all_proportion%>%
  filter(year == "rolling mean (shifted)")%>%
  dplyr::select(week, case_proportion)

stopifnot(sum(df_seasonal_scaling$case_proportion) == 1)  


###############################
### SCALE INFECTIONS WEEKLY ###
###############################

### Male and Female, all ages
### must do these district-by-district due to massive data

load("demography/GID_1_final.Rdata")

###################################
### SHRINK DATA TO n=500 n_runs ###
###################################

df_infections_projected_allAges = df_infections_projected_allAges%>%filter(n_draw <501)
df_infections_projected_finalAges = df_infections_projected_finalAges%>%filter(n_draw <501)
df_infections_projected_finalAges_preg = df_infections_projected_finalAges_preg%>%filter(n_draw <501)

###############################################
### LOOP THROUGH EACH YEAR IN EACH DISTRICT ###
###############################################

for(GID_1_i in GID_1_final){
  
  ### Extract data for each district
  df_infections_projected_weekly_allAges_i = df_infections_projected_allAges%>%
    filter(GID_1 == GID_1_i)
  df_infections_projected_weekly_finalAges_i = df_infections_projected_finalAges%>%
    filter(GID_1 == GID_1_i)
  df_infections_projected_weekly_finalAges_preg_i = df_infections_projected_finalAges_preg%>%
    filter(GID_1 == GID_1_i)
  
  list_infection_weekly_allAges = list()
  list_infection_weekly_finalAges = list()
  list_infection_weekly_finalAges_preg = list()
  
  qounter = 0
  
  for(Year_j in 2025:2037){
    
    qounter = qounter + 1
    
    print(paste0("seasonally adjusting final infections in ", GID_1_i, " in year ", Year_j))
    
    df_infections_projected_weekly_allAges_i_j = df_infections_projected_weekly_allAges_i%>%
      filter(Year == Year_j)%>%
      cross_join(., df_seasonal_scaling)%>%
      mutate(N_infection_weekly = N_infection*case_proportion)
    
    df_infections_projected_weekly_finalAges_i_j = df_infections_projected_weekly_finalAges_i%>%
      filter(Year == Year_j)%>%
      cross_join(., df_seasonal_scaling)%>%
      mutate(N_infection_weekly = N_infection*case_proportion)
    
    df_infections_projected_weekly_finalAges_preg_i_j = df_infections_projected_weekly_finalAges_preg_i%>%
      filter(Year == Year_j)%>%
      cross_join(., df_seasonal_scaling)%>%
      mutate(N_infection_weekly = N_infection*case_proportion)
    
    list_infection_weekly_allAges[[qounter]] <- df_infections_projected_weekly_allAges_i_j
    list_infection_weekly_finalAges[[qounter]] <- df_infections_projected_weekly_finalAges_i_j
    list_infection_weekly_finalAges_preg[[qounter]] <- df_infections_projected_weekly_finalAges_preg_i_j
  }
  
  ### save data for each district separately
  df_infections_projected_weekly_allAges = do.call(rbind, list_infection_weekly_allAges)
  df_infections_projected_weekly_finalAges = do.call(rbind, list_infection_weekly_finalAges)
  df_infections_projected_weekly_finalAges_preg = do.call(rbind, list_infection_weekly_finalAges_preg)
  
  save(df_infections_projected_weekly_allAges, file= paste0("df_infections_projected_weekly_allAges_", GID_1_i, ".Rdata"))
  save(df_infections_projected_weekly_finalAges, file= paste0("df_infections_projected_weekly_finalAges_", GID_1_i, ".Rdata"))
  save(df_infections_projected_weekly_finalAges_preg, file= paste0("df_infections_projected_weekly_finalAges_preg_", GID_1_i, ".Rdata"))
}

df_infections_projected_weekly_allAges%>%filter(n_draw == 1, Age == 1, week == 2, Year == 2025)
