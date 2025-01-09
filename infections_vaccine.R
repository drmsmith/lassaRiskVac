
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


###########################################
### GENERATE INFECTION VACCINE DATASETS ###
###########################################

##################################
### LOAD VACCINATION SCENARIOS ###
##################################

load("vaccination/df_vacc_scenarios_weekly.Rdata")

### LIMIT TO "HIGH" STRATEGY WHICH INCLUDES ALL DISTRICTS
### THEN LATER APPLY ESTIMATES FOr SPECIFIC DISTRICTS FOR MEDIUM AND LOW STRATEGIES

df_vacc_scenarios_weekly_high = df_vacc_scenarios_weekly%>%
  filter(strategy %in% c("wcba_high", 
                         "adults_high", 
                         "children_high",
                         "elderly_high"))%>%
  dplyr::select(-c(week))%>%
  rename(week = week_of_year)%>%
  mutate(strategy = case_when(strategy == "wcba_high" ~ "wcba",
                              strategy == "adults_high" ~ "adults",
                              strategy == "children_high" ~ "children",
                              strategy == "elderly_high" ~ "elderly",
                              T ~ "ERROR"))%>%
  dplyr::select(-c(uptake_prop_weekly_cumul, waning_prop_weekly_cumul, net_prop_vacc_weekly_cumul,
                   immune_prop_weekly_cumul, waned_prop_weekly_cumul))

rm(df_vacc_scenarios_weekly)

vec_vacc_strategies_loop = unique(df_vacc_scenarios_weekly_high$strategy)

####################################################
### SEPARATE LISTS DEPENDING ON COUNTRY GROUPING ###
####################################################

vec_GID_1_burden_h = unique(df_vacc_scenarios_weekly_high%>%filter(burden == "High")%>%dplyr::select(GID_1))
vec_GID_1_burden_h_m = unique(df_vacc_scenarios_weekly_high%>%filter(burden != "Low")%>%dplyr::select(GID_1))
vec_GID_1_burden_h_m_l = unique(df_vacc_scenarios_weekly_high%>%dplyr::select(GID_1))


##########################################################
### COMBINE INFECTION AND VACCINE DISTRICT BY DISTRICT ### 
##########################################################

### LOOP THROUGH WEEKLY DATA AND AGGREGATE ANNUALLY, SAVED SEPARATELY FOR EACH DISTRICT
### SAVE FULL WEEKLY OUTPUTS FOR ONE DISTRICT TO DEMONSTRATE EFFECT


for(GID_1_i in GID_1_final){
  
  print(paste0("on ", GID_1_i))
  
  ######################
  ### INFECTION DATA ###
  ######################
  
  ### Load data excluding pregnancy
  # (pregnancy added in separately for hospital outcomes and downstream)
  dt_infections_projected_weekly_allAges_i = loadRData(paste0("infections/projections_weekly/df_infections_projected_weekly_allAges_", GID_1_i,".Rdata"))%>%
    ungroup()%>%
    dplyr::select(-c(Region, N_infection, case_proportion))%>%
    as.data.table()
  
  ###################################################
  ### MERGE WEEKLY INFECTION AND VACCINE DATASETS ###
  ###################################################
  
  for(vec_vacc_strategies_i in vec_vacc_strategies_loop){
    
    print(paste0("on ", vec_vacc_strategies_i))
    
    dt_vacc_scenarios_weekly_high_i = df_vacc_scenarios_weekly_high%>%
      filter(GID_1 == GID_1_i, strategy == vec_vacc_strategies_i)%>%
      ungroup()%>%
      dplyr::select(-c(Country, GID_0, Region, burden))%>%
      as.data.table()
    
    ### SPLIT BY YEAR TO MAKE IT MANAGEABLE
    dt_infections_weekly_vaccinated_i = merge(dt_infections_projected_weekly_allAges_i, 
                                                        dt_vacc_scenarios_weekly_high_i,
                                                        by = c("GID_1", "Year", "week", "Sex", "Age"),
                                                        all.x = TRUE, allow.cartesian = T)
    
    dt_infections_weekly_vaccinated_i[ , N_infection_vaccinated_weekly := N_infection_weekly * net_prop_immune_weekly_cumul]
    
    #############################################################
    ### ONLY FOR NGA.12_1, SAVE WEEKLY INFECTION-VACCINE DATA ###
    #############################################################
    
    if(GID_1_i == "NGA.12_1"){save(dt_infections_weekly_vaccinated_i, file = paste0("dt_infections_weekly_vaccinated_", GID_1_i,"_", vec_vacc_strategies_i,".Rdata"))}
    
    ##################################################
    ### AGGREGATE ANNUAL DATASETS FROM WEEKLY DATA ###
    ##################################################
    
    ### Empty list to save final annual aggregated results in each district
    list_infections_annual_vaccinated = list()
    
    for(Year_j in 2025:2037){
      
      print(paste0("Aggregating district ", GID_1_i, ", ", Year_j))
      
      list_infections_annual_vaccinated[[which(Year_j == 2025:2037)]] = dt_infections_weekly_vaccinated_i[
        Year == Year_j,
        keyby = .(GID_1, strategy, Sex, Year, Age, ag, n_draw),
        .(N_infection = sum(N_infection_weekly),
          N_infection_vaccinated = sum(N_infection_vaccinated_weekly))
      ]
      
    }
    
    dt_infections_annual_vaccinated = rbindlist(list_infections_annual_vaccinated)
    
    ##########################################
    ### SAVE ANNUAL INFECTION-VACCINE DATA ###
    ##########################################
    
    save(dt_infections_annual_vaccinated, file = paste0("dt_infections_annual_vaccinated_", GID_1_i,"_", vec_vacc_strategies_i,".Rdata"))
    
  }
}
