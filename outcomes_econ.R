###################################################
### CALCULATE FULL HEALTH AND ECONOMIC OUTCOMES ###
###################################################

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

##################################
### DEFINE SIMULATION SCENARIO ###
##################################

### scen1: base case: no symptom age structure, all symptomatic cases have equal probability of developing SNHL
### scen2: only survivors of severe LF can develop SNHL
### scen3: mild symptom risk increases with age and sex

sim_scen = "scen1"
# sim_scen = "scen2"
# sim_scen = "scen3"


################################
### LOAD ECONOMIC PARAMETERS ###
################################

df_params_econ_mf = loadRData("parameters_data/df_params_econ_final.Rdata")

### Replicate econ params for preg
df_params_econ_m = df_params_econ_mf%>%
  filter(Sex == "Male")
df_params_econ_f = df_params_econ_mf%>%
  filter(Sex == "Female")%>%
  mutate(Sex = "Female_NotPreg")
df_params_econ_pw = df_params_econ_f%>%
  mutate(Sex = "Female_Preg")

### Final age-sex-stratified econ params dataset
df_params_econ_final = bind_rows(df_params_econ_m,
                                 df_params_econ_f,
                                 df_params_econ_pw)%>%
  mutate(Year = as.numeric(Year))

###################################
### LOAD MONTE CARLO PARAMETERS ###
###################################

df_params_montecarlo = loadRData("parameters_data/params_montecarlo.Rdata")


############################################################
### LOAD STRATIFIED SYMPTOM, HOSPITAL AND MORTALITY RISK ###
############################################################

### SYMPTOMS, DEPENDS ON SCENARIO ###
if(sim_scen %in% c("scen1", "scen2")){
  df_prob_symptoms_MF = read.csv("risk/df_infection_symptom_risk_str.csv")%>%
    dplyr::select(-X)%>%
    mutate(Sex = case_when(Sex == "Female" ~ "Female_NotPreg",
                           T ~ Sex))
  
  df_prob_symptoms_Fpreg = df_prob_symptoms_MF%>%
    filter(Sex == "Female_NotPreg")%>%
    mutate(Sex = "Female_Preg")
  
  # Combine male, female_preg, female_notpreg
  df_prob_symptoms = bind_rows(df_prob_symptoms_MF,
                               df_prob_symptoms_Fpreg)
  }

if(sim_scen %in% c("scen3")){
  df_prob_symptoms_MF = read.csv("risk/df_infection_symptom_risk_str_scen3.csv")%>%
    dplyr::select(-X)%>%
    mutate(Sex = case_when(Sex == "Female" ~ "Female_NotPreg",
                           T ~ Sex))
  
  df_prob_symptoms_Fpreg = df_prob_symptoms_MF%>%
    filter(Sex == "Female_NotPreg")%>%
    mutate(Sex = "Female_Preg")
  
  # Combine male, female_preg, female_notpreg
  df_prob_symptoms = bind_rows(df_prob_symptoms_MF,
                               df_prob_symptoms_Fpreg)
}

### HOSPITALISATION ###
# Load base male/female data and specify non-pregnant female
df_prob_hosp_MF = read.csv("risk/df_infection_hospital_risk_str.csv")%>%
  dplyr::select(-X)%>%
  mutate(Sex = case_when(Sex == "Female" ~ "Female_NotPreg",
                         T ~ Sex))

# Apply same infection-hospital risk to pregnant female
df_prob_hosp_Fpreg = df_prob_hosp_MF%>%
  filter(Sex == "Female_NotPreg")%>%
  mutate(Sex = "Female_Preg")

# Combine male, female_preg, female_notpreg
df_prob_hosp = bind_rows(df_prob_hosp_MF,
                         df_prob_hosp_Fpreg)

### CFR ###
# Load pre-stratified data
df_prob_death = read.csv("risk/df_CFR_finalAges_preg.csv")%>%
  dplyr::select(-X)


################################
### LOAD PROPORTION PREGNANT ###
################################

df_PropPregnant = loadRData("demography/df_PropPregnant.Rdata")%>%
  dplyr::select(-c("Country", "Region"))


#####################################
### SUMMARY OF INCLUDED DISTRICTS ###
#####################################

df_countries_regions = unique(df_PropPregnant[,c('GID_0','GID_1')])

##########################
### COMBINE PARAMETERS ###
##########################

### COMBINE MONTE CARLO PARAMS, ECON PARAMS AND STRATIFIED RISK PARAMS
df_params_all = df_params_econ_final%>%
  dplyr::select(-c("dur_snhl"))%>%
  left_join(., df_countries_regions, by = "GID_0", relationship = "many-to-many")%>%
  left_join(., df_prob_symptoms, by = c("Sex", "ag", "n_draw"))%>%
  left_join(., df_prob_hosp, by = c("Sex", "ag", "n_draw"))%>%
  left_join(., df_prob_death%>%dplyr::select(-c(Country, Region, GID_1, GID_0)), by = c("Sex", "ag", "n_draw"))%>%
  left_join(., df_params_montecarlo, by = "n_draw")

### list of variable names to drop from final data (to massively lighten datasets)
vec_variables_to_drop = c("EmploymentRate",
                          "future_life_years", "future_life_years_disc", "future_life_years_nnd", "future_life_years_nnd_disc", "future_life_years_snhl", "future_life_years_snhl_disc",
                          "future_work_years", "future_work_years_disc", "future_work_years_snhl", "future_work_years_snhl_disc", 
                          "PropPregnant", "GNI", "GNI_daily", "cost_per_hosp", "cost_per_hosp_oop", "cost_per_hosp_gvt",
                          "cost_per_outpatientvisit", "cost_per_daly", "cost_per_vsl", "countr_specific_OOP_2017IntD",
                          "income_threshold", "prob_catastrophic", "prop_below_nat_pov_line", "prob_impoverishment",
                          "PropHosp", "prob_hosp_str",
                          "prob_death_str", "prob_hosp", "prob_death",
                          "prob_symptoms", "prob_symptoms_str",
                          "prob_treat_comm_any", "prob_treat_comm_gvt", "prob_snhl", "prob_fl_lassa", "prob_nnd_lassa", 
                          "dur_fever", "dur_ill_prehosp", "dur_hosp_survived", "dur_hosp_died", "dur_snhl", "dur_snhl_disc",
                          "disutility_fever", "disutility_hospital", "disutility_snhl", "DALYperpatient_fever", "DALYperpatient_hosp_survived", "DALYperpatient_hosp_died")

### severe disease comes out of symptomatic disease, so confirm prob_hosp is never greater than prob_symptoms
stopifnot(sum(df_params_all$prob_symptoms_str < df_params_all$prob_hosp_str) < 0.1)



#######################################################
### VACCINE ROLLOUT STRATEGY AND EFFICACY SCENARIOS ###
#######################################################

vec_strategy = c("adults", "children", "elderly", "wcba")

df_VE = data.frame(VE_scenario = c(1:5),
                   VE_mild = c(0.5, 0.5, 0.7, 0.7, 0.9),
                   VE_severe = c(0.5, 0.7, 0.7, 0.9, 0.9))


#############################################################################
### LOAD PROJECTED INFECTIONS AND CALCULATE OUTCOMES DISTRICT BY DISTRICT ###
#############################################################################

for(GID_1_i in GID_1_final){
  
  for(strategy_k in vec_strategy){
    
    ### PRINT
    print(paste0("calculating outcomes in ", GID_1_i, " for strategy ", strategy_k))
    
    ################################
    ### FINAL INFECTIONS DATASET ###
    ################################
    
    df_infections_vaccinated_annual_MF_i = loadRData(paste0("infections/vaccinated_annual/dt_infections_annual_vaccinated_", 
                                                            GID_1_i, "_", strategy_k, ".Rdata"))%>%
      as.data.frame()
    
    #######################################
    ### SPLIT INFECTIONS BY PREG STATUS ###
    #######################################
    
    df_infections_vaccinated_annual_F_i = df_infections_vaccinated_annual_MF_i%>%
      filter(Sex != "Male")%>%
      left_join(., df_PropPregnant%>%dplyr::select(-c(GID_0)), by = c("GID_1", "Sex", "ag"))%>%
      mutate(N_infection_Preg = N_infection * PropPregnant * duration_pregnant,
             N_infection_vaccinated_Preg = N_infection_vaccinated * PropPregnant * duration_pregnant,
             N_infection_NotPreg = N_infection - N_infection_Preg,
             N_infection_vaccinated_NotPreg = N_infection_vaccinated - N_infection_vaccinated_Preg)
    
    df_infections_vaccinated_annual_F_NotPreg_i = df_infections_vaccinated_annual_F_i%>%
      mutate(N_infection = N_infection_NotPreg,
             N_infection_vaccinated = N_infection_vaccinated_NotPreg,
             Sex = "Female_NotPreg")%>%
      dplyr::select(-c(N_infection_Preg, N_infection_vaccinated_Preg, N_infection_NotPreg, N_infection_vaccinated_NotPreg, PropPregnant))
    
    df_infections_vaccinated_annual_F_Preg_i = df_infections_vaccinated_annual_F_i%>%
      mutate(N_infection = N_infection_Preg,
             N_infection_vaccinated = N_infection_vaccinated_Preg,
             Sex = "Female_Preg")%>%
      dplyr::select(-c(N_infection_Preg, N_infection_vaccinated_Preg, N_infection_NotPreg, N_infection_vaccinated_NotPreg, PropPregnant))
    
    df_infections_vaccinated_annual_i = bind_rows(df_infections_vaccinated_annual_MF_i%>%
                                                    filter(Sex == "Male"),
                                                  df_infections_vaccinated_annual_F_NotPreg_i,
                                                  df_infections_vaccinated_annual_F_Preg_i)
    
    ### check totals among females (preg + not preg) equals female total
    check_infec_fem_tot = sum(df_infections_vaccinated_annual_MF_i%>%filter(Sex == "Female")%>%dplyr::select(N_infection))
    check_infec_fem_split = sum(df_infections_vaccinated_annual_i%>%filter(Sex != "Male")%>%dplyr::select(N_infection))
    stopifnot(abs(check_infec_fem_tot - check_infec_fem_split) < 0.01)
    
    ### check totals across everyone still identical
    check_infec_all_tot = sum(df_infections_vaccinated_annual_MF_i%>%dplyr::select(N_infection))
    check_infec_all_split = sum(df_infections_vaccinated_annual_i%>%dplyr::select(N_infection))
    stopifnot(abs(check_infec_all_tot - check_infec_all_split) < 0.01)
    
    ######################################
    ### START WITH NO VACCINE SCENARIO ###
    ######################################
    
    ### Infections always in "N_infection"
    
    ### NO VACCINE
    df_infections_baseline_annual_i = df_infections_vaccinated_annual_i%>%
      mutate(strategy = "baseline")%>%
      dplyr::select(-c(N_infection_vaccinated))%>%
      mutate(VE_scenario = 0, VE_mild = 1, VE_severe = 1)
    
    
    ### LOOP THROUGH VACCINE EFFICACY SCENARIOS
    ### EACH CONTAINS BASELINE (no vaccine) FOR EASE OF CALCULATING RELATIVE REDUCTION
    
    list_infections_vaccine_annual_i = list()
    
    for(VE_scenario_j in df_VE$VE_scenario){
      
      VE_mild_j = df_VE[VE_scenario_j, 2]
      VE_severe_j = df_VE[VE_scenario_j, 3]
      
      ##########################################
      ### SPLIT INFECTIONS BY VACCINE vs. NO ###
      ##########################################
      
      ### Infections always in "N_infection"
      
      ### VACCINE
      df_infections_vaccine_annual_i_j = df_infections_vaccinated_annual_i%>%
        dplyr::select(-c(N_infection))%>%
        rename(N_infection = N_infection_vaccinated)%>%
        mutate(VE_scenario = VE_scenario_j, VE_mild = VE_mild_j, VE_severe = VE_severe_j)
      
      list_infections_vaccine_annual_i[[VE_scenario_j]] = df_infections_vaccine_annual_i_j
      
      ### CHECK
      if(render_plots){
        df_infections_vaccine_annual_i_j%>%filter(Age == 55, Sex == "Male", n_draw == 20)%>%
          ggplot(., aes(x = Year, y = N_infection, colour = strategy))+
          geom_line()+
          theme_bw()
      }
    } 
    
    df_infections_vaccine_annual_i = do.call(rbind, list_infections_vaccine_annual_i)
    
    
    ### FINAL TOGETHER
    df_infections_final_annual_i = bind_rows(df_infections_baseline_annual_i,
                                             df_infections_vaccine_annual_i)
    
    
    ############################
    ### DEFINE DISCOUNT RATE ###
    ############################
    
    discRate = 0.035
    
    ######################################################################################
    ### MERGE INFECTIONS WITH PARAMETERS AND VE_SCENARIOS, AND CALCULATE BASE OUTCOMES ###
    ######################################################################################
    
    ### APPLY SNHL TO ALL SYMPTOMATIC CASES VS ONLY SEVERE DEPENDING ON "SIM_SCEN"
    
    if(!sim_scen %in% c("scen1", "scen2", "scen3")){warning("WRONG SIMULATION SCENARIO SPECIFIED")}
    
    ### SCEN1 or SCEN3: SNHL applies to all symptomatic infections
    if(sim_scen %in% c("scen1", "scen3")){
      ### Merge annual infections with base parameters
      df_outcomes_econ_wide_base_i = df_infections_final_annual_i%>%
        left_join(., df_params_all%>%
                    filter(GID_1 == GID_1_i), by = c("GID_1", "Year", "Sex", "Age", "ag", "n_draw"))%>%
        # main outcomes
        mutate(N_mild = N_infection * (prob_symptoms_str - prob_hosp_str) * VE_mild,
               N_hospital = N_infection * prob_hosp_str * VE_severe,
               N_death = N_hospital * prob_death_str,
               N_snhl = N_mild * prob_snhl + (N_hospital - N_death) * prob_snhl,
               N_fl = case_when(Sex == "Female_Preg" ~ prob_fl_lassa * N_hospital,
                                T ~ 0),
               N_nnd = case_when(Sex == "Female_Preg" ~ prob_nnd_lassa * N_hospital,
                                 T ~ 0),
               N_catastrophic = N_hospital * prob_catastrophic,
               N_impoverished = N_hospital * prob_impoverishment
        )
    }
    
    ### SCEN2: SNHL applies only to survivors of severe infection
    if(sim_scen == "scen2"){
      df_outcomes_econ_wide_base_i = df_infections_final_annual_i%>%
        left_join(., df_params_all%>%
                    filter(GID_1 == GID_1_i), by = c("GID_1", "Year", "Sex", "Age", "ag", "n_draw"))%>%
        # main outcomes
        mutate(N_mild = N_infection * (prob_symptoms_str - prob_hosp_str) * VE_mild,
               N_hospital = N_infection * prob_hosp_str * VE_severe,
               N_death = N_hospital * prob_death_str,
               N_snhl = (N_hospital - N_death) * prob_snhl,
               N_fl = case_when(Sex == "Female_Preg" ~ prob_fl_lassa * N_hospital,
                                T ~ 0),
               N_nnd = case_when(Sex == "Female_Preg" ~ prob_nnd_lassa * N_hospital,
                                 T ~ 0),
               N_catastrophic = N_hospital * prob_catastrophic,
               N_impoverished = N_hospital * prob_impoverishment
        )
    }
    
   
    
    
    ##########################
    ### Calculate outcomes ###
    ##########################
    
    dt_outcomes_econ_wide_all_i = df_outcomes_econ_wide_base_i%>%
      mutate(year_disc = Year - 2025)%>%
      # create N_death_total that includes neonatal deaths (and foetal loss if included) in total deaths
      mutate(N_hospital_nnd = N_nnd,
             N_death_total_noFL = N_death + N_nnd,
             N_death_total = N_death + N_nnd + N_fl)%>%
      # future losses: both undiscounted and discounted values (ref for continuous discount rate: https://resource-allocation.biomedcentral.com/articles/10.1186/1478-7547-11-18)
      mutate(YWL_death = N_death*future_work_years,
             YWL_death_disc = N_death*future_work_years_disc,
             YWL_snhl = N_snhl*future_work_years_snhl*0.18, ## assumed 18% reduction in labour force participation due to SNHL
             YWL_snhl_disc = N_snhl*future_work_years_snhl_disc*0.18,
             YLS = N_snhl*future_life_years_snhl,
             YLS_disc = N_snhl*future_life_years_snhl_disc)%>%
      # calculate DALYs for fever (community), hospitalization and death
      mutate(DALY_fever = N_mild*DALYperpatient_fever, # DALYs for those with fever in community
             DALY_hospital = (N_hospital-N_death)*DALYperpatient_hosp_survived + N_death*DALYperpatient_hosp_died,
             DALY_acute = DALY_fever + DALY_hospital,
             DALY_snhl = disutility_snhl*YLS,
             DALY_snhl_disc = disutility_snhl*YLS_disc,
             DALY_death = N_death*future_life_years,,
             DALY_death_disc = N_death*future_life_years_disc,
             DALY_nnd = N_nnd*future_life_years_nnd,
             DALY_nnd_disc = N_nnd*future_life_years_nnd_disc,
             DALY_fl = N_fl*future_life_years_nnd,
             DALY_fl_disc = N_fl*future_life_years_nnd_disc,
             DALY_death_total_noFL = DALY_death + DALY_nnd,
             DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
             DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
             DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
             DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
             DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
             DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
             DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
      # Calcualte value of statistical life (year), which does not include fetal loss
      # VSLY includes neonatal death with life expectancy at birth
      mutate(Cost_VSL = N_death_total_noFL*cost_per_vsl,
             Cost_VSLY = DALY_death*cost_per_vsl/future_life_years_nnd + DALY_nnd*cost_per_vsl/future_life_years_nnd)%>%
      # calculate DALY cost
      mutate(Cost_DALY_fever = DALY_fever*cost_per_daly,
             Cost_DALY_fever_disc = DALY_fever*cost_per_daly*(1/(1+discRate)^year_disc),
             Cost_DALY_hospital = DALY_hospital*cost_per_daly,
             Cost_DALY_hospital_disc = DALY_hospital*cost_per_daly*(1/(1+discRate)^year_disc),
             Cost_DALY_snhl = DALY_snhl*cost_per_daly,
             Cost_DALY_snhl_disc = DALY_snhl_disc*cost_per_daly*(1/(1+discRate)^year_disc),
             Cost_DALY_death_total_noFL = DALY_death_total_noFL*cost_per_daly,
             Cost_DALY_death_total_noFL_disc = DALY_death_total_noFL_disc*cost_per_daly*(1/(1+discRate)^year_disc),
             Cost_DALY_death_total = DALY_death_total*cost_per_daly,
             Cost_DALY_death_total_disc = DALY_death_total_disc*cost_per_daly*(1/(1+discRate)^year_disc),
             Cost_DALY_total_noFL = DALY_total_noFL*cost_per_daly,
             Cost_DALY_total_noFL_disc = (DALY_total_noFL_disc*cost_per_daly)*(1/(1+discRate)^year_disc),
             Cost_DALY_total = DALY_total*cost_per_daly,
             Cost_DALY_total_disc = (DALY_total_disc*cost_per_daly)*(1/(1+discRate)^year_disc))%>%
      # calculate care costs, including OOP costs vs government care costs in outpatients and hospital
      mutate(Cost_outpatient_gvt = N_mild*prob_treat_comm_gvt*cost_per_outpatientvisit,
             Cost_outpatient_gvt_disc = Cost_outpatient_gvt*(1/(1+discRate)^year_disc),
             Cost_outpatient_oop = N_mild*(prob_treat_comm_any - prob_treat_comm_gvt)*cost_per_outpatientvisit,
             Cost_outpatient_oop_disc = Cost_outpatient_oop*(1/(1+discRate)^year_disc),
             Cost_hosp_gvt = N_hospital*cost_per_hosp_gvt,
             Cost_hosp_gvt_disc = Cost_hosp_gvt*(1/(1+discRate)^year_disc),
             Cost_hosp_oop = N_hospital*cost_per_hosp_oop,
             Cost_hosp_oop_disc = Cost_hosp_oop*(1/(1+discRate)^year_disc),
             Cost_hosp_nnd_gvt = N_hospital_nnd*cost_per_hosp_gvt,
             Cost_hosp_nnd_gvt_disc = Cost_hosp_nnd_gvt*(1/(1+discRate)^year_disc),
             Cost_hosp_nnd_oop = N_hospital_nnd*cost_per_hosp_oop,
             Cost_hosp_nnd_oop_disc = Cost_hosp_nnd_oop*(1/(1+discRate)^year_disc),
             Cost_care_oop = Cost_outpatient_oop + Cost_hosp_oop,
             Cost_care_oop_disc = Cost_care_oop*(1/(1+discRate)^year_disc),
             Cost_care_gvt = Cost_outpatient_gvt + Cost_hosp_gvt,
             Cost_care_gvt_disc = Cost_care_gvt*(1/(1+discRate)^year_disc),
             Cost_care_total = Cost_care_oop + Cost_care_gvt,
             Cost_care_total_disc = Cost_care_total*(1/(1+discRate)^year_disc))%>%
      # calculate productivity losses
      mutate(Cost_prod_fever = N_mild*EmploymentRate*GNI_daily*dur_fever,
             Cost_prod_fever_disc = Cost_prod_fever*(1/(1+discRate)^year_disc),
             Cost_prod_hospital = (N_hospital-N_death)*EmploymentRate*GNI_daily*(dur_ill_prehosp + dur_hosp_survived) + N_death*EmploymentRate*GNI_daily*(dur_ill_prehosp + dur_hosp_died),
             Cost_prod_hospital_disc = Cost_prod_hospital*(1/(1+discRate)^year_disc),
             Cost_prod_death = GNI * YWL_death,
             Cost_prod_death_disc = GNI * YWL_death_disc * (1/(1+discRate)^year_disc),
             Cost_prod_snhl = GNI * YWL_snhl,
             Cost_prod_snhl_disc = GNI * YWL_snhl_disc * (1/(1+discRate)^year_disc),
             Cost_prod_total = Cost_prod_fever + Cost_prod_hospital + Cost_prod_death + Cost_prod_snhl,
             Cost_prod_total_disc = Cost_prod_fever_disc + Cost_prod_hospital_disc + Cost_prod_death_disc + Cost_prod_snhl_disc)%>%
      # group societal costs
      mutate(Cost_societal = Cost_care_total + Cost_prod_total,
             Cost_societal_disc = Cost_care_total_disc + Cost_prod_total_disc)%>%
      dplyr::select(-all_of(vec_variables_to_drop))%>%
      as.data.table()
    
    
    ### GROUP AGES AND SUM (USING DATA TABLE)
    grouping_columns <- c("GID_0", "Country", "GID_1", "strategy", "Sex", "Year", "ag", "n_draw", "VE_scenario", "VE_mild", "VE_severe")
    
    dt_outcomes_econ_wide_all_ag_i <- dt_outcomes_econ_wide_all_i[, lapply(.SD, sum), by = grouping_columns]
    
    ### SAVE ECON OUTCOME DATA
    save(dt_outcomes_econ_wide_all_ag_i, file = paste0("dt_", sim_scen, "_outcomes_econ_wide_all_ag_", GID_1_i, "_", strategy_k, ".Rdata"))
    
    rm(df_infections_final_annual_i)
    rm(dt_outcomes_econ_wide_all_i)
    rm(dt_outcomes_econ_wide_all_ag_i)
    
  }
}



