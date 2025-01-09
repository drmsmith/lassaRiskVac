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

#######################################
### LOAD FINAL GROUPED OUTCOME DATA ###
#######################################

files_outcomes_grouped = list.files(paste0("outcomes/organised_data_grouped/", sim_scen, "/"))
for(file_i in files_outcomes_grouped){load(paste0("outcomes/organised_data_grouped/", sim_scen, "/", file_i))}


##########################
### LOAD VACCINE DOSES ###
##########################

load("vaccination/demand_forecast/df_doses_allDistricts_annual.Rdata")
load("vaccination/demand_forecast/df_doses_allDistricts_cumul.Rdata")
load("vaccination/demand_forecast/df_doses_medDistricts_annual.Rdata")
load("vaccination/demand_forecast/df_doses_medDistricts_cumul.Rdata")
load("vaccination/demand_forecast/df_doses_highDistricts_annual.Rdata")
load("vaccination/demand_forecast/df_doses_highDistricts_cumul.Rdata")

#####################################
### REATTRIBUTE NEONATAL OUTCOMES ### 
#####################################

### FOR DATA WITH AGE STRATIFICATION, UPDATE AGES OF NEONATAL OUTCOMES TO AGE GROUP ag = <2 ###

### Recalculate individual outcomes: N_fl, N_nnd, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
### Recalculate corresponding totals: N_death_total, N_death_total_noFL, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, 
### DALY_death_total_noFL, DALY_death_total_noFL_disc, DALY_death_total, DALY_death_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc

### FOR HOSPITALISATIONS, ENSURE THE NUMBER OF NEONATAL DEATHS IS NOT GREATER THAN THE NUMBER OF HOSPITALISATIONS UNDER 2s


#################################################################################
### NB: COST DATA ARE NOT REATTRIBUTED AND HENCE NOT CORRECTLY AGE STRATIFIED ###
#################################################################################

### Totals are correct, but should not display cost data (monetised DALYs, societal costs, VSL) by age or sex

########################################
### All Districts, cumulative by age ###
########################################

### Attribute neonatal and foetal loss outcomes to under 2s, instead of mothers
dt_outcomes_allDistricts_cumul_age_neonatal = dt_outcomes_allDistricts_cumul_age%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()


dt_outcomes_allDistricts_cumul_age_updated = dt_outcomes_allDistricts_cumul_age%>%
  dplyr::select(-c(N_nnd, N_fl, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_allDistricts_cumul_age_neonatal, by = c("strategy", "n_draw", "ag", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$N_nnd) - sum(dt_outcomes_allDistricts_cumul_age_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$N_fl) - sum(dt_outcomes_allDistricts_cumul_age_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_nnd) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_nnd_disc) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_fl) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_fl_disc) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$N_death_total_noFL) - sum(dt_outcomes_allDistricts_cumul_age_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$N_death_total) - sum(dt_outcomes_allDistricts_cumul_age_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_total_noFL) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_total_noFL_disc) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_total) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age$DALY_total_disc) - sum(dt_outcomes_allDistricts_cumul_age_updated$DALY_total_disc)) < 0.01)

### CHECK THAT NEONATAL DEATHS DO NOT EXCEED HOSPITALISATIONS IN UNDER 2s
dt_check_neonatal_hospital = dt_outcomes_allDistricts_cumul_age_updated%>%filter(ag == "<2", strategy == "baseline")
dt_check_neonatal_hospital$N_hospital > dt_check_neonatal_hospital$N_nnd

####################################################
### Medium and high Districts, cumulative by age ###
####################################################

dt_outcomes_medDistricts_cumul_age_neonatal = dt_outcomes_medDistricts_cumul_age%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()

dt_outcomes_medDistricts_cumul_age_updated = dt_outcomes_medDistricts_cumul_age%>%
  dplyr::select(-c(N_nnd, N_fl, #YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, 
                   DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_medDistricts_cumul_age_neonatal, by = c("strategy", "n_draw", "ag", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$N_nnd) - sum(dt_outcomes_medDistricts_cumul_age_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$N_fl) - sum(dt_outcomes_medDistricts_cumul_age_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_nnd) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_nnd_disc) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_fl) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_fl_disc) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$N_death_total_noFL) - sum(dt_outcomes_medDistricts_cumul_age_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$N_death_total) - sum(dt_outcomes_medDistricts_cumul_age_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_total_noFL) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_total_noFL_disc) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_total) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age$DALY_total_disc) - sum(dt_outcomes_medDistricts_cumul_age_updated$DALY_total_disc)) < 0.01)


##############################################
### High Districts only, cumulative by age ###
##############################################

dt_outcomes_highDistricts_cumul_age_neonatal = dt_outcomes_highDistricts_cumul_age%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()

dt_outcomes_highDistricts_cumul_age_updated = dt_outcomes_highDistricts_cumul_age%>%
  dplyr::select(-c(N_nnd, N_fl, #YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, 
                   DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_highDistricts_cumul_age_neonatal, by = c("strategy", "n_draw", "ag", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$N_nnd) - sum(dt_outcomes_highDistricts_cumul_age_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$N_fl) - sum(dt_outcomes_highDistricts_cumul_age_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_nnd) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_nnd_disc) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_fl) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_fl_disc) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$N_death_total_noFL) - sum(dt_outcomes_highDistricts_cumul_age_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$N_death_total) - sum(dt_outcomes_highDistricts_cumul_age_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_total_noFL) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_total_noFL_disc) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_total) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age$DALY_total_disc) - sum(dt_outcomes_highDistricts_cumul_age_updated$DALY_total_disc)) < 0.01)


################################################
### All Districts, cumulative by age and sex ###
################################################

dt_outcomes_allDistricts_cumul_age_sex_neonatal = dt_outcomes_allDistricts_cumul_age_sex%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()

dt_outcomes_allDistricts_cumul_age_sex_neonatal_M = dt_outcomes_allDistricts_cumul_age_sex_neonatal%>%
  mutate(N_nnd = N_nnd/2,
         N_fl = N_fl/2,
         DALY_nnd = DALY_nnd/2,
         DALY_nnd_disc = DALY_nnd_disc/2,
         DALY_fl = DALY_fl/2,
         DALY_fl_disc = DALY_fl_disc/2)%>%
  mutate(Sex = "Male")

dt_outcomes_allDistricts_cumul_age_sex_neonatal_F_notpreg = dt_outcomes_allDistricts_cumul_age_sex_neonatal_M%>%
  mutate(Sex = "Female_NotPreg")

dt_outcomes_allDistricts_cumul_age_sex_neonatal_F_preg = dt_outcomes_allDistricts_cumul_age_sex_neonatal_M%>%
  mutate(N_nnd = 0,
         N_fl = 0,
         DALY_nnd = 0,
         DALY_nnd_disc = 0,
         DALY_fl = 0,
         DALY_fl_disc = 0)%>%
  mutate(Sex = "Female_Preg")

dt_outcomes_allDistricts_cumul_age_sex_neonatal_MF = bind_rows(dt_outcomes_allDistricts_cumul_age_sex_neonatal_M,
                                                               dt_outcomes_allDistricts_cumul_age_sex_neonatal_F_notpreg,
                                                               dt_outcomes_allDistricts_cumul_age_sex_neonatal_F_preg)



dt_outcomes_allDistricts_cumul_age_sex_updated = dt_outcomes_allDistricts_cumul_age_sex%>%
  dplyr::select(-c(N_nnd, N_fl, #YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, 
                   DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_allDistricts_cumul_age_sex_neonatal_MF, by = c("strategy", "n_draw", "ag", "Sex", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$N_nnd) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$N_fl) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_nnd) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_nnd_disc) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_fl) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_fl_disc) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$N_death_total_noFL) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$N_death_total) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_total_noFL) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_total_noFL_disc) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_total) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_allDistricts_cumul_age_sex$DALY_total_disc) - sum(dt_outcomes_allDistricts_cumul_age_sex_updated$DALY_total_disc)) < 0.01)


############################################################
### Medium and high Districts, cumulative by age and sex ###
############################################################

dt_outcomes_medDistricts_cumul_age_sex_neonatal = dt_outcomes_medDistricts_cumul_age_sex%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()

dt_outcomes_medDistricts_cumul_age_sex_neonatal_M = dt_outcomes_medDistricts_cumul_age_sex_neonatal%>%
  mutate(N_nnd = N_nnd/2,
         N_fl = N_fl/2,
         DALY_nnd = DALY_nnd/2,
         DALY_nnd_disc = DALY_nnd_disc/2,
         DALY_fl = DALY_fl/2,
         DALY_fl_disc = DALY_fl_disc/2)%>%
  mutate(Sex = "Male")

dt_outcomes_medDistricts_cumul_age_sex_neonatal_F_notpreg = dt_outcomes_medDistricts_cumul_age_sex_neonatal_M%>%
  mutate(Sex = "Female_NotPreg")

dt_outcomes_medDistricts_cumul_age_sex_neonatal_F_preg = dt_outcomes_medDistricts_cumul_age_sex_neonatal_M%>%
  mutate(N_nnd = 0,
         N_fl = 0,
         DALY_nnd = 0,
         DALY_nnd_disc = 0,
         DALY_fl = 0,
         DALY_fl_disc = 0)%>%
  mutate(Sex = "Female_Preg")

dt_outcomes_medDistricts_cumul_age_sex_neonatal_MF = bind_rows(dt_outcomes_medDistricts_cumul_age_sex_neonatal_M,
                                                               dt_outcomes_medDistricts_cumul_age_sex_neonatal_F_notpreg,
                                                               dt_outcomes_medDistricts_cumul_age_sex_neonatal_F_preg)



dt_outcomes_medDistricts_cumul_age_sex_updated = dt_outcomes_medDistricts_cumul_age_sex%>%
  dplyr::select(-c(N_nnd, N_fl,# YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, 
                   DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_medDistricts_cumul_age_sex_neonatal_MF, by = c("strategy", "n_draw", "ag", "Sex", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$N_nnd) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$N_fl) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_nnd) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_nnd_disc) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_fl) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_fl_disc) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$N_death_total_noFL) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$N_death_total) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_total_noFL) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_total_noFL_disc) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_total) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_medDistricts_cumul_age_sex$DALY_total_disc) - sum(dt_outcomes_medDistricts_cumul_age_sex_updated$DALY_total_disc)) < 0.01)



######################################################
### High Districts only, cumulative by age and sex ###
######################################################

dt_outcomes_highDistricts_cumul_age_sex_neonatal = dt_outcomes_highDistricts_cumul_age_sex%>%
  group_by(strategy, n_draw, VE_scenario, VE_mild, VE_severe)%>%
  summarise(N_nnd = sum(N_nnd),
            N_fl = sum(N_fl),
            DALY_nnd = sum(DALY_nnd),
            DALY_nnd_disc = sum(DALY_nnd_disc),
            DALY_fl = sum(DALY_fl),
            DALY_fl_disc = sum(DALY_fl_disc))%>%
  mutate(ag = "<2")%>%
  ungroup()

dt_outcomes_highDistricts_cumul_age_sex_neonatal_M = dt_outcomes_highDistricts_cumul_age_sex_neonatal%>%
  mutate(N_nnd = N_nnd/2,
         N_fl = N_fl/2,
         DALY_nnd = DALY_nnd/2,
         DALY_nnd_disc = DALY_nnd_disc/2,
         DALY_fl = DALY_fl/2,
         DALY_fl_disc = DALY_fl_disc/2)%>%
  mutate(Sex = "Male")

dt_outcomes_highDistricts_cumul_age_sex_neonatal_F_notpreg = dt_outcomes_highDistricts_cumul_age_sex_neonatal_M%>%
  mutate(Sex = "Female_NotPreg")

dt_outcomes_highDistricts_cumul_age_sex_neonatal_F_preg = dt_outcomes_highDistricts_cumul_age_sex_neonatal_M%>%
  mutate(N_nnd = 0,
         N_fl = 0,
         DALY_nnd = 0,
         DALY_nnd_disc = 0,
         DALY_fl = 0,
         DALY_fl_disc = 0)%>%
  mutate(Sex = "Female_Preg")

dt_outcomes_highDistricts_cumul_age_sex_neonatal_MF = bind_rows(dt_outcomes_highDistricts_cumul_age_sex_neonatal_M,
                                                               dt_outcomes_highDistricts_cumul_age_sex_neonatal_F_notpreg,
                                                               dt_outcomes_highDistricts_cumul_age_sex_neonatal_F_preg)



dt_outcomes_highDistricts_cumul_age_sex_updated = dt_outcomes_highDistricts_cumul_age_sex%>%
  dplyr::select(-c(N_nnd, N_fl, #YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, 
                   DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc))%>%
  left_join(., dt_outcomes_highDistricts_cumul_age_sex_neonatal_MF, by = c("strategy", "n_draw", "ag", "Sex", "VE_scenario", "VE_mild", "VE_severe"))%>%
  mutate_if(is.numeric,coalesce,0)%>%
  mutate(N_death_total_noFL = N_death + N_nnd,
         N_death_total = N_death + N_nnd + N_fl,             
         DALY_death_total_noFL = DALY_death + DALY_nnd,
         DALY_death_total_noFL_disc = DALY_death_disc + DALY_nnd_disc,
         DALY_death_total = DALY_death + DALY_nnd + DALY_fl,
         DALY_death_total_disc = DALY_death_disc + DALY_nnd_disc + DALY_fl_disc,
         DALY_total_noFL = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd,
         DALY_total_noFL_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc,
         DALY_total = DALY_fever + DALY_hospital + DALY_snhl + DALY_death + DALY_nnd + DALY_fl,
         DALY_total_disc = DALY_fever + DALY_hospital + DALY_snhl_disc + DALY_death_disc + DALY_nnd_disc + DALY_fl_disc)%>%
  mutate(N_cases = N_mild + N_hospital)

### CHECK THAT UPDATED TOTALS ARE THE SAME AS INITIAL TOTALS ###
# Set 1: N_nnd, N_fl, YLL_fl, YLL_fl_disc, YLL_nnd, YLL_nnd_disc, DALY_nnd, DALY_nnd_disc, DALY_fl, DALY_fl_disc
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$N_nnd) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$N_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$N_fl) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$N_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_nnd) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_nnd)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_nnd_disc) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_nnd_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_fl) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_fl)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_fl_disc) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_fl_disc)) < 0.01)

# Set 2: N_death_total_noFL, N_death_total, YLL_total_noFL, YLL_total_noFL_disc, YLL_total, YLL_total_disc, DALY_total_noFL, DALY_total_noFL_disc, DALY_total, DALY_total_disc
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$N_death_total_noFL) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$N_death_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$N_death_total) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$N_death_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_total_noFL) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_total_noFL)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_total_noFL_disc) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_total_noFL_disc)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_total) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_total)) < 0.01)
stopifnot(abs(sum(dt_outcomes_highDistricts_cumul_age_sex$DALY_total_disc) - sum(dt_outcomes_highDistricts_cumul_age_sex_updated$DALY_total_disc)) < 0.01)



########################################################################
### FOR NON-AGE-STRATIFIED DATA, ADD ADDITIONAL SUMMARY CALCULATIONS ###
########################################################################
### N_cases = N_mild + N_hospital

### All districts
dt_outcomes_allDistricts_annual_updated = dt_outcomes_allDistricts_annual%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

dt_outcomes_allDistricts_cumul_updated = dt_outcomes_allDistricts_cumul%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

### Medium and high districts
dt_outcomes_medDistricts_annual_updated = dt_outcomes_medDistricts_annual%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

dt_outcomes_medDistricts_cumul_updated = dt_outcomes_medDistricts_cumul%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

### High districts only
dt_outcomes_highDistricts_annual_updated = dt_outcomes_highDistricts_annual%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

dt_outcomes_highDistricts_cumul_updated = dt_outcomes_highDistricts_cumul%>%
  mutate(N_cases = N_mild + N_hospital,
         Cost_societal_MDALY = Cost_societal + Cost_DALY_total,
         Cost_societal_MDALY_noFL = Cost_societal + Cost_DALY_total_noFL,
         Cost_societal_MDALY_disc = Cost_societal_disc + Cost_DALY_total_disc,
         Cost_societal_MDALY_noFL_disc = Cost_societal_disc + Cost_DALY_total_noFL_disc)

########################################################################################################
### PERSON-TIME IN ANNUAL VS CUMULATIVE DATA FOR ALL DISTRICTS, MED/HIGH BURDEN AND HIGH BURDEN ONLY ###
########################################################################################################

load("demography/df_population_ag.Rdata")

### All districts ###

# cumul
df_pt_allDistricts_cumul = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"))%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age
df_pt_allDistricts_cumul_age = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(ag)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age and sex
df_pt_allDistricts_cumul_age_sex = df_population_ag%>%
  mutate(Sex = case_when(Sex == "Preg_Female" ~ "Female_Preg",
                         Sex == "Not_Preg_Female" ~ "Female_NotPreg",
                         T ~ Sex))%>%
  filter(Sex %in% c("Male", "Female_NotPreg", "Female_Preg"))%>%
  group_by(ag, Sex)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# annual
df_pt_allDistricts_annual = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"))%>%
  group_by(Year)%>%
  summarise(person_time = sum(UN_scaled_subnational))



### Med/high districts ###

# cumul
df_pt_medDistricts_cumul = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_medDistricts)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age
df_pt_medDistricts_cumul_age = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_medDistricts)%>%
  group_by(ag)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age and sex
df_pt_medDistricts_cumul_age_sex = df_population_ag%>%
  mutate(Sex = case_when(Sex == "Preg_Female" ~ "Female_Preg",
                         Sex == "Not_Preg_Female" ~ "Female_NotPreg",
                         T ~ Sex))%>%
  filter(Sex %in% c("Male", "Female_NotPreg", "Female_Preg"),
         GID_1 %in% vec_GID_1_medDistricts)%>%
  group_by(ag, Sex)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# annual
df_pt_medDistricts_annual = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_medDistricts)%>%
  group_by(Year)%>%
  summarise(person_time = sum(UN_scaled_subnational))



### High districts ###

# cumul
df_pt_highDistricts_cumul = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_highDistricts)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age
df_pt_highDistricts_cumul_age = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_highDistricts)%>%
  group_by(ag)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# cumul by age and sex
df_pt_highDistricts_cumul_age_sex = df_population_ag%>%
  mutate(Sex = case_when(Sex == "Preg_Female" ~ "Female_Preg",
                         Sex == "Not_Preg_Female" ~ "Female_NotPreg",
                         T ~ Sex))%>%
  filter(Sex %in% c("Male", "Female_NotPreg", "Female_Preg"),
         GID_1 %in% vec_GID_1_highDistricts)%>%
  group_by(ag, Sex)%>%
  summarise(person_time = sum(UN_scaled_subnational))

# annual
df_pt_highDistricts_annual = df_population_ag%>%
  filter(Sex %in% c("Male", "Female"),
         GID_1 %in% vec_GID_1_highDistricts)%>%
  group_by(Year)%>%
  summarise(person_time = sum(UN_scaled_subnational))


##################################
### SENSITIVITY ANALYSIS: PRCC ###
##################################

### LOAD PARAMS ###

### Load monte carlo parameters
df_params_montecarlo = loadRData("parameters_data/params_montecarlo.Rdata")

### Load age-specific CFR
df_cfr_prcc = read.csv("risk/df_CFR_finalAges_preg.csv")%>%
  filter(Sex == "Male")%>%
  mutate(param = paste0("cfr_", ag))%>%
  dplyr::select(c(n_draw, param, prob_death_str))%>%
  pivot_wider(id_cols = "n_draw", names_from = "param", values_from = "prob_death_str")

### Load age-specific IHR
df_ihr_prcc = read.csv("risk/df_infection_hospital_risk_str.csv")%>%
  filter(Sex == "Male")%>%
  mutate(param = paste0("ihr_", ag))%>%
  dplyr::select(c(n_draw, param, prob_hosp_str))%>%
  pivot_wider(id_cols = "n_draw", names_from = "param", values_from = "prob_hosp_str")

### Join params
df_params_prcc = df_params_montecarlo%>%
  left_join(., df_cfr_prcc, by = "n_draw")%>%
  left_join(., df_ihr_prcc, by = "n_draw")

df_par_names_clean = data.frame(var = c("n_draw", "prob_symptoms", "prob_treat_comm_any", "prob_treat_comm_gvt",
                                        "prob_hosp", "prob_death", "prob_snhl", "prob_fl_lassa",
                                        "prob_nnd_lassa", "dur_fever", "dur_ill_prehosp", "dur_hosp_survived",
                                        "dur_hosp_died", "dur_snhl", "dur_snhl_disc", "disutility_fever",
                                        "disutility_hospital", "disutility_snhl", "DALYperpatient_fever", "DALYperpatient_hosp_survived",
                                        "DALYperpatient_hosp_died", "cfr_<2", "cfr_2-14", "cfr_15-24",
                                        "cfr_25-34", "cfr_35-49", "cfr_50+", "ihr_15-24",
                                        "ihr_2-14", "ihr_25-34", "ihr_35-49", "ihr_50+",
                                        "ihr_<2"),
                                var_clean = c("n_draw", "P(symptoms, any)", "P(seeking outpatient care)", "P(outpatient care reimbursed)",
                                              "P(hospitalisation)", "P(death)", "P(SNHL)", "P(foetal loss)",
                                              "P(neonatal death)", "D(mild/moderate disease)", "D(lag to hospitalisation)", "D(severe disease, survivors)",
                                              "D(severe disease, fatal cases)", "D(SNHL)", "D(SNHL, discounted)", "U(mild/moderate disease)",
                                              "U(severe disease)", "U(SNHL)", "DALY(mild/moderate disease)", "DALY(severe disease, survivors)",
                                              "DALY(severe disease, fatal cases)", "CFR (<2)", "CFR (2-14)", "CFR (15-24)",
                                              "CFR (25-34)", "CFR (35-49)", "CFR (50+)", "IHR (15-24)",
                                              "IHR (2-14)", "IHR (25-34)", "IHR (35-49)", "IHR (50+)",
                                              "IHR (<2)"))

######################################
### PRCC: run for Healthcare costs ###
######################################

### Combine params with selected outcome
df_prcc_hcc1 = df_params_prcc%>%
  left_join(., dt_outcomes_allDistricts_cumul_updated%>%
              filter(strategy == "baseline")%>%
              dplyr::select(c(n_draw, Cost_care_total_disc)),
            by = "n_draw")%>%
  dplyr::select(-c(n_draw, dur_snhl_disc, prob_hosp , prob_death))

### PRCC with all params (will not run because of multicollinearity in some params)
# res_prcc_soc1 = epi.prcc(df_prcc_soc1, sided.test = 2)

### exclude any parameters without variance
apply(df_prcc_hcc1[,1:(ncol(df_prcc_hcc1)-1)], 2, var)

### check for multicollinearity
cor_matrix_hcc1 <- cor(df_prcc_hcc1[,1:(ncol(df_prcc_hcc1)-1)], method = "spearman")
print(cor_matrix_hcc1)
corrplot(cor_matrix_hcc1)

### remove duration of acute illness parameters, and disutility for acute disease,
### which have extremely high autocorrelation and variance near zero,
df_prcc_hcc2 = df_prcc_hcc1%>%dplyr::select(-c(dur_fever, dur_ill_prehosp, dur_hosp_survived, dur_hosp_died, 
                                               disutility_fever, DALYperpatient_fever, prob_treat_comm_any))

cor_matrix_hcc2 <- cor(df_prcc_hcc2[,1:(ncol(df_prcc_hcc2)-1)], method = "spearman")
print(cor_matrix_soc2)
corrplot(cor_matrix_soc2)

res_prcc_hcc2 = epi.prcc(df_prcc_hcc2, sided.test = 2)%>%
  mutate(p_sig = case_when(p.value < 0.001 ~ "p < 0.001",
                           p.value >= 0.001 & p.value < 0.01 ~ "p < 0.01",
                           p.value >= 0.01 & p.value < 0.05 ~ "p < 0.05",
                           T ~ "p ≥ 0.05"))


if(render_plots == T){
  
  res_prcc_hcc2%>%
    left_join(., df_par_names_clean, by = "var")%>%
    # mutate(var = factor(var, levels = res_prcc_soc2_order$var))%>%
    ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_bar(stat = "identity", colour = "black")+
    geom_errorbar(width = 0.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("PRCC (cumulative societal costs)")+xlab("")+
    scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))
  
}


####################################
### PRCC: run for Societal costs ###
####################################

### Combine params with selected outcome
df_prcc_soc1 = df_params_prcc%>%
  left_join(., dt_outcomes_allDistricts_cumul_updated%>%
              filter(strategy == "baseline")%>%
              dplyr::select(c(n_draw, Cost_societal_disc)),
            by = "n_draw")%>%
  dplyr::select(-c(n_draw, dur_snhl_disc, prob_hosp , prob_death))

### PRCC with all params (will not run because of multicollinearity in some params)
# res_prcc_soc1 = epi.prcc(df_prcc_soc1, sided.test = 2)

### exclude any parameters without variance
apply(df_prcc_soc1[,1:(ncol(df_prcc_soc1)-1)], 2, var)

### check for multicollinearity
cor_matrix_soc1 <- cor(df_prcc_soc1[,1:(ncol(df_prcc_soc1)-1)], method = "spearman")
print(cor_matrix_soc1)
corrplot(cor_matrix_soc1)

### remove duration of acute illness parameters, and disutility for acute disease,
### which have extremely high autocorrelation and variance near zero,
df_prcc_soc2 = df_prcc_soc1%>%dplyr::select(-c(dur_fever, dur_ill_prehosp, dur_hosp_survived, dur_hosp_died, 
                                       disutility_fever, DALYperpatient_fever, prob_treat_comm_any))

cor_matrix_soc2 <- cor(df_prcc_soc2[,1:(ncol(df_prcc_soc2)-1)], method = "spearman")
print(cor_matrix_soc2)
corrplot(cor_matrix_soc2)

res_prcc_soc2 = epi.prcc(df_prcc_soc2, sided.test = 2)%>%
  mutate(p_sig = case_when(p.value < 0.001 ~ "p < 0.001",
                           p.value >= 0.001 & p.value < 0.01 ~ "p < 0.01",
                           p.value >= 0.01 & p.value < 0.05 ~ "p < 0.05",
                           T ~ "p ≥ 0.05"))

### update variable names and order by descending PRCC
# res_prcc_soc2_order = res_prcc_soc2%>%
#   arrange(desc(est))%>%
#   dplyr::select(var)

if(render_plots == T){
  
  res_prcc_soc2%>%
    left_join(., df_par_names_clean, by = "var")%>%
    # mutate(var = factor(var, levels = res_prcc_soc2_order$var))%>%
    ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_bar(stat = "identity", colour = "black")+
    geom_errorbar(width = 0.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("PRCC (cumulative societal costs)")+xlab("")+
    scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))
  
}



#####################################
### PRCC: run for Monetised DALYs ###
#####################################

### Combine params with selected outcome
df_prcc_mdaly1 = df_params_prcc%>%
  left_join(., dt_outcomes_allDistricts_cumul_updated%>%
              filter(strategy == "baseline")%>%
              dplyr::select(c(n_draw, Cost_DALY_total_noFL_disc)),
            by = "n_draw")%>%
  dplyr::select(-c(n_draw, dur_snhl_disc, prob_hosp , prob_death))

### PRCC with all params (will not run because of multicollinearity in some params)
# res_prcc_mdaly1 = epi.prcc(df_prcc_mdaly1, sided.test = 2)

### exclude any parameters without variance
apply(df_prcc_mdaly1[,1:(ncol(df_prcc_mdaly1)-1)], 2, var)

### check for multicollinearity
cor_matrix_mdaly1 <- cor(df_prcc_mdaly1[,1:(ncol(df_prcc_mdaly1)-1)], method = "spearman")
print(cor_matrix_mdaly1)
corrplot(cor_matrix_mdaly1)

### remove duration of acute illness parameters, and disutility for acute disease,
### which have extremely high autocorrelation and variance near zero,
df_prcc_mdaly2 = df_prcc_mdaly1%>%dplyr::select(-c(dur_fever, dur_ill_prehosp, dur_hosp_survived, dur_hosp_died, 
                                               disutility_fever, DALYperpatient_fever, prob_treat_comm_any))

cor_matrix_mdaly2 <- cor(df_prcc_mdaly2[,1:(ncol(df_prcc_mdaly2)-1)], method = "spearman")
print(cor_matrix_mdaly2)
corrplot(cor_matrix_mdaly2)

res_prcc_mdaly2 = epi.prcc(df_prcc_mdaly2, sided.test = 2)%>%
  mutate(p_sig = case_when(p.value < 0.001 ~ "p < 0.001",
                           p.value >= 0.001 & p.value < 0.01 ~ "p < 0.01",
                           p.value >= 0.01 & p.value < 0.05 ~ "p < 0.05",
                           T ~ "p ≥ 0.05"))


if(render_plots == T){
  
  res_prcc_mdaly2%>%
    left_join(., df_par_names_clean, by = "var")%>%
    # mutate(var = factor(var, levels = res_prcc_mdaly2_order$var))%>%
    ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_bar(stat = "identity", colour = "black")+
    geom_errorbar(width = 0.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("PRCC (cumulative monetised DALYs)")+xlab("")+
    scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))
  
}


####################################################
### PRCC: run for Social costs + Monetised DALYs ###
####################################################

### Combine params with selected outcome
df_prcc_total1 = df_params_prcc%>%
  left_join(., dt_outcomes_allDistricts_cumul_updated%>%
              filter(strategy == "baseline")%>%
              dplyr::select(c(n_draw, Cost_societal_MDALY_noFL_disc)),
            by = "n_draw")%>%
  dplyr::select(-c(n_draw, dur_snhl_disc, prob_hosp , prob_death))

### PRCC with all params (will not run because of multicollinearity in some params)
# df_prcc_total1 = epi.prcc(df_prcc_total1, sided.test = 2)

### exclude any parameters without variance
apply(df_prcc_total1[,1:(ncol(df_prcc_total1)-1)], 2, var)

### check for multicollinearity
cor_matrix_total1 <- cor(df_prcc_total1[,1:(ncol(df_prcc_total1)-1)], method = "spearman")
print(cor_matrix_total1)
corrplot(cor_matrix_total1)

### remove duration of acute illness parameters, and disutility for acute disease,
### which have extremely high autocorrelation and variance near zero,
df_prcc_total2 = df_prcc_total1%>%dplyr::select(-c(dur_fever, dur_ill_prehosp, dur_hosp_survived, dur_hosp_died, 
                                       disutility_fever, DALYperpatient_fever, prob_treat_comm_any))

cor_matrix_total2 <- cor(df_prcc_total2[,1:(ncol(df_prcc_total2)-1)], method = "spearman")
print(cor_matrix_total2)
corrplot(cor_matrix_total2)

res_prcc_total2 = epi.prcc(df_prcc_total2, sided.test = 2)%>%
  mutate(p_sig = case_when(p.value < 0.001 ~ "p < 0.001",
                           p.value >= 0.001 & p.value < 0.01 ~ "p < 0.01",
                           p.value >= 0.01 & p.value < 0.05 ~ "p < 0.05",
                           T ~ "p ≥ 0.05"))


if(render_plots == T){
  
  res_prcc_total2%>%
    left_join(., df_par_names_clean, by = "var")%>%
    #mutate(var = factor(var, levels = res_prcc2_order$var))%>%
    ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_bar(stat = "identity", colour = "black")+
    geom_errorbar(width = 0.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("PRCC")+xlab("")+
    scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))
  
}


##########################
### PRCC: run for VSLY ###
##########################

### Combine params with selected outcome
df_prcc_vsly1 = df_params_prcc%>%
  left_join(., dt_outcomes_allDistricts_cumul_updated%>%
              filter(strategy == "baseline")%>%
              dplyr::select(c(n_draw, Cost_VSLY)),
            by = "n_draw")%>%
  dplyr::select(-c(n_draw, dur_snhl_disc, prob_hosp , prob_death))

### PRCC with all params (will not run because of multicollinearity in some params)
# df_prcc_total1 = epi.prcc(df_prcc_total1, sided.test = 2)

### exclude any parameters without variance
apply(df_prcc_vsly1[,1:(ncol(df_prcc_vsly1)-1)], 2, var)

### check for multicollinearity
cor_matrix_vsly1 <- cor(df_prcc_vsly1[,1:(ncol(df_prcc_vsly1)-1)], method = "spearman")
print(cor_matrix_vsly1)
corrplot(cor_matrix_vsly1)

### remove duration of acute illness parameters, and disutility for acute disease,
### which have extremely high autocorrelation and variance near zero,
df_prcc_vsly2 = df_prcc_vsly1%>%dplyr::select(-c(dur_fever, dur_ill_prehosp, dur_hosp_survived, dur_hosp_died, 
                                                   disutility_fever, DALYperpatient_fever, prob_treat_comm_any))

cor_matrix_vsly2 <- cor(df_prcc_vsly2[,1:(ncol(df_prcc_vsly2)-1)], method = "spearman")
print(cor_matrix_vsly2)
corrplot(cor_matrix_vsly2)

res_prcc_vsly2 = epi.prcc(df_prcc_vsly2, sided.test = 2)%>%
  mutate(p_sig = case_when(p.value < 0.001 ~ "p < 0.001",
                           p.value >= 0.001 & p.value < 0.01 ~ "p < 0.01",
                           p.value >= 0.01 & p.value < 0.05 ~ "p < 0.05",
                           T ~ "p ≥ 0.05"))


if(render_plots == T){
  
  res_prcc_vsly2%>%
    left_join(., df_par_names_clean, by = "var")%>%
    #mutate(var = factor(var, levels = res_prcc2_order$var))%>%
    ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_bar(stat = "identity", colour = "black")+
    geom_errorbar(width = 0.25)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))+
    ylab("PRCC")+xlab("")+
    scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))
  
}

###############################################################
### COMBINE PRCC PLOT FOR DALY, SOCIETAL COSTS AND COMBINED ### 
###############################################################

res_prcc3 = bind_rows(res_prcc_mdaly2%>%
                        mutate(outcome = "Monetised DALYs"),
                      res_prcc_soc2%>%
                        mutate(outcome = "Societal costs"),
                      res_prcc_total2%>%
                        mutate(outcome = "Monetised DALYs + societal costs"))

p_prcc_final = res_prcc3%>%
  left_join(., df_par_names_clean, by = "var")%>%
  mutate(outcome = factor(outcome, levels = c("Monetised DALYs", "Societal costs", "Monetised DALYs + societal costs")))%>%
  ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("PRCC")+xlab("")+
  scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))+
  facet_wrap(facets = vars(outcome), nrow = 3)

ggsave(p_prcc_final, file = paste0("p_", sim_scen, "_prcc.png"),
       width = 18, height = 18, units = "cm")

#####################################################
### COMBINE PRCC PLOT FOR SOCIETAL COSTS AND VSLY ### 
#####################################################

res_prcc4 = bind_rows(res_prcc_total2%>%
                        mutate(outcome = "Societal costs"),
                      res_prcc_vsly2%>%
                        mutate(outcome = "VSLY"))

p_prcc_final2 = res_prcc4%>%
  left_join(., df_par_names_clean, by = "var")%>%
  mutate(outcome = factor(outcome, levels = c("Societal costs", "VSLY")))%>%
  ggplot(., aes(x = var_clean, y = est, ymin = lower, ymax = upper, fill = p_sig))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_bar(stat = "identity", colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Partial rank correlation coefficient")+xlab("")+
  scale_fill_manual("p value", values = rev(c('#ffffb2','#fecc5c','#fd8d3c','#f03b20')))+
  facet_wrap(facets = vars(outcome), nrow = 3)

ggsave(p_prcc_final2, file = paste0("p_", sim_scen, "_prcc2.png"),
       width = 16, height = 14, units = "cm")

######################
### SUMMARISE DATA ###
######################

################################
### PREPARE DATA SUMMARISING ###
################################

### GROUPING AND SUMMARY COLUMNS ###

### Annual
grouping_columns_annual = c("strategy", "Year", "VE_scenario", "VE_mild", "VE_severe")
summary_columns_annual = setdiff(names(dt_outcomes_allDistricts_annual_updated), c(grouping_columns_annual, "n_draw"))

### Cumul
grouping_columns_cumul = c("strategy", "VE_scenario", "VE_mild", "VE_severe")
summary_columns_cumul = setdiff(names(dt_outcomes_allDistricts_cumul_updated), c(grouping_columns_cumul, "n_draw"))

### Cumul by age
grouping_columns_cumul_age = c("strategy", "VE_scenario", "VE_mild", "VE_severe", "ag")
summary_columns_cumul_age = setdiff(names(dt_outcomes_allDistricts_cumul_age_updated), c(grouping_columns_cumul_age, "n_draw"))

### Cumul by age and sex
grouping_columns_cumul_age_sex = c("strategy", "VE_scenario", "VE_mild", "VE_severe", "ag", "Sex")
summary_columns_cumul_age_sex = setdiff(names(dt_outcomes_allDistricts_cumul_age_sex_updated), c(grouping_columns_cumul_age_sex, "n_draw"))


###############################
### SUMMARISE ALL DISTRICTS ###
###############################

### All districts, annual ###

# Totals
dt_outcomes_summarisedTotals_allDistricts_annual = dt_outcomes_allDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))


# Rates
dt_outcomes_summarisedRates_allDistricts_annual = dt_outcomes_allDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_allDistricts_annual, by = "Year")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

### All districts, cumul ###

# Totals
dt_outcomes_summarisedTotals_allDistricts_cumul = dt_outcomes_allDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Totals per dose
dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy != "baseline")%>%
  left_join(., df_doses_allDistricts_cumul, by = "strategy")%>%
  mutate(mean = mean/doses,
         median = median/doses,
         q025 = q025/doses,
         q050 = q050/doses,
         q250 = q250/doses,
         q750 = q750/doses,
         q950 = q950/doses,
         q975 = q975/doses,
         mean_disc = mean*doses/doses_disc,
         median_disc = median*doses/doses_disc,
         q025_disc = q025*doses/doses_disc,
         q050_disc = q050*doses/doses_disc,
         q250_disc = q250*doses/doses_disc,
         q750_disc = q750*doses/doses_disc,
         q950_disc = q950*doses/doses_disc,
         q975_disc = q975*doses/doses_disc)
  

# Rates
dt_outcomes_summarisedRates_allDistricts_cumul = dt_outcomes_allDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  cross_join(., df_pt_allDistricts_cumul)%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



### All districts, cumul by age ###

# Totals
dt_outcomes_summarisedTotals_allDistricts_cumul_age = dt_outcomes_allDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_allDistricts_cumul_age = dt_outcomes_allDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_allDistricts_cumul_age, by = "ag")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



### All districts, cumul by age and sex ###

# Totals
dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex = dt_outcomes_allDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_allDistricts_cumul_age_sex = dt_outcomes_allDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_allDistricts_cumul_age_sex, by = c("ag", "Sex"))%>%
  mutate(value = case_when(person_time == 0 ~ 0,
                           person_time == 0 & value > 0 ~ NA,
                           T ~ value / person_time * 100000))%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



###########################################
### SUMMARISE MEDIUM AND HIGH DISTRICTS ###
###########################################

### Medium and high districts, annual ###

# Totals
dt_outcomes_summarisedTotals_medDistricts_annual = dt_outcomes_medDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_medDistricts_annual = dt_outcomes_medDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_medDistricts_annual, by = "Year")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

### Medium and high districts, cumul ###

# Totals
dt_outcomes_summarisedTotals_medDistricts_cumul = dt_outcomes_medDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

dt_outcomes_summarisedTotalsPerDose_medDistricts_cumul = dt_outcomes_summarisedTotals_medDistricts_cumul%>%
  filter(strategy != "baseline")%>%
  left_join(., df_doses_medDistricts_cumul, by = "strategy")%>%
  mutate(mean = mean/doses,
         median = median/doses,
         q025 = q025/doses,
         q050 = q050/doses,
         q250 = q250/doses,
         q750 = q750/doses,
         q950 = q950/doses,
         q975 = q975/doses,
         mean_disc = mean*doses/doses_disc,
         median_disc = median*doses/doses_disc,
         q025_disc = q025*doses/doses_disc,
         q050_disc = q050*doses/doses_disc,
         q250_disc = q250*doses/doses_disc,
         q750_disc = q750*doses/doses_disc,
         q950_disc = q950*doses/doses_disc,
         q975_disc = q975*doses/doses_disc)

# Rates
dt_outcomes_summarisedRates_medDistricts_cumul = dt_outcomes_medDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  cross_join(., df_pt_medDistricts_cumul)%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))


### Medium and high districts, cumul by age ###

# Totals
dt_outcomes_summarisedTotals_medDistricts_cumul_age = dt_outcomes_medDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_medDistricts_cumul_age = dt_outcomes_medDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_medDistricts_cumul_age, by = "ag")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))


### Medium and high districts, cumul by age and sex ###

# Totals
dt_outcomes_summarisedTotals_medDistricts_cumul_age_sex = dt_outcomes_medDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_medDistricts_cumul_age_sex = dt_outcomes_medDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_medDistricts_cumul_age_sex, by = c("ag", "Sex"))%>%
  mutate(value = case_when(person_time == 0 ~ 0,
                           person_time == 0 & value > 0 ~ NA,
                           T ~ value / person_time * 100000))%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))




#####################################
### SUMMARISE HIGH DISTRICTS ONLY ###
#####################################

### High districts only, annual ###

# Totals
dt_outcomes_summarisedTotals_highDistricts_annual = dt_outcomes_highDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_highDistricts_annual = dt_outcomes_highDistricts_annual_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_annual), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_highDistricts_annual, by = "Year")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_annual)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

### High districts only, cumul ###

# Totals
dt_outcomes_summarisedTotals_highDistricts_cumul = dt_outcomes_highDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

dt_outcomes_summarisedTotalsPerDose_highDistricts_cumul = dt_outcomes_summarisedTotals_highDistricts_cumul%>%
  filter(strategy != "baseline")%>%
  left_join(., df_doses_highDistricts_cumul, by = "strategy")%>%
  mutate(mean = mean/doses,
         median = median/doses,
         q025 = q025/doses,
         q050 = q050/doses,
         q250 = q250/doses,
         q750 = q750/doses,
         q950 = q950/doses,
         q975 = q975/doses,
         mean_disc = mean*doses/doses_disc,
         median_disc = median*doses/doses_disc,
         q025_disc = q025*doses/doses_disc,
         q050_disc = q050*doses/doses_disc,
         q250_disc = q250*doses/doses_disc,
         q750_disc = q750*doses/doses_disc,
         q950_disc = q950*doses/doses_disc,
         q975_disc = q975*doses/doses_disc)

# Rates
dt_outcomes_summarisedRates_highDistricts_cumul = dt_outcomes_highDistricts_cumul_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul), names_to = "outcome", values_to = "value")%>%
  cross_join(., df_pt_highDistricts_cumul)%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



### High districts only, cumul by age ###

# Totals
dt_outcomes_summarisedTotals_highDistricts_cumul_age = dt_outcomes_highDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_highDistricts_cumul_age = dt_outcomes_highDistricts_cumul_age_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_highDistricts_cumul_age, by = "ag")%>%
  mutate(value = value / person_time * 100000)%>%
  group_by(across(all_of(grouping_columns_cumul_age)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



### High districts only, cumul by age and sex ###

# Totals
dt_outcomes_summarisedTotals_highDistricts_cumul_age_sex = dt_outcomes_highDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))

# Rates
dt_outcomes_summarisedRates_highDistricts_cumul_age_sex = dt_outcomes_highDistricts_cumul_age_sex_updated%>%
  dplyr::select(-n_draw)%>%
  pivot_longer(
    cols = all_of(summary_columns_cumul_age_sex), names_to = "outcome", values_to = "value")%>%
  left_join(., df_pt_highDistricts_cumul_age_sex, by = c("ag", "Sex"))%>%
  mutate(value = case_when(person_time == 0 ~ 0,
                           person_time == 0 & value > 0 ~ NA,
                           T ~ value / person_time * 100000))%>%
  group_by(across(all_of(grouping_columns_cumul_age_sex)), outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            q025 = quantile(value, 0.025),
            q050 = quantile(value, 0.05),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q950 = quantile(value, 0.95),
            q975 = quantile(value, 0.975))



##################################
### SAVE FINAL SUMMARISED DATA ###
##################################

### ALL DISTRICTS ###
save(dt_outcomes_summarisedTotals_allDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_allDistricts_annual.Rdata"))
save(dt_outcomes_summarisedRates_allDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_allDistricts_annual.Rdata"))
save(dt_outcomes_summarisedTotals_allDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_allDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotalsPerDose_allDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedRates_allDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_allDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotals_allDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_allDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedRates_allDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_allDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_allDistricts_cumul_age_sex.Rdata"))
save(dt_outcomes_summarisedRates_allDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_allDistricts_cumul_age_sex.Rdata"))

### MEDIUM AND HIGH BURDEN DISTRICTS ###
save(dt_outcomes_summarisedTotals_medDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_medDistricts_annual.Rdata"))
save(dt_outcomes_summarisedRates_medDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_medDistricts_annual.Rdata"))
save(dt_outcomes_summarisedTotals_medDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_medDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotalsPerDose_medDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotalsPerDose_medDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedRates_medDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_medDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotals_medDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_medDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedRates_medDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_medDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedTotals_medDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_medDistricts_cumul_age_sex.Rdata"))
save(dt_outcomes_summarisedRates_medDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_medDistricts_cumul_age_sex.Rdata"))


### HIGH BURDEN DISTRICTS ONLY ###
save(dt_outcomes_summarisedTotals_highDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_highDistricts_annual.Rdata"))
save(dt_outcomes_summarisedRates_highDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_highDistricts_annual.Rdata"))
save(dt_outcomes_summarisedTotals_highDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_highDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotalsPerDose_highDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotalsPerDose_highDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedRates_highDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_highDistricts_cumul.Rdata"))
save(dt_outcomes_summarisedTotals_highDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_highDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedRates_highDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_highDistricts_cumul_age.Rdata"))
save(dt_outcomes_summarisedTotals_highDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedTotals_highDistricts_cumul_age_sex.Rdata"))
save(dt_outcomes_summarisedRates_highDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_summarisedRates_highDistricts_cumul_age_sex.Rdata"))

