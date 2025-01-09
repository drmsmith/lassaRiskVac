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


############################################
### SAVING FOR EACH DISTRICT OR GROUPED? ###
############################################

## If T, save each district
## If F, only compile and save at the end
aggrDistr = F

################################################
### EMPTY LISTS FOR FILLING UP FINAL RESULTS ###
################################################

### ALL DISTRICTS ###
### cumulative outcomes (combined 2025 to 2037)
list_outcomes_allDistricts_cumul_age_sex = list()
list_outcomes_allDistricts_cumul_age = list()
list_outcomes_allDistricts_cumul = list()

### annual outcomes (no age or sex aggregation to keep size manageable)
list_outcomes_allDistricts_annual = list()


############################################################
### LOOP THROUGH DISTRICTS AND EVALUATE RAW OUTCOME DATA ###
############################################################

for(GID_1_i in GID_1_final){
  
  print(paste0("Organising health-econ outcome data for ", GID_1_i))
  
  dt_outcomes_raw_i_adults = loadRData(paste0("outcomes/raw_data_ag/", sim_scen, "/dt_", sim_scen, "_outcomes_econ_wide_all_ag_", GID_1_i,"_adults.Rdata"))
  dt_outcomes_raw_i_children = loadRData(paste0("outcomes/raw_data_ag/", sim_scen, "/dt_", sim_scen, "_outcomes_econ_wide_all_ag_", GID_1_i,"_children.Rdata"))
  dt_outcomes_raw_i_elderly = loadRData(paste0("outcomes/raw_data_ag/", sim_scen, "/dt_", sim_scen, "_outcomes_econ_wide_all_ag_", GID_1_i,"_elderly.Rdata"))
  dt_outcomes_raw_i_wcba = loadRData(paste0("outcomes/raw_data_ag/", sim_scen, "/dt_", sim_scen, "_outcomes_econ_wide_all_ag_", GID_1_i,"_wcba.Rdata"))
  
  dt_outcomes_raw_i_baseline = dt_outcomes_raw_i_adults[strategy == "baseline", !c("Age", "year_disc")]
  dt_outcomes_raw_i_adults = dt_outcomes_raw_i_adults[strategy == "adults", !c("Age", "year_disc")]
  dt_outcomes_raw_i_children = dt_outcomes_raw_i_children[strategy == "children", !c("Age", "year_disc")]
  dt_outcomes_raw_i_elderly = dt_outcomes_raw_i_elderly[strategy == "elderly", !c("Age", "year_disc")]
  dt_outcomes_raw_i_wcba = dt_outcomes_raw_i_wcba[strategy == "wcba", !c("Age", "year_disc")]
  
  ### combine children, adults and elderly ("vaccinate everyone")
  dt_outcomes_raw_i_combined = bind_rows(dt_outcomes_raw_i_adults%>%dplyr::select(-c(strategy)), 
                                         dt_outcomes_raw_i_children%>%dplyr::select(-c(strategy)),
                                         dt_outcomes_raw_i_elderly%>%dplyr::select(-c(strategy)))
  
  grouping_columns <- c("GID_0", "Country", "GID_1", "Sex", "Year", "ag", "n_draw", "VE_scenario", "VE_mild", "VE_severe")
  
  dt_outcomes_raw_i_combined <- dt_outcomes_raw_i_combined[, lapply(.SD, sum), by = grouping_columns]
  
  #################################################################
  ### FINAL DATA: BASELINE + ALL VACC STRATEGIES FOR DISTRICT i ###
  #################################################################
  
  dt_outcomes_raw_i_allStrategies = bind_rows(dt_outcomes_raw_i_baseline,
                                              dt_outcomes_raw_i_adults,
                                              dt_outcomes_raw_i_children,
                                              dt_outcomes_raw_i_elderly,
                                              dt_outcomes_raw_i_wcba,
                                              dt_outcomes_raw_i_combined[, strategy := "combined"])
  
  ### clear up RAM by removing these individually
  rm(dt_outcomes_raw_i_baseline, dt_outcomes_raw_i_adults, 
     dt_outcomes_raw_i_children, dt_outcomes_raw_i_elderly, 
     dt_outcomes_raw_i_wcba, dt_outcomes_raw_i_combined)
  
  
  #################################
  ### FINAL DATA: GROUP AND SUM ###
  #################################
  
  ### FINAL DATA: CUMULATIVE ###
  
  ### combine years (cumulative by age and sex)
  grouping_columns_noYear <- c("strategy", grouping_columns[!grouping_columns == "Year"])
  dt_outcomes_raw_i_allStrategies_noYear <- dt_outcomes_raw_i_allStrategies[, !c("Year")]
  dt_outcomes_raw_i_allStrategies_noYear <- dt_outcomes_raw_i_allStrategies_noYear[, lapply(.SD, sum), by = grouping_columns_noYear]
  
  ### combine years and sex (cumulative by age)
  grouping_columns_noYear_noSex <- c("strategy", grouping_columns[!grouping_columns %in% c("Year", "Sex")])
  dt_outcomes_raw_i_allStrategies_noYear_noSex <- dt_outcomes_raw_i_allStrategies[, !c("Year", "Sex")]
  dt_outcomes_raw_i_allStrategies_noYear_noSex <- dt_outcomes_raw_i_allStrategies_noYear_noSex[, lapply(.SD, sum), by = grouping_columns_noYear_noSex]
  
  ### combine years, sex and ag (full cumulative)
  grouping_columns_noYear_noSex_noAg <- c("strategy", grouping_columns[!grouping_columns %in% c("Year", "Sex", "ag")])
  dt_outcomes_raw_i_allStrategies_noYear_noSex_noAg <- dt_outcomes_raw_i_allStrategies[, !c("Year", "Sex", "ag")]
  dt_outcomes_raw_i_allStrategies_noYear_noSex_noAg <- dt_outcomes_raw_i_allStrategies_noYear_noSex_noAg[, lapply(.SD, sum), by = grouping_columns_noYear_noSex_noAg]
  
  ### FINAL DATA: ANNUAL ###
  
  ### combine sex and ag (annual total)
  grouping_columns_noSex_noAg <- c("strategy", grouping_columns[!grouping_columns %in% c("Sex", "ag")])
  dt_outcomes_raw_i_allStrategies_noSex_noAg <- dt_outcomes_raw_i_allStrategies[, !c("Sex", "ag")]
  dt_outcomes_raw_i_allStrategies_noSex_noAg <- dt_outcomes_raw_i_allStrategies_noSex_noAg[, lapply(.SD, sum), by = grouping_columns_noSex_noAg]
  
  #########################################
  ### SAVE FINAL DATA FOR EACH DISTRICT ###
  #########################################
  
  if(aggrDistr == T){
    
    print(paste0("Saving organised health-econ outcome data for ", GID_1_i))
    
    save(dt_outcomes_raw_i_allStrategies_noYear, file = paste0("dt_", sim_scen, "_outcomes_cumul_age_sex_", GID_1_i, ".Rdata"))
    save(dt_outcomes_raw_i_allStrategies_noYear_noSex, file = paste0("dt_", sim_scen, "_outcomes_cumul_age_", GID_1_i, ".Rdata"))
    save(dt_outcomes_raw_i_allStrategies_noYear_noSex_noAg, file = paste0("dt_", sim_scen, "_outcomes_cumul_", GID_1_i, ".Rdata"))
    save(dt_outcomes_raw_i_allStrategies_noSex_noAg, file = paste0("dt_", sim_scen, "_outcomes_annual_", GID_1_i, ".Rdata"))
  }

  
  #############################
  ### GROUP DATA INTO LISTS ###
  #############################
  
  ### ALL DISTRICTS ###
  
  ### cumulative outcomes (combined 2025 to 2037)
  list_outcomes_allDistricts_cumul_age_sex[[which(GID_1_i == GID_1_final)]] = dt_outcomes_raw_i_allStrategies_noYear
  list_outcomes_allDistricts_cumul_age[[which(GID_1_i == GID_1_final)]] = dt_outcomes_raw_i_allStrategies_noYear_noSex
  list_outcomes_allDistricts_cumul[[which(GID_1_i == GID_1_final)]] = dt_outcomes_raw_i_allStrategies_noYear_noSex_noAg
  
  ### annual outcomes -- only save totals due to large memory
  list_outcomes_allDistricts_annual[[which(GID_1_i == GID_1_final)]] = dt_outcomes_raw_i_allStrategies_noSex_noAg
  
}

#######################################################################
### BIND, GROUP AND SUM OUTCOME DATA ACCORDING TO DISTRICT GROUPING ###
#######################################################################

### BIND LISTS ###

### ALL DISTRICTS
dt_outcomes_allDistricts_cumul_age_sex = rbindlist(list_outcomes_allDistricts_cumul_age_sex)
dt_outcomes_allDistricts_cumul_age = rbindlist(list_outcomes_allDistricts_cumul_age)
dt_outcomes_allDistricts_cumul = rbindlist(list_outcomes_allDistricts_cumul)
dt_outcomes_allDistricts_annual = rbindlist(list_outcomes_allDistricts_annual)

### FILTER OUT MEDIUM AND HIGH BURDEN DISTRICTS
dt_outcomes_medDistricts_cumul_age_sex = dt_outcomes_allDistricts_cumul_age_sex[GID_1 %in% vec_GID_1_medDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_medDistricts_cumul_age = dt_outcomes_allDistricts_cumul_age[GID_1 %in% vec_GID_1_medDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_medDistricts_cumul = dt_outcomes_allDistricts_cumul[GID_1 %in% vec_GID_1_medDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_medDistricts_annual = dt_outcomes_allDistricts_annual[GID_1 %in% vec_GID_1_medDistricts, !c("GID_1", "GID_0", "Country")]

### FILTER OUT HIGH BURDEN DISTRICTS ONLY
dt_outcomes_highDistricts_cumul_age_sex = dt_outcomes_allDistricts_cumul_age_sex[GID_1 %in% vec_GID_1_highDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_highDistricts_cumul_age = dt_outcomes_allDistricts_cumul_age[GID_1 %in% vec_GID_1_highDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_highDistricts_cumul = dt_outcomes_allDistricts_cumul[GID_1 %in% vec_GID_1_highDistricts, !c("GID_1", "GID_0", "Country")]
dt_outcomes_highDistricts_annual = dt_outcomes_allDistricts_annual[GID_1 %in% vec_GID_1_highDistricts, !c("GID_1", "GID_0", "Country")]

### REMOVE DISTRICT FROM "ALL DISTRICTS"
dt_outcomes_allDistricts_cumul_age_sex = dt_outcomes_allDistricts_cumul_age_sex[, !c("GID_1", "GID_0", "Country")]
dt_outcomes_allDistricts_cumul_age = dt_outcomes_allDistricts_cumul_age[, !c("GID_1", "GID_0", "Country")]
dt_outcomes_allDistricts_cumul = dt_outcomes_allDistricts_cumul[, !c("GID_1", "GID_0", "Country")]
dt_outcomes_allDistricts_annual = dt_outcomes_allDistricts_annual[, !c("GID_1", "GID_0", "Country")]



### GROUP AND SUM INCLUDED DISTRICTS BY DIFFERENT LEVELS OF DATA GROUPING ###

### CUMULATIVE BY AGE AND SEX
grouping_columns_cumul_age_sex <- c("strategy", grouping_columns[!grouping_columns %in% c("GID_1", "GID_0", "Country", "Year")])
dt_outcomes_allDistricts_cumul_age_sex <- dt_outcomes_allDistricts_cumul_age_sex[, lapply(.SD, sum), by = grouping_columns_cumul_age_sex]
dt_outcomes_medDistricts_cumul_age_sex <- dt_outcomes_medDistricts_cumul_age_sex[, lapply(.SD, sum), by = grouping_columns_cumul_age_sex]
dt_outcomes_highDistricts_cumul_age_sex <- dt_outcomes_highDistricts_cumul_age_sex[, lapply(.SD, sum), by = grouping_columns_cumul_age_sex]

### CUMULATIVE BY AGE
grouping_columns_cumul_age <- c("strategy", grouping_columns[!grouping_columns %in% c("GID_1", "GID_0", "Country", "Year", "Sex")])
dt_outcomes_allDistricts_cumul_age <- dt_outcomes_allDistricts_cumul_age[, lapply(.SD, sum), by = grouping_columns_cumul_age]
dt_outcomes_medDistricts_cumul_age <- dt_outcomes_medDistricts_cumul_age[, lapply(.SD, sum), by = grouping_columns_cumul_age]
dt_outcomes_highDistricts_cumul_age <- dt_outcomes_highDistricts_cumul_age[, lapply(.SD, sum), by = grouping_columns_cumul_age]

### CUMULATIVE ALL
grouping_columns_cumul <- c("strategy", grouping_columns[!grouping_columns %in% c("GID_1", "GID_0", "Country", "Year", "Sex", "ag")])
dt_outcomes_allDistricts_cumul <- dt_outcomes_allDistricts_cumul[, lapply(.SD, sum), by = grouping_columns_cumul]
dt_outcomes_medDistricts_cumul <- dt_outcomes_medDistricts_cumul[, lapply(.SD, sum), by = grouping_columns_cumul]
dt_outcomes_highDistricts_cumul <- dt_outcomes_highDistricts_cumul[, lapply(.SD, sum), by = grouping_columns_cumul]

### ANNUAL ALL
grouping_columns_annual <- c("strategy", grouping_columns[!grouping_columns %in% c("GID_1", "GID_0", "Country", "Sex", "ag")])
dt_outcomes_allDistricts_annual <- dt_outcomes_allDistricts_annual[, lapply(.SD, sum), by = grouping_columns_annual]
dt_outcomes_medDistricts_annual <- dt_outcomes_medDistricts_annual[, lapply(.SD, sum), by = grouping_columns_annual]
dt_outcomes_highDistricts_annual <- dt_outcomes_highDistricts_annual[, lapply(.SD, sum), by = grouping_columns_annual]


#########################
### SAVE GROUPED DATA ###
#########################

### CUMULATIVE BY AGE AND SEX
save(dt_outcomes_allDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_allDistricts_cumul_age_sex.Rdata"))
save(dt_outcomes_medDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_medDistricts_cumul_age_sex.Rdata"))
save(dt_outcomes_highDistricts_cumul_age_sex, file = paste0("dt_", sim_scen, "_outcomes_highDistricts_cumul_age_sex.Rdata"))

### CUMULATIVE BY AGE
save(dt_outcomes_allDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_allDistricts_cumul_age.Rdata"))
save(dt_outcomes_medDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_medDistricts_cumul_age.Rdata"))
save(dt_outcomes_highDistricts_cumul_age, file = paste0("dt_", sim_scen, "_outcomes_highDistricts_cumul_age.Rdata"))

### CUMULATIVE ALL
save(dt_outcomes_allDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_allDistricts_cumul.Rdata"))
save(dt_outcomes_medDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_medDistricts_cumul.Rdata"))
save(dt_outcomes_highDistricts_cumul, file = paste0("dt_", sim_scen, "_outcomes_highDistricts_cumul.Rdata"))

### ANNUAL ALL
save(dt_outcomes_allDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_allDistricts_annual.Rdata"))
save(dt_outcomes_medDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_medDistricts_annual.Rdata"))
save(dt_outcomes_highDistricts_annual, file = paste0("dt_", sim_scen, "_outcomes_highDistricts_annual.Rdata"))


