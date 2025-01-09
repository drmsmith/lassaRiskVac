####################
### SET FILEPATH ###
####################

filepath = this.path::here()
setwd(filepath)

########################
### RUN HOUSEKEEPING ###
########################

source("housekeeping.R")

save_tables = F


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

files_outcomes_grouped = list.files(paste0("outcomes/summarised_data/", sim_scen, "/"))
for(file_i in files_outcomes_grouped){load(paste0("outcomes/summarised_data/", sim_scen, "/", file_i))}


#######################################################
### FUNCTION TO TIDY UP SCALING OF NUMERIC OUTCOMES ### 
#######################################################

comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste0(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
         c(""," K"," M"," B"," T")[div] )}




##############
##############
##############
### TABLES ###
##############
##############
##############

################################
### BASELINE HEALTH OUTCOMES ###
################################


cols_health_outcomes = c("N_infection", "N_cases", "N_hospital", "N_snhl", "N_death_total_noFL", "N_fl", "DALY_total_noFL")
cols_health_outcomes_labels = c("Infections", "Cases", "Hospitalisations", "Hearing loss", "Deaths", "Foetal losses", "DALYs")

##########################################################################
### WHAT SHARE OF HOSPITALISED WOMEN ARE PREGNANT UPON HOSPITALISATION ###
##########################################################################

check_hosp_preg = dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex%>%
  filter(strategy == "baseline", outcome == "N_hospital")%>%
  filter(Sex == "Female_Preg",
         ag %in% c("15-24", "25-34", "35-49"))

check_hosp_all_women = dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex%>%
  filter(strategy == "baseline", outcome == "N_hospital")%>%
  filter(Sex %in% c("Female_Preg", "Female_NotPreg"),
         ag %in% c("15-24", "25-34", "35-49"))

sum(check_hosp_preg$mean)/sum(check_hosp_all_women$mean)




#####################################################
### BASELINE HEALTH OUTCOMES ACROSS ALL DISTRICTS ###
#####################################################

### Totals by age
t_outcomes_baseline_totals_ag = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, ag, label))%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  dplyr::select(all_of(c("ag", cols_health_outcomes_labels)))%>%
  arrange(ag)%>%
  rename("Age group" = "ag")

### Totals for all ages
t_outcomes_baseline_totals = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         ag = "All",
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, ag, label))%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  dplyr::select(all_of(c("ag", cols_health_outcomes_labels)))%>%
  arrange(ag)%>%
  rename("Age group" = "ag")

### Rates by age
t_outcomes_baseline_rates_ag = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, ag, label))%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  dplyr::select(all_of(c("ag", cols_health_outcomes_labels)))%>%
  arrange(ag)%>%
  rename("Age group" = "ag")

### Rates for all ages
t_outcomes_baseline_rates = dt_outcomes_summarisedRates_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         ag = "All",
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, ag, label))%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  dplyr::select(all_of(c("ag", cols_health_outcomes_labels)))%>%
  arrange(ag)%>%
  rename("Age group" = "ag")


### COMBINE
t_outcomes_baseline_ag = bind_rows(t_outcomes_baseline_totals_ag,
                                t_outcomes_baseline_totals,
                                t_outcomes_baseline_rates_ag,
                                t_outcomes_baseline_rates)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_outcomes_baseline_ag = read_docx()%>%
  body_add_flextable(t_outcomes_baseline_ag)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_outcomes_baseline_ag, target = paste0("t_", sim_scen, "_outcomes_baseline_ag.docx"))
}

### Check rates for DALYs with FL
dt_outcomes_summarisedRates_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% "DALY_total")%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )

#####################################################
### BASELINE HEALTH OUTCOMES BY COUNTRY GROUPINGS ###
#####################################################

### TOTALS BY DISTRICT GROUPINGS
t_outcomes_baseline_totals_byGroup = bind_rows(
  dt_outcomes_summarisedTotals_highDistricts_cumul%>%mutate(grouping = "High burden districts"),
  dt_outcomes_summarisedTotals_medDistricts_cumul%>%mutate(grouping = "Medium & high burden districts"),
  dt_outcomes_summarisedTotals_allDistricts_cumul%>%mutate(grouping = "All districts"),
  )%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, grouping, label))%>%
  pivot_wider(names_from = grouping, values_from = label)%>%
  arrange(outcome)

### RATES BY DISTRICT GROUPINGS
t_outcomes_baseline_rates_byGroup = bind_rows(
  dt_outcomes_summarisedRates_highDistricts_cumul%>%mutate(grouping = "High burden districts"),
  dt_outcomes_summarisedRates_medDistricts_cumul%>%mutate(grouping = "Medium & high burden districts"),
  dt_outcomes_summarisedRates_allDistricts_cumul%>%mutate(grouping = "All districts"),
)%>%
  filter(strategy == "baseline",
         outcome %in% cols_health_outcomes)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes,
                          labels = cols_health_outcomes_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, grouping, label))%>%
  pivot_wider(names_from = grouping, values_from = label)%>%
  arrange(outcome)


### COMBINED TOTALS AND RATES BY DISTRICT GROUPING
t_outcomes_baseline_byGroup = bind_rows(t_outcomes_baseline_totals_byGroup,
                                t_outcomes_baseline_rates_byGroup)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_outcomes_baseline_byGroup = read_docx()%>%
  body_add_flextable(t_outcomes_baseline_byGroup)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_outcomes_baseline_byGroup, target = paste0("t_", sim_scen, "_outcomes_baseline_byGroup.docx"))
}


######################
### BASELINE COSTS ###
######################

### Disaggregated costs, discounted (exploring where coming from)
cols_costs_disaggr_disc = c("Cost_outpatient_oop_disc", "Cost_outpatient_gvt_disc", "Cost_hosp_oop_disc", "Cost_hosp_gvt_disc", 
                            "Cost_care_total_disc",
                            "Cost_prod_fever_disc", "Cost_prod_hospital_disc", "Cost_prod_snhl_disc", "Cost_prod_death_disc",
                            "Cost_prod_total_disc", 
                            "Cost_DALY_fever_disc", "Cost_DALY_hospital_disc", "Cost_DALY_snhl_disc", "Cost_DALY_death_total_noFL_disc",
                            "Cost_DALY_total_noFL_disc")

### Disaggregated costs, not discounted
cols_costs_disaggr = c("Cost_outpatient_oop", "Cost_outpatient_gvt", "Cost_hosp_oop", "Cost_hosp_gvt", 
                            "Cost_care_total",
                            "Cost_prod_fever", "Cost_prod_hospital", "Cost_prod_snhl", "Cost_prod_death",
                            "Cost_prod_total", 
                            "Cost_DALY_fever", "Cost_DALY_hospital", "Cost_DALY_snhl", "Cost_DALY_death_total_noFL",
                            "Cost_DALY_total_noFL")

cols_costs_disaggr_labels = c("Outpatient (out-of-pocket)", "Outpatient (reimbursed)", "Inpatient (out-of-pocket)", "Inpatient (reimbursed)", 
                              "Total healthcare costs",
                              "PL: Mild/moderate disease", "PL: Severe disease", "PL: Hearing loss", "PL: Death",
                              "Total productivity losses", 
                              "MD: Mild/moderate disease", "MD: Severe disease", "MD: Hearing loss", "MD: Death",
                              "Total monetised DALYs")

### Leading economic outcomes (including catastrophic and impoverishing)
cols_costs_main = c("Cost_care_total_disc",
                    "N_catastrophic", "N_impoverished",
                    "Cost_prod_total_disc", 
                    "Cost_DALY_total_noFL_disc",
                    "Cost_VSL", "Cost_VSLY")

cols_costs_main_labels = c("Total healthcare costs",
                              "Catastrophic healthcare expenditure", "Impoverishing healthcare expenditure",
                              "Total productivity losses", 
                              "Total monetised DALYs",
                              "Value of statistical life", 
                              "Value of statistical life-years")


############################
### TOTAL SOCIETAL COSTS ###
############################

dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% "Cost_societal_MDALY_noFL_disc")%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )

dt_outcomes_summarisedRates_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% "Cost_societal_MDALY_noFL_disc")%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )

###########################################
### BASELINE COSTS DISAGGREGATING PARTS ###
###########################################

### Discounted
t_costs_baseline_disaggr_disc = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% cols_costs_disaggr_disc)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_disaggr_disc,
                          labels = cols_costs_disaggr_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, label_disc))%>%
  arrange(outcome)

### Undiscounted
t_costs_baseline_disaggr_notDisc = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy == "baseline",
         outcome %in% cols_costs_disaggr)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_notDisc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_disaggr,
                          labels = cols_costs_disaggr_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, label_notDisc))%>%
  arrange(outcome)

### Combined
t_costs_baseline_disaggr = left_join(t_costs_baseline_disaggr_disc, t_costs_baseline_disaggr_notDisc,
                                     by = "outcome")%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costs_baseline_disaggr = read_docx()%>%
  body_add_flextable(t_costs_baseline_disaggr)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costs_baseline_disaggr, target = paste0("t_", sim_scen, "_costs_baseline_disaggr.docx"))
}


###########################################
### BASELINE COSTS BY COUNTRY GROUPINGS ###
###########################################

### TOTALS BY DISTRICT GROUPINGS
t_costs_baseline_totals_byGroup = bind_rows(
  dt_outcomes_summarisedTotals_highDistricts_cumul%>%mutate(grouping = "High burden districts"),
  dt_outcomes_summarisedTotals_medDistricts_cumul%>%mutate(grouping = "Medium & high burden districts"),
  dt_outcomes_summarisedTotals_allDistricts_cumul%>%mutate(grouping = "All districts"),
)%>%
  filter(strategy == "baseline",
         outcome %in% cols_costs_main)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_main,
                          labels = cols_costs_main_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, grouping, label))%>%
  pivot_wider(names_from = grouping, values_from = label)%>%
  arrange(outcome)

### RATES BY DISTRICT GROUPINGS
t_costs_baseline_rates_byGroup = bind_rows(
  dt_outcomes_summarisedRates_highDistricts_cumul%>%mutate(grouping = "High burden districts"),
  dt_outcomes_summarisedRates_medDistricts_cumul%>%mutate(grouping = "Medium & high burden districts"),
  dt_outcomes_summarisedRates_allDistricts_cumul%>%mutate(grouping = "All districts"),
)%>%
  filter(strategy == "baseline",
         outcome %in% cols_costs_main)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_main,
                          labels = cols_costs_main_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, grouping, label))%>%
  pivot_wider(names_from = grouping, values_from = label)%>%
  arrange(outcome)

### COMBINED TOTALS AND RATES BY DISTRICT GROUPING
t_costs_baseline_byGroup = bind_rows(t_costs_baseline_totals_byGroup,
                                        t_costs_baseline_rates_byGroup)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costs_baseline_byGroup = read_docx()%>%
  body_add_flextable(t_costs_baseline_byGroup)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costs_baseline_byGroup, target = paste0("t_", sim_scen, "_costs_baseline_byGroup.docx"))
}



##############################################
##############################################
### MAIN HEALTH BENEFIT OF VACCINE RESULTS ###
##############################################
##############################################

t_outcomes_averted_by_strategy = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_health_outcomes[2:7])%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes[2:7],
                          labels = cols_health_outcomes_labels[2:7]))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_outcomes_averted_by_strategy = read_docx()%>%
  body_add_flextable(t_outcomes_averted_by_strategy)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_outcomes_averted_by_strategy, target = paste0("t_", sim_scen, "_outcomes_averted_by_strategy.docx"))
}


############################################
############################################
### MAIN ECON BENEFIT OF VACCINE RESULTS ###
############################################
############################################

###########################
### MAIN COST GROUPINGS ###
###########################

cols_costs_major = c("Cost_care_total_disc",
                    "Cost_prod_total_disc", 
                    "Cost_DALY_total_noFL_disc",
                    "Cost_VSLY")

cols_costs_major_labels = c("Total healthcare costs",
                           "Total productivity losses", 
                           "Total monetised DALYs",
                           "Value of statistical life-years")

cols_costs_care = c("Cost_outpatient_gvt_disc",
                     "Cost_outpatient_oop_disc", 
                     "Cost_hosp_gvt_disc",
                     "Cost_hosp_oop_disc")

cols_costs_care_labels = c("Outpatient healthcare costs (reimbursed)",
                            "Outpatient healthcare costs (out-of-pocket)", 
                            "Inpatient healthcare costs (reimbursed)",
                            "Inpatient healthcare costs (out-of-pocket)")

cols_costs_prod = c("Cost_prod_fever_disc",
                    "Cost_prod_hospital_disc", 
                    "Cost_prod_snhl_disc",
                    "Cost_prod_death_disc")

cols_costs_prod_labels = c("Productivity lost due to mild and moderate disease",
                           "Productivity lost due to severe disease", 
                           "Productivity lost due to hearing loss",
                           "Productivity lost due to death")


##########################################
### ABSOLUTE VACCINE BENEFIT: EFFICACY ###
##########################################

#####################################################################
### TOTAL COSTS AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
#####################################################################

t_costs_averted_by_strategy_disc = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_major)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_major,
                          labels = cols_costs_major_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
    pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costs_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costs_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costs_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costs_averted_by_strategy_disc.docx"))
}

##########################################################################
### HEALTHCARE COSTS AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
##########################################################################

t_costsCare_averted_by_strategy_disc = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_care)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_care,
                          labels = cols_costs_care_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costsCare_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costsCare_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costsCare_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costsCare_averted_by_strategy_disc.docx"))
}


#############################################################################
### PRODUCTIVITY LOSSES AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
#############################################################################

t_costsProd_averted_by_strategy_disc = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_prod)%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costsProd_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costsProd_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costsProd_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costsProd_averted_by_strategy_disc.docx"))
}


############################################
### RELATIVE VACCINE BENEFIT: EFFICIENCY ###
############################################

##################################################################################
### HEALTH BENEFITS PER DOSE AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
##################################################################################

t_outcomesPerDose_averted_by_strategy = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_health_outcomes[c(2, 3, 5, 7)])%>%
  mutate(mean_signif = signif(mean*100000, 3),
         q025_signif = signif(q025*100000, 3),
         q975_signif = signif(q975*100000, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_health_outcomes[c(2, 3, 5, 7)],
                          labels = cols_health_outcomes_labels[c(2, 3, 5, 7)]))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_outcomesPerDose_averted_by_strategy = read_docx()%>%
  body_add_flextable(t_outcomesPerDose_averted_by_strategy)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_outcomesPerDose_averted_by_strategy, target = paste0("t_", sim_scen, "_outcomesPerDose_averted_by_strategy.docx"))
}

########################################################################
### COSTS PER DOSE AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
########################################################################

t_costsPerDose_averted_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_major)%>%
  mutate(mean_signif = signif(mean*100000, 3),
         q025_signif = signif(q025*100000, 3),
         q975_signif = signif(q975*100000, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_major,
                          labels = cols_costs_major_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costsPerDose_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costsPerDose_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costsPerDose_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costsPerDose_averted_by_strategy_disc.docx"))
}


###################################################################################
### HEALTHCARE COSTS PER DOSE AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
###################################################################################

t_costsCarePerDose_averted_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_care)%>%
  mutate(mean_signif = signif(mean*100000, 3),
         q025_signif = signif(q025*100000, 3),
         q975_signif = signif(q975*100000, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_care,
                          labels = cols_costs_care_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costsCarePerDose_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costsCarePerDose_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costsCarePerDose_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costsCarePerDose_averted_by_strategy_disc.docx"))
}


######################################################################################
### PRODUCTIVITY LOSSES PER DOSE AVERTED DUE TO VACCINATION BY TARGET GROUP AND VE ###
######################################################################################

t_costsProdPerDose_averted_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% cols_costs_prod)%>%
  mutate(mean_signif = signif(mean*100000, 3),
         q025_signif = signif(q025*100000, 3),
         q975_signif = signif(q975*100000, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_costsProdPerDose_averted_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_costsProdPerDose_averted_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_costsProdPerDose_averted_by_strategy_disc, target = paste0("t_", sim_scen, "_costsProdPerDose_averted_by_strategy_disc.docx"))
}


###############################
###############################
### THRESHOLD VACCINE COSTS ###
###############################
###############################

#################################################
### THRESHOLD VACCINE COSTS: HEALTHCARE COSTS ###
#################################################

t_TVC_careCosts_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_care_total_disc")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_careCosts_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_careCosts_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_careCosts_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_careCosts_by_strategy_disc.docx"))
}


################################################
### THRSEHOLD VACCINE COSTS: MONETISED DALYS ###
################################################

t_TVC_mDALYs_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_DALY_total_noFL_disc")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_mDALYs_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_mDALYs_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_mDALYs_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_mDALYs_by_strategy_disc.docx"))
}


####################################################
### THRESHOLD VACCINE COSTS: PRODUCTIVITY LOSSES ###
####################################################

t_TVC_prodLosses_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_prod_total_disc")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_prodLosses_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_prodLosses_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_prodLosses_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_prodLosses_by_strategy_disc.docx"))
}


#####################################
### THRESHOLD VACCINE COSTS: VSLY ###
#####################################

t_TVC_VSLY_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_VSLY")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_VSLY_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_VSLY_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_VSLY_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_VSLY_by_strategy_disc.docx"))
}


###############################################################
### THRESHOLD VACCINE COSTS: SOCIAL COSTS + MONETISED DALYS ###
###############################################################

t_TVC_socCosts_mDALY_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_societal_MDALY_noFL_disc")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_socCosts_mDALY_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_socCosts_mDALY_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_socCosts_mDALY_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_socCosts_mDALY_by_strategy_disc.docx"))
}



#############################################################################
### THRESHOLD VACCINE COSTS: SOCIAL COSTS + MONETISED DALYS, UNDISCOUNTED ###
#############################################################################

t_TVC_socCosts_mDALY_by_strategy_undisc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_societal_MDALY_noFL")%>%
  mutate(mean_signif = signif(mean, 3),
         q025_signif = signif(q025, 3),
         q975_signif = signif(q975, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_socCosts_mDALY_by_strategy_undisc = read_docx()%>%
  body_add_flextable(t_TVC_socCosts_mDALY_by_strategy_undisc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_socCosts_mDALY_by_strategy_undisc, target = paste0("t_", sim_scen, "_TVC_socCosts_mDALY_by_strategy_undisc.docx"))
}



#####################################################################################
### THRESHOLD VACCINE COSTS: SOCIAL COSTS + MONETISED DALYS INCLUDING FOETAL LOSS ###
#####################################################################################

t_TVC_socCosts_mDALY_FL_by_strategy_disc = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(strategy != "baseline",
         outcome %in% "Cost_societal_MDALY_disc")%>%
  mutate(mean_signif = signif(mean_disc, 3),
         q025_signif = signif(q025_disc, 3),
         q975_signif = signif(q975_disc, 3),
         mean_neat = comprss(mean_signif),
         q025_neat = comprss(q025_signif),
         q975_neat = comprss(q975_signif)
  )%>%
  mutate(label_VE = paste0(VE_mild*100, "% mild\n", VE_severe*100, "% severe"),
         label_disc = paste0(mean_neat, " (", q025_neat, " - ", q975_neat, ")"),
         outcome = factor(outcome, 
                          levels = cols_costs_prod,
                          labels = cols_costs_prod_labels))%>%
  ungroup()%>%
  dplyr::select(c(outcome, strategy, label_VE, label_disc))%>%
  mutate(strategy = factor(strategy, levels = c(strategy_levels, "combined"), labels = c(strategy_labels, "All (2+)")))%>%
  arrange(outcome, strategy)%>%
  pivot_wider(., id_cols = c(outcome, strategy), names_from = label_VE, values_from = label_disc)%>%
  flextable()%>%
  fontsize(size = 6)%>%
  theme_vanilla()%>%
  autofit()

doc_TVC_socCosts_mDALY_FL_by_strategy_disc = read_docx()%>%
  body_add_flextable(t_TVC_socCosts_mDALY_FL_by_strategy_disc)%>%
  body_end_section_landscape()

if(save_tables){
  print(doc_TVC_socCosts_mDALY_FL_by_strategy_disc, target = paste0("t_", sim_scen, "_TVC_socCosts_mDALY_FL_by_strategy_disc.docx"))
}


###################################################
### DIAGNOSTIC: TVC RELATIVE TO BURDEN AND DOSE ###
###################################################

### How much cumul VSLY averted per strategy
dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(VE_scenario == 3, outcome == "Cost_VSLY")%>%
  mutate(label = paste0(comprss(mean), " (", comprss(q025), "-", comprss(q975), ")"))%>%
  dplyr::select(c(strategy, label))

### How many doses of vaccine per strategy
dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(VE_scenario == 3, outcome == "Cost_VSLY")%>%
  mutate(label = comprss(doses))%>%
  dplyr::select(c(strategy, label))

### TVC
t_TVC_VSLY_by_strategy_disc
