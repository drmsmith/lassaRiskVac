####################
### SET FILEPATH ###
####################

filepath = this.path::here()
setwd(filepath)

########################
### RUN HOUSEKEEPING ###
########################

source("housekeeping.R")

save_plots = F

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





#############
#############
#############
### PLOTS ###
#############
#############
#############

#######################
#######################
### BASELINE BURDEN ###
#######################
#######################

######################################
### BASELINE BURDEN BY AGE: TOTALS ###
######################################

### Cases
p_cumul_casesTotal_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_cases")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Symptomatic cases\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Hearing loss
p_cumul_snhlTotal_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_snhl")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Incident hearing loss\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Hospitalisations
p_cumul_hospitalTotal_by_age =dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_hospital")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Hospitalisations\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Deaths
p_cumul_deathTotal_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_death_total_noFL")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Deaths\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))



#####################################
### BASELINE BURDEN BY AGE: RATES ###
#####################################

### Cases
p_cumul_casesRate_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_cases")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Symptomatic cases\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Hearing loss
p_cumul_snhlRate_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_snhl")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Incident hearing loss\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Hospital
p_cumul_hospitalRate_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_hospital")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Hospitalisations\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

### Deaths
p_cumul_deathRate_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome == "N_death_total_noFL")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Deaths\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))

##########################################
### BASELINE BURDEN BY AGE: PLOT GRIDS ###
##########################################

### COMBINE TOTALS AND RATES FOR CASES, HOSPITALISATIONS AND DEATHS
p_cumul_outcomes_baseline_by_age = plot_grid(p_cumul_casesTotal_by_age,
                                             p_cumul_casesRate_by_age,
                                             p_cumul_hospitalTotal_by_age,
                                             p_cumul_hospitalRate_by_age,
                                             p_cumul_deathTotal_by_age,
                                             p_cumul_deathRate_by_age,
                                             ncol = 2, nrow = 3, labels = c("A", "", "B", "", "C", ""),
                                             align = "hv",
                                             axis = "tblr")


if(save_plots){
  ggsave(p_cumul_outcomes_baseline_by_age, file = paste0("p_", sim_scen, "_cumul_outcomes_baseline_by_age.png"),
         width = 12, height = 20, units = "cm")
}


##############################################
### BASELINE BURDEN BY AGE AND SEX: TOTALS ###
##############################################

### Share of outcomes by pregnancy status
dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex%>%
  filter(strategy == "baseline", outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         Sex = factor(Sex, levels = sex_preg_levels, labels = sex_preg_labels),
         outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths")))%>%
  group_by(Sex, outcome)%>%
  summarise(mean = sum(mean))

### Cases
p_cumul_outcomesTotal_baseline_by_age_sex = dt_outcomes_summarisedTotals_allDistricts_cumul_age_sex%>%
  filter(strategy == "baseline", outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         Sex = factor(Sex, levels = sex_preg_levels, labels = sex_preg_labels),
         outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = Sex))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(width = 0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(width = 0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Lassa fever burden\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = sex_preg_cols_map)+
  scale_colour_manual("", values = sex_preg_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), scale = "free")


if(save_plots){
  ggsave(p_cumul_outcomesTotal_baseline_by_age_sex, file = paste0("p_", sim_scen, "_cumul_outcomesTotal_baseline_by_age_sex.png"),
         width = 24, height = 8, units = "cm")
}

###############################################
### BASELINE YEARS OF LIFE AFFECTED: TOTALS ###
###############################################

### TOTAL
p_cumul_yearsLostTotal_baseline_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome %in% c("YLS", "DALY_death_total_noFL", "YWL_death", "YWL_snhl"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome,
                          levels = c("YLS", "DALY_death_total_noFL", "YWL_death", "YWL_snhl"),
                          labels = c("YLD", "YLL", "YPPLL (death)", "YPPLL (SNHL)")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Years\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4)

##############################################
### BASELINE YEARS OF LIFE AFFECTED: RATES ###
##############################################

p_cumul_yearsLostRates_baseline_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome %in% c("YLS", "DALY_death_total_noFL", "YWL_death", "YWL_snhl"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome,
                          levels = c("YLS", "DALY_death_total_noFL", "YWL_death", "YWL_snhl"),
                          labels = c("YLD", "YLL", "YPPLL (death)", "YPPLL (SNHL)")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Years\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4)


###################################################
### BASELINE YEARS OF LIFE AFFECTED: PLOT GRIDS ###
###################################################

### COMBINE TOTALS AND RATES FOR DALYS DUE TO SYMPTOMS, HEARING LOSS AND DEATH
p_cumul_yearsLost_baseline_by_age = plot_grid(p_cumul_yearsLostTotal_baseline_by_age,
                                          p_cumul_yearsLostRates_baseline_by_age,
                                          ncol = 1, nrow = 2, labels = c("A", "B"),
                                          align = "hv",
                                          axis = "tblr")


if(save_plots){
  ggsave(p_cumul_yearsLost_baseline_by_age, file = paste0("p_", sim_scen, "_cumul_yearsLost_baseline_by_age.png"),
         width = 18, height = 14, units = "cm")
}

##############################
### BASELINE DALYs: TOTALS ###
##############################

### Acute, SNHL, Death and Total
p_cumul_dalyAttrTotal_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome %in% c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome,
                          levels = c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Acute infection", "Hearing loss", "Death", "Total")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("DALYs\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4)


#############################
### BASELINE DALYs: RATES ###
#############################

### Acute, SNHL, Death and Total
p_cumul_dalyAttrRates_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "baseline", outcome %in% c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome,
                          levels = c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Acute infection", "Hearing loss", "Death", "Total")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("DALYs\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4)


##################################
### BASELINE DALYS: PLOT GRIDS ###
##################################

### COMBINE TOTALS AND RATES FOR DALYS DUE TO SYMPTOMS, HEARING LOSS AND DEATH
p_cumul_dalys_baseline_by_age = plot_grid(p_cumul_dalyAttrTotal_by_age,
                                          p_cumul_dalyAttrRates_by_age,
                                          ncol = 1, nrow = 2, labels = c("A", "B"),
                                          align = "hv",
                                          axis = "tblr")


if(save_plots){
  ggsave(p_cumul_dalys_baseline_by_age, file = paste0("p_", sim_scen, "_cumul_dalys_baseline_by_age.png"),
         width = 18, height = 14, units = "cm")
}

########################################
### BASELINE OUTCOMES: ANNUAL TOTALS ###
########################################

p_annual_outcomes_baseline = dt_outcomes_summarisedTotals_allDistricts_annual%>%
  filter(strategy == "baseline", outcome %in% c("N_infection", "N_cases", "N_snhl", "N_hospital", "N_death_total_noFL",  "N_fl"))%>%
  mutate(outcome = factor(outcome,
                          levels = c("N_infection", "N_cases", "N_snhl", "N_hospital", "N_death_total_noFL", "N_fl"),
                          labels = c("Infection", "Acute symptoms", "Hearing loss", "Hospitalisation", "Death", "Foetal loss")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = q025, ymax = q975, fill = outcome, colour = outcome))+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_point(alpha = 1)+
  geom_line(alpha = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  ylab("Annual Lassa fever outcomes")+xlab("Year")+
  scale_fill_manual("", values = outcome_cols_map)+
  scale_colour_manual("", values = outcome_cols_map)+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  scale_x_continuous(breaks = seq(2025, 2037, by = 2))+
  scale_y_continuous(trans = "log10", 
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)))

#####################################
### BASELINE DALYS: ANNUAL TOTALS ###
#####################################

p_annual_dalys_baseline = dt_outcomes_summarisedTotals_allDistricts_annual%>%
  filter(strategy == "baseline", outcome %in% c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_fl"))%>%
  mutate(outcome = factor(outcome,
                          levels = c("DALY_acute", "DALY_snhl", "DALY_death_total_noFL", "DALY_fl"),
                          labels = c("Acute symptoms", "Hearing loss", "Death", "Foetal loss")))%>%
  ggplot(., aes(x = Year, y = mean, ymin = q025, ymax = q975, fill = outcome, colour = outcome, linetype = outcome))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_point(alpha = 1)+
  geom_line(alpha = 1)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  ylab("Annual DALYs (undiscounted)")+xlab("Year")+
  scale_fill_manual("", values = daly_cols_map)+
  scale_linetype_manual(values = c(1, 1, 1, 2), guide = "none")+
  scale_colour_manual("", values = daly_cols_map)+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  scale_x_continuous(breaks = seq(2025, 2037, by = 2))

#####################################################
### BASELINE ANNUAL OUTCOMES AND DALYS: PLOT GRID ###
#####################################################

### COMBINE TOTALS AND RATES FOR DALYS DUE TO SYMPTOMS, HEARING LOSS AND DEATH
p_annual_outcomes_dalys_baseline = plot_grid(p_annual_outcomes_baseline,
                                            p_annual_dalys_baseline,
                                          ncol = 1, nrow = 2, labels = c("A", "B"),
                                          align = "hv",
                                          axis = "tblr")


if(save_plots){
  ggsave(p_annual_outcomes_dalys_baseline, file = paste0("p_", sim_scen, "_annual_outcomes_dalys_baseline.png"),
         width = 16, height = 16, units = "cm")
}



######################
######################
### VACCINE IMPACT ###
######################
######################

###########################################################################
### CUMULATIVE OUTCOMES BY AGE AVERTED VIA COMBINED VACCINATION: TOTALS ###
###########################################################################

p_cumul_outcomeTotals_vacc_by_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(strategy == "combined",
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Outcomes averted due to\nwhole-population vaccination\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")

##########################################################################
### CUMULATIVE OUTCOMES BY AGE AVERTED VIA COMBINED VACCINATION: RATES ###
##########################################################################

p_cumul_outcomeRates_vacc_by_age = dt_outcomes_summarisedRates_allDistricts_cumul_age%>%
  filter(strategy == "combined",
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Outcomes averted due to\nwhole-population vaccination\n(per 100,000 person-years)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")

##############################################################################
### CUMULATIVE OUTCOMES BY AGE AVERTED VIA COMBINED VACCINATION: PLOT GRID ###
##############################################################################
p_cumul_outcomes_vacc_by_age = plot_grid(p_cumul_outcomeTotals_vacc_by_age,
                                          p_cumul_outcomeRates_vacc_by_age,
                                          ncol = 1, nrow = 2, labels = c("A", "B"),
                                          align = "hv",
                                          axis = "tblr")


if(save_plots){
  ggsave(p_cumul_outcomes_vacc_by_age, file = paste0("p_", sim_scen, "_cumul_outcomes_vacc_by_age.png"),
         width = 18, height = 14, units = "cm")
}


##################################################################################
### CUMULATIVE OUTCOMES AVERTED VIA AGE-TARGETED VACCINATION: TOTALS AND RATES ###
##################################################################################

### DISTRIBUTIONS OF TOTALS AND RATES ARE IDENTICAL BECAUSE NO POPULATION STRATIFICATION IN OUTCOME

p_cumul_outcomeTotals_vacc_by_target = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Total outcomes averted\ndue to vaccination\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")

p_cumul_outcomeRates_vacc_by_target = dt_outcomes_summarisedRates_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Total outcomes averted\ndue to vaccination\n(per 100,000 population)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")

### SAVE TOTALS ###

if(save_plots){
  ggsave(p_cumul_outcomeTotals_vacc_by_target, file = paste0("p_", sim_scen, "_cumul_outcomeTotals_vacc_by_target.png"),
         width = 18, height = 8, units = "cm")
}

### REMOVE X AXIS TEXT

p_cumul_outcomeTotals_vacc_by_target_noText = p_cumul_outcomeTotals_vacc_by_target+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

if(save_plots){
  ggsave(p_cumul_outcomeTotals_vacc_by_target_noText, file = paste0("p_", sim_scen, "_cumul_outcomeTotals_vacc_by_target_noText.png"),
         width = 18, height = 6.5, units = "cm")
  
  ggsave(p_cumul_outcomeTotals_vacc_by_target_noText, file = paste0("p_", sim_scen, "_cumul_outcomeTotals_vacc_by_target_noText.tiff"),
         width = 18, height = 6.5, units = "cm")
}

###############################################################################
### CUMULATIVE OUTCOMES BY AGE AVERTED VIA AGE-TARGETED VACCINATION: TOTALS ###
###############################################################################

### SYMPTOMS
p_cumul_caseTotals_vacc_by_target_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_cases"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Symptomatic\ncases averted\n(cumulative)")+xlab("")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(strategy), ncol = 4)

### HOSPITALISATIONS
p_cumul_hospitalTotals_vacc_by_target_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_hospital"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Hospitalisations\naverted\n(cumulative)")+xlab("")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(strategy), ncol = 4)

### DEATHS
p_cumul_deathTotals_vacc_by_target_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_death_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Deaths\naverted\n(cumulative)")+xlab("")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(strategy), ncol = 4)

### DALYs
p_cumul_dalyTotals_vacc_by_target_age = dt_outcomes_summarisedTotals_allDistricts_cumul_age%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("DALY_total_noFL"))%>%
  mutate(ag = factor(ag, levels = ag_levels),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = ag, y = mean, ymin = q025, ymax = q975, fill = ag))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("DALYs\naverted\n(cumulative)")+xlab("Age group")+
  scale_fill_manual("", values = ag_cols_map)+
  scale_colour_manual("", values = ag_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(strategy), ncol = 4)

##############################################################################
### CUMULATIVE OUTCOMES BY AGE AVERTED VIA TARGETED VACCINATION: PLOT GRID ###
##############################################################################
p_cumul_outcomes_vacc_by_target_age = plot_grid(p_cumul_caseTotals_vacc_by_target_age,
                                         p_cumul_hospitalTotals_vacc_by_target_age,
                                         p_cumul_deathTotals_vacc_by_target_age,
                                         p_cumul_dalyTotals_vacc_by_target_age,
                                         ncol = 1, nrow = 4, labels = c("A", "B", "C", "D"),
                                         align = "hv",
                                         axis = "tblr")


if(save_plots){
  ggsave(p_cumul_outcomes_vacc_by_target_age, file = paste0("p_", sim_scen, "_cumul_outcomes_vacc_by_target_age.png"),
         width = 16, height = 20.5, units = "cm")
}

#######################################################
### ANNUAL OUTCOMES AVERTED BY TARGETED VACCINATION ###
#######################################################

p_annual_outcomes_vacc_by_target = dt_outcomes_summarisedTotals_allDistricts_annual%>%
  filter(!strategy %in% c("combined", "baseline"), 
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
         VE_scenario == 3)%>%
  mutate(outcome = factor(outcome,
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptoms", "Hospitalisation", "Death", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = Year, y = mean, ymin = q025, ymax = q975, fill = strategy, colour = strategy))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_point(alpha = 1)+
  geom_line(alpha = 1)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  ylab("Annual Lassa fever burden averted due to targeted vaccination")+xlab("Year")+
  scale_fill_manual("Vaccination\ntarget group", values = strategy_cols_maps)+
  scale_colour_manual("Vaccination\ntarget group", values = strategy_cols_maps)+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  scale_x_continuous(breaks = seq(2025, 2037, by = 2))+
  facet_wrap(facets = vars(outcome), nrow = 4, scales = "free_y")


if(save_plots){
  ggsave(p_annual_outcomes_vacc_by_target, file = paste0("p_", sim_scen, "_annual_outcomes_vacc_by_target.png"),
         width = 14, height = 16, units = "cm")
}


###############################################################
### ANNUAL DISCOUNTED COSTS AVERTED BY TARGETED VACCINATION ###
###############################################################

p_annual_costs_vacc_by_target = dt_outcomes_summarisedTotals_allDistricts_annual%>%
  filter(!strategy %in% c("combined", "baseline"), 
         outcome %in% c("Cost_DALY_total_noFL_disc", "Cost_care_total_disc", "Cost_prod_total_disc", "Cost_VSLY"),
         VE_scenario == 3)%>%
  mutate(outcome = factor(outcome,
                          levels = c( "Cost_care_total_disc", "Cost_prod_total_disc", "Cost_DALY_total_noFL_disc",  "Cost_VSLY"),
                          labels = c("Healthcare costs", "Productivity losses", "Monetised DALYs",  "Value of statisical life-years")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = Year, y = mean, ymin = q025, ymax = q975, fill = strategy, colour = strategy))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_point(alpha = 1)+
  geom_line(alpha = 1)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")+
  ylab("Annual costs averted due to targeted vaccination (Int'l $ 2023)")+xlab("Year")+
  scale_fill_manual("Vaccination\ntarget group", values = strategy_cols_maps)+
  scale_colour_manual("Vaccination\ntarget group", values = strategy_cols_maps)+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  scale_x_continuous(breaks = seq(2025, 2037, by = 2))+
  facet_wrap(facets = vars(outcome), nrow = 4, scales = "free_y")


if(save_plots){
  ggsave(p_annual_costs_vacc_by_target, file = paste0("p_", sim_scen, "_annual_costs_vacc_by_target.png"),
         width = 14, height = 16, units = "cm")
}

################################################################
### CUMULATIVE GROUPED COSTS AVERTED BY TARGETED VACCINATION ###
################################################################

dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Total outcomes averted\ndue to vaccination\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")


############################################################################
### CUMULATIVE GROUPED COSTS AVERTED BY TARGETED VACCINATION: COMPARE VE ###
############################################################################

p_cumul_costs_vacc_by_target = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_care_total_disc", "Cost_prod_total_disc", "Cost_DALY_total_noFL_disc", "Cost_VSLY"),
         VE_scenario == 3)%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         outcome = factor(outcome,
                          levels = c("Cost_care_total_disc", "Cost_prod_total_disc", "Cost_DALY_total_noFL_disc", "Cost_VSLY"),
                          labels = c("Healthcare costs", "Productivity losses", "Monetised DALYs", "VSLY")))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Total costs averted due to\nvaccination, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")


if(save_plots){
  ggsave(p_cumul_costs_vacc_by_target_VE, file = paste0("p_", sim_scen, "_cumul_costs_vacc_by_target_VE.png"),
         width = 22, height = 12, units = "cm")
}

#############################################################################
### CUMULATIVE SOCIETAL COSTS AVERTED BY TARGETED VACCINATION: COMPARE VE ###
#############################################################################

### HEALTHCARE COSTS ###
p_cumul_costs_care_vacc_by_target_VE = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_outpatient_gvt_disc", "Cost_outpatient_oop_disc", 
                        "Cost_hosp_gvt_disc", "Cost_hosp_oop_disc"))%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels),
         outcome = factor(outcome,
                          levels = c("Cost_outpatient_gvt_disc", "Cost_outpatient_oop_disc", 
                                     "Cost_hosp_gvt_disc", "Cost_hosp_oop_disc"),
                          labels = c("Outpatient healthcare costs\n(reimbursed)", "Outpatient healthcare costs\n(out-of-pocket)",
                                     "Inpatient healthcare costs\n(reimbursed)", "Inpatient healthcare costs\n(out-of-pocket)")))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted due to vaccination, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), nrow = 2)


if(save_plots){
  ggsave(p_cumul_costs_care_vacc_by_target_VE, file = paste0("p_", sim_scen, "_cumul_costs_care_vacc_by_target_VE.png"),
         width = 16, height = 14, units = "cm")
}

### PRODUCTIVITY LOSSES ###
p_cumul_costs_prodLoss_vacc_by_target_VE = dt_outcomes_summarisedTotals_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_prod_fever_disc", "Cost_prod_hospital_disc", 
                        "Cost_prod_snhl_disc", "Cost_prod_death_disc"))%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels),
         outcome = factor(outcome,
                          levels = c("Cost_prod_fever_disc", "Cost_prod_hospital_disc", 
                                     "Cost_prod_snhl_disc", "Cost_prod_death_disc"),
                          labels = c("Productivity lost due to\nmild and moderate disease", "Productivity lost due to\nsevere disease",
                                     "Productivity lost due to\nhearing loss", "Productivity lost due to\ndeath")))%>%
  ggplot(., aes(x = strategy, y = mean, ymin = q025, ymax = q975, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted due to vaccination, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), nrow = 2)



if(save_plots){
  ggsave(p_cumul_costs_prodLoss_vacc_by_target_VE, file = paste0("p_", sim_scen, "_cumul_costs_prodLoss_vacc_by_target_VE.png"),
         width = 16, height = 14, units = "cm")
}

##########################
##########################
### VACCINE EFFICIENCY ###
##########################
##########################

##############################
### GAINS PER VACCINE DOSE ###
##############################

###############################################################################
### CUMULATIVE PER-DOSE HEALTH OUTCOMES AVERTED DUE TO TARGETED VACCINATION ###
###############################################################################

p_cumul_efficiency_outcomes_by_target = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Outcomes averted per\n100,000 vaccine doses\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")


if(save_plots){
  ggsave(p_cumul_efficiency_outcomes_by_target, file = paste0("p_", sim_scen, "_cumul_efficiency_outcomes_by_target.png"),
         width = 18, height = 10, units = "cm")
  
  ggsave(p_cumul_efficiency_outcomes_by_target, file = paste0("p_", sim_scen, "_cumul_efficiency_outcomes_by_target.tiff"),
         width = 18, height = 9, units = "cm")
}


######################################################################################
### CUMULATIVE PER-DOSE HEALTH OUTCOMES AVERTED DUE TO TARGETED VACCINATION, BY VE ###
######################################################################################

p_cumul_efficiency_outcomes_by_target_VE = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_cases", "N_hospital", "N_death_total_noFL", "DALY_total_noFL"),
                          labels = c("Symptomatic cases", "Hospitalisations", "Deaths", "DALYs")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Outcomes averted per 100,000 vaccine doses\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), nrow = 4, scales = "free_y")



if(save_plots){
  ggsave(p_cumul_efficiency_outcomes_by_target_VE, file = paste0("p_", sim_scen, "_cumul_efficiency_outcomes_by_target_VE.png"),
         width = 14, height = 18, units = "cm")
}

#####################################################################################
### FOETAL LOSS SENSITIVITY ANALYSIS: CUMULATIVE PER-DOSE HEALTH OUTCOMES AVERTED ###
#####################################################################################

### Panel A: Deaths
p_cumul_efficiency_deaths_by_target_SA_FL = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("N_death_total_noFL", "N_death_total"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("N_death_total_noFL", "N_death_total"),
                          labels = c("Baseline", "Baseline + foetal loss")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Deaths averted per\n100,000 vaccine doses\n(cumulative)")+xlab("")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 2)



### DALYs
p_cumul_efficiency_DALYs_by_target_SA_FL = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("DALY_total_noFL", "DALY_total"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("DALY_total_noFL", "DALY_total"),
                          labels = c("Baseline", "Baseline + foetal loss")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("DALYs averted per\n100,000 vaccine doses\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 2)


### Combined deaths + DALYs ###

p_cumul_efficiency_deaths_DALYs_by_target_SA_FL = plot_grid(p_cumul_efficiency_deaths_by_target_SA_FL,
                                                            p_cumul_efficiency_DALYs_by_target_SA_FL,
                                                            nrow = 2,
                                                            labels = c("A", "B"))



if(save_plots){
  
  ggsave(p_cumul_efficiency_deaths_DALYs_by_target_SA_FL, file = paste0("p_", sim_scen, "_cumul_efficiency_deaths_DALYs_by_target_SA_FL.png"),
         width = 9, height = 16, units = "cm")
}




##################################################################
### OVERALL VACCINE EFFICIENCY IN TERMS OF MAJOR COSTS AVERTED ###
##################################################################

#####################################################################
### CUMULATIVE PER-DOSE COSTS AVERTED DUE TO TARGETED VACCINATION ###
#####################################################################

p_cumul_efficiency_costs_by_target = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         VE_scenario == 3,
         outcome %in% c("Cost_care_total_disc", "Cost_prod_total_disc", "Cost_DALY_total_noFL_disc", "Cost_VSLY"))%>%
  mutate(outcome = factor(outcome, 
                          levels = c("Cost_care_total_disc", "Cost_prod_total_disc", "Cost_DALY_total_noFL_disc", "Cost_VSLY"),
                          labels = c("Healthcare costs", "Productivity Losses", "Monetised DALYs", "VSLY")),
         strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = strategy))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black")+
  geom_errorbar(width = 0.25)+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted per 100,000\nvaccine doses, Int'l $ 2023\n(cumulative)")+xlab("Vaccine target group")+
  scale_fill_manual("", values = strategy_cols_maps)+
  scale_colour_manual("", values = strategy_cols_maps)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 4, scales = "free_y")


if(save_plots){
  ggsave(p_cumul_efficiency_costs_by_target, file = paste0("p_", sim_scen, "_cumul_efficiency_costs_by_target.png"),
         width = 18, height = 10, units = "cm")
}

################################################################################################
### CUMULATIVE PER-DOSE COSTS AVERTED DUE TO TARGETED VACCINATION, VARYING VE AND COST TYPES ###
################################################################################################

### OVERALL COST GROUPS ###
p_cumul_efficiency_costs_by_target_VE = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_DALY_total_noFL_disc", "Cost_societal_disc", "Cost_VSLY"))%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels),
         outcome = factor(outcome,
                          levels = c("Cost_DALY_total_noFL_disc", "Cost_societal_disc", "Cost_VSLY"),
                          labels = c("Monetised DALYs", "Societal costs", "Value of statistical life-years")))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted per 100,000 vaccine doses, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), ncol = 3)+
  guides(fill = guide_legend(nrow = 2, byrow = T))

if(save_plots){
  ggsave(p_cumul_efficiency_costs_by_target_VE, file = paste0("p_", sim_scen, "_cumul_efficiency_costs_by_target_VE.png"),
         width = 22, height = 14, units = "cm")
}



### HEALTHCARE COSTS ###
p_cumul_efficiency_costs_care_vacc_by_target_VE = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_outpatient_gvt_disc", "Cost_outpatient_oop_disc", 
                        "Cost_hosp_gvt_disc", "Cost_hosp_oop_disc"))%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels),
         outcome = factor(outcome,
                          levels = c("Cost_outpatient_gvt_disc", "Cost_outpatient_oop_disc", 
                                     "Cost_hosp_gvt_disc", "Cost_hosp_oop_disc"),
                          labels = c("Outpatient healthcare costs\n(reimbursed)", "Outpatient healthcare costs\n(out-of-pocket)",
                                     "Inpatient healthcare costs\n(reimbursed)", "Inpatient healthcare costs\n(out-of-pocket)")))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted per 100,000 vaccine doses, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), nrow = 2)


if(save_plots){
  ggsave(p_cumul_efficiency_costs_care_vacc_by_target_VE, file = paste0("p_", sim_scen, "_cumul_efficiency_costs_care_vacc_by_target_VE.png"),
         width = 20, height = 14, units = "cm")
}

### PRODUCTIVITY LOSSES ###

p_cumul_efficiency_prodLoss_vacc_by_target_VE = dt_outcomes_summarisedTotalsPerDose_allDistricts_cumul%>%
  filter(!strategy %in% c("combined", "baseline"),
         outcome %in% c("Cost_prod_fever_disc", "Cost_prod_hospital_disc", 
                        "Cost_prod_snhl_disc", "Cost_prod_death_disc"))%>%
  mutate(strategy = factor(strategy,
                           levels = strategy_levels,
                           labels = strategy_labels),
         VE_scenario = factor(VE_scenario,
                              levels = VE_levels,
                              labels = VE_labels),
         outcome = factor(outcome,
                          levels = c("Cost_prod_fever_disc", "Cost_prod_hospital_disc", 
                                     "Cost_prod_snhl_disc", "Cost_prod_death_disc"),
                          labels = c("Productivity lost due to\nmild and moderate disease", "Productivity lost due to\nsevere disease",
                                     "Productivity lost due to\nhearing loss", "Productivity lost due to\ndeath")))%>%
  ggplot(., aes(x = strategy, y = mean*100000, ymin = q025*100000, ymax = q975*100000, fill = VE_scenario))+
  geom_bar(stat = "identity", alpha = 0.9, colour = "black", position = position_dodge(0.9))+
  geom_errorbar(width = 0.25, position = position_dodge(0.9))+
  theme_bw()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Costs averted per 100,000 vaccine doses, Int'l $ 2023\n(cumulative)")+#
  xlab("Vaccination target group")+
  scale_fill_manual("Vaccine efficacy", values = VE_cols_map)+
  scale_colour_manual("Vaccine efficacy", values = VE_cols_map)+
  geom_hline(yintercept = 0, colour = "grey")+
  guides(fill = guide_legend(override.aes = list(alpha=1)))+
  facet_wrap(facets = vars(outcome), nrow = 2)


if(save_plots){
  ggsave(p_cumul_efficiency_prodLoss_vacc_by_target_VE, file = paste0("p_", sim_scen, "_cumul_efficiency_prodLoss_vacc_by_target_VE.png"),
         width = 20, height = 14, units = "cm")
}

#####################
### FINAL FIGURES ###
#####################

##########################################################################
### COMBINED BASELINE BURDEN PLOT WITH ANNUAL DATA AND CUMULATIVE DATA ###
##########################################################################

p_annual_outcomes_dalys_baseline_finalFig = plot_grid(p_annual_outcomes_baseline+ylab("Total annual Lassa\nfever outcomes"),
                                             p_annual_dalys_baseline+ylab("Total annual DALYs\n(undiscounted)"),
                                             ncol = 1, nrow = 2, labels = c("A", "B"),
                                             align = "hv",
                                             axis = "tblr")

p_cumul_outcomes_baseline_by_age_finalFig = plot_grid(p_cumul_casesTotal_by_age+ylab("Cumulative\nsymptomatic cases"),
                                                      p_cumul_hospitalTotal_by_age+ylab("Cumulative\nhospitalisations"),
                                                      p_cumul_deathTotal_by_age+ylab("Cumulative\ndeaths"),
                                                      p_cumul_casesRate_by_age+ylab("Symptom incidence\n(per 100,000 person-years)"),
                                                      p_cumul_hospitalRate_by_age+ylab("Hospitalisation incidence\n(per 100,000 person-years)"),
                                                      p_cumul_deathRate_by_age+ylab("Death incidence\n(per 100,000 person-years)"),
                                                      ncol = 3, nrow = 2, labels = c("C", "D", "E", "F", "G", "H"),
                                                      align = "hv",
                                                      axis = "tblr")

Fig_BaselineBurden_annualCumul = plot_grid(p_annual_outcomes_dalys_baseline_finalFig,
          p_cumul_outcomes_baseline_by_age_finalFig,
          nrow = 2)

if(save_plots){
  ggsave(Fig_BaselineBurden_annualCumul, file = paste0("Fig_", sim_scen, "_BaselineBurden_annualCumul.png"),
         width = 18, height = 22, units = "cm")
  ggsave(Fig_BaselineBurden_annualCumul, file = paste0("Fig_", sim_scen, "_BaselineBurden_annualCumul.tiff"),
         width = 18, height = 22, units = "cm")
  ggsave(Fig_BaselineBurden_annualCumul, file = paste0("Fig_", sim_scen, "_BaselineBurden_annualCumul.pdf"),
         width = 18, height = 22, units = "cm")
  
}


##########################################################################################
### COMBINED EFFICACY AND EFFICIENY OF TARGETED VACCINATION FOR LASSA FEVER PREVENTION ###
##########################################################################################

p_vaccImpact_byTarget = plot_grid(p_cumul_outcomeTotals_vacc_by_target+theme(axis.text.x = element_blank(),
                                                     axis.title.x = element_blank()), 
          p_cumul_efficiency_outcomes_by_target,
          nrow = 2,
          align = "v",
          axis = "tb",
          rel_heights = c(1,1.4),
          labels = c("A", "B"))
p_vaccImpact_byTarget

if(save_plots){
  ggsave(p_vaccImpact_byTarget, file = paste0("Fig_", sim_scen, "_vaccImpact_byTarget.png"),
         width = 18, height = 14, units = "cm")
  ggsave(p_vaccImpact_byTarget, file = paste0("Fig_", sim_scen, "_vaccImpact_byTarget.tiff"),
         width = 18, height = 14, units = "cm")
  ggsave(p_vaccImpact_byTarget, file = paste0("Fig_", sim_scen, "_vaccImpact_byTarget.pdf"),
         width = 18, height = 14, units = "cm")
  
}

###################################################################################
### COMBINED EFFICACY AND EFFICIENY OF TARGETED VACCINATION FOR ECONONOMIC COSTS###
###################################################################################

p_vaccImpact_Costs_byTarget = plot_grid(p_cumul_costs_vacc_by_target+theme(axis.text.x = element_blank(),
                                                                             axis.title.x = element_blank()), 
                                        p_cumul_efficiency_costs_by_target,
                                  nrow = 2,
                                  align = "v",
                                  axis = "tb",
                                  rel_heights = c(1,1.4),
                                  labels = c("A", "B"))
p_vaccImpact_Costs_byTarget

if(save_plots){
  ggsave(p_vaccImpact_Costs_byTarget, file = paste0("p_", sim_scen, "_vaccImpact_Costs_byTarget.png"),
         width = 18, height = 14, units = "cm")
  
}
