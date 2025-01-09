
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
### LOAD IN MONTE CARLO PARAMS ###
##################################

### Account for probability of symptoms in determining Weekly Lassa fever cases

# df_params_montecarlo
load("parameters_data/params_montecarlo.Rdata")


##############################################################
### LOAD WEEKLY INFECTION VACCINE DATA FOR TARGET DISTRICT ###
##############################################################

### Instead of final 500 simulation data, use a reduced sample of 100 simulations for these plots

GID_1_i = "NGA_12.1"

dt_infections_weekly_vaccinated_adults_i = loadRData("infections/vaccinated_weekly/dt_infections_weekly_vaccinated_NGA.12_1_adults.Rdata")
dt_infections_weekly_vaccinated_adults_i = dt_infections_weekly_vaccinated_adults_i[n_draw <= 100]

dt_infections_weekly_vaccinated_children_i = loadRData("infections/vaccinated_weekly/dt_infections_weekly_vaccinated_NGA.12_1_children.Rdata")
dt_infections_weekly_vaccinated_children_i = dt_infections_weekly_vaccinated_children_i[n_draw <= 100]

dt_infections_weekly_vaccinated_elderly_i = loadRData("infections/vaccinated_weekly/dt_infections_weekly_vaccinated_NGA.12_1_elderly.Rdata")
dt_infections_weekly_vaccinated_elderly_i = dt_infections_weekly_vaccinated_elderly_i[n_draw <= 100]

dt_infections_weekly_vaccinated_wcba_i = loadRData("infections/vaccinated_weekly/dt_infections_weekly_vaccinated_NGA.12_1_wcba.Rdata")
dt_infections_weekly_vaccinated_wcba_i = dt_infections_weekly_vaccinated_wcba_i[n_draw <= 100]

dt_infections_weekly_vaccinated_i = bind_rows(dt_infections_weekly_vaccinated_adults_i,
                                            dt_infections_weekly_vaccinated_children_i,
                                            dt_infections_weekly_vaccinated_elderly_i,
                                            dt_infections_weekly_vaccinated_wcba_i)


#####################
### GENERATE DATA ###
#####################

df_VE = data.frame(VE_symptoms = c(0.7, 0.9))
#VE_symptoms = 0.7

### TOTALS BY STRATEGY ###
### Generate data: weekly vaccine impact, stratify only by strategy
df_infections_weekly_vaccinated_i_total = dt_infections_weekly_vaccinated_i%>%
  as.data.frame()%>%
  group_by(strategy, GID_1, Year, week, n_draw)%>%
  summarise(N_infection_weekly = sum(N_infection_weekly),
            N_infection_vaccinated_weekly = sum(N_infection_vaccinated_weekly))%>%
  left_join(., df_params_montecarlo%>%dplyr::select(n_draw, prob_symptoms), by = "n_draw")%>%
  cross_join(., df_VE)%>%
  mutate(N_lf_weekly = N_infection_weekly * prob_symptoms,
         N_lf_weekly_averted = N_infection_vaccinated_weekly * prob_symptoms * VE_symptoms,
         N_lf_weekly_remaining = N_lf_weekly - N_lf_weekly_averted)

### Generate data: cumulative infections averted, stratify only by strategy
df_infections_weekly_vaccinated_i_total_cumul = df_infections_weekly_vaccinated_i_total%>%
  group_by(VE_symptoms, strategy, GID_1, n_draw)%>%
  mutate(N_lf_weekly_cumsum = cumsum(N_lf_weekly),
         N_lf_weekly_averted_cumsum = cumsum(N_lf_weekly_averted),
         N_lf_weekly_remaining_cumsum = cumsum(N_lf_weekly_remaining))


### TOTALS BY STRATEGY AND AGE ###
### Generate data: weekly vaccine impact, stratify by strategy and age (not sex)
df_infections_weekly_vaccinated_i_ag = dt_infections_weekly_vaccinated_i%>%
  as.data.frame()%>%
  group_by(strategy, GID_1, ag, Year, week, n_draw)%>%
  summarise(N_infection_weekly = sum(N_infection_weekly),
            N_infection_vaccinated_weekly = sum(N_infection_vaccinated_weekly))%>%
  left_join(., df_params_montecarlo%>%dplyr::select(n_draw, prob_symptoms), by = "n_draw")%>%
  cross_join(., df_VE)%>%
  mutate(N_lf_weekly = N_infection_weekly * prob_symptoms,
         N_lf_weekly_averted = N_infection_vaccinated_weekly * prob_symptoms * VE_symptoms,
         N_lf_weekly_remaining = N_lf_weekly - N_lf_weekly_averted)

### Generate data: cumulative infections averted, stratify by strategy and age (not sex)
df_infections_weekly_vaccinated_i_ag_cumul = df_infections_weekly_vaccinated_i_ag%>%
  group_by(VE_symptoms, strategy, GID_1, ag, n_draw)%>%
  mutate(N_lf_weekly_cumsum = cumsum(N_lf_weekly),
         N_lf_weekly_averted_cumsum = cumsum(N_lf_weekly_averted),
         N_lf_weekly_remaining_cumsum = cumsum(N_lf_weekly_remaining))



### TOTALS, COMBINE ALL VACCINATION ###
### Generate data: Separate out no vacc scenario for baseline without vaccine
df_infections_weekly_vaccinated_i_total_noVacc = df_infections_weekly_vaccinated_i_total%>%
  filter(strategy == "adults", VE_symptoms == 0.7)%>%
  group_by(GID_1, Year, week, n_draw)%>%
  summarise(N_lf_weekly = sum(N_lf_weekly))

### Generate data: weekly vaccine impact, combine all vaccination (exclude WCBA as already within adults)
df_infections_weekly_vaccinated_i_total_allVacc = df_infections_weekly_vaccinated_i_total%>%
  filter(strategy != "wcba")%>%
  group_by(VE_symptoms, GID_1, Year, week, n_draw)%>%
  summarise(N_lf_weekly_averted = sum(N_lf_weekly_averted))

### Generate data: baseline infections vs. total infections averted with combined strategies
df_infections_weekly_vaccinated_i_total_beforeAfterVaccine = left_join(df_infections_weekly_vaccinated_i_total_noVacc,
                                                                       df_infections_weekly_vaccinated_i_total_allVacc,
                                                                       by = c("GID_1", "Year", "week", "n_draw"))%>%
  mutate(N_lf_weekly_remaining = N_lf_weekly - N_lf_weekly_averted)



### Generate data: cumulative infections averted, combine all vaccination
df_infections_weekly_vaccinated_i_total_cumul_beforeAfterVaccine = df_infections_weekly_vaccinated_i_total_beforeAfterVaccine%>%
  group_by(VE_symptoms, GID_1, n_draw)%>%
  mutate(N_lf_weekly_cumsum = cumsum(N_lf_weekly),
         N_lf_weekly_averted_cumsum = cumsum(N_lf_weekly_averted),
         N_lf_weekly_remaining_cumsum = cumsum(N_lf_weekly_remaining))

######################
### SUMMARISE DATA ###
######################

### TOTALS BY STRATEGY ###
### Weekly Lassa fever cases and cases averted, totals
df_infections_weekly_vaccinated_i_total_summarised = df_infections_weekly_vaccinated_i_total%>%
  dplyr::select(strategy, VE_symptoms, GID_1, Year, week, n_draw,
                N_lf_weekly, N_lf_weekly_averted, N_lf_weekly_remaining)%>%
  pivot_longer(-c(strategy, VE_symptoms, GID_1, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(strategy, VE_symptoms, GID_1, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))

### Cumulative Lassa fever cases and cases averted, totals
df_infections_weekly_vaccinated_i_total_cumul_summarised = df_infections_weekly_vaccinated_i_total_cumul%>%
  dplyr::select(strategy, VE_symptoms, GID_1, Year, week, n_draw,
                N_lf_weekly_cumsum, N_lf_weekly_averted_cumsum, N_lf_weekly_remaining_cumsum)%>%
  pivot_longer(-c(strategy, VE_symptoms, GID_1, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(strategy, VE_symptoms, GID_1, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))

### Baseline cases vs. Cases with vaccination
df_infections_weekly_vaccinated_i_total_cumul_summarised_beforeAfterVaccine = df_infections_weekly_vaccinated_i_total_cumul_summarised%>%
  filter(strategy == "adults", measure == "N_lf_weekly_cumsum", VE_symptoms == 0.7)%>%
  mutate(strategy = "none",
         VE_symptoms = 0)%>%
  bind_rows(., df_infections_weekly_vaccinated_i_total_cumul_summarised%>%
              filter(measure == "N_lf_weekly_remaining_cumsum")%>%
              mutate(measure = "N_lf_weekly_cumsum"))


### BY AGE ###
### Weekly Lassa fever cases and cases averted, summarised by age
df_infections_weekly_vaccinated_i_ag_summarised = df_infections_weekly_vaccinated_i_ag%>%
  dplyr::select(strategy, VE_symptoms, GID_1, ag, Year, week, n_draw,
                N_lf_weekly, N_lf_weekly_averted, N_lf_weekly_remaining)%>%
  pivot_longer(-c(strategy, VE_symptoms, GID_1, ag, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(strategy, VE_symptoms, GID_1, ag, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))

### Cumulative Lassa fever cases and cases averted, summarised by age
df_infections_weekly_vaccinated_i_ag_cumul_summarised = df_infections_weekly_vaccinated_i_ag_cumul%>%
  dplyr::select(strategy, VE_symptoms, GID_1, ag, Year, week, n_draw,
                N_lf_weekly_cumsum, N_lf_weekly_averted_cumsum, N_lf_weekly_remaining_cumsum)%>%
  pivot_longer(-c(strategy, VE_symptoms, GID_1, ag, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(strategy, VE_symptoms, GID_1, ag, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))


### TOTALS, COMBINE VACCINATION ###
### Weekly Lassa fever cases and cases averted, totals
df_infections_weekly_vaccinated_i_total_summarised_beforeAfterVaccine = df_infections_weekly_vaccinated_i_total_beforeAfterVaccine%>%
  dplyr::select(GID_1, VE_symptoms, Year, week, n_draw,
                N_lf_weekly, N_lf_weekly_averted, N_lf_weekly_remaining)%>%
  pivot_longer(-c(GID_1, VE_symptoms, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(GID_1, VE_symptoms, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))

### Cumulative Lassa fever cases and cases averted, totals
df_infections_weekly_vaccinated_i_total_cumul_summarised_beforeAfterVaccine = df_infections_weekly_vaccinated_i_total_cumul_beforeAfterVaccine%>%
  dplyr::select(VE_symptoms, GID_1, Year, week, n_draw,
                N_lf_weekly_cumsum, N_lf_weekly_averted_cumsum, N_lf_weekly_remaining_cumsum)%>%
  pivot_longer(-c(VE_symptoms, GID_1, Year, week, n_draw), values_to = "value", names_to = "measure")%>%
  group_by(VE_symptoms, GID_1, Year, week, measure)%>%
  summarise(mean = mean(value),
            q025 = quantile(value, 0.025),
            q975 = quantile(value, 0.975))

#############
### PLOTS ###
#############

####################################
### INFECTION PLOTS (NO VACCINE) ###
####################################

### WEEKLY CASES ###

### Weekly Lassa fever cases, total
p_lf_weekly_baseline = df_infections_weekly_vaccinated_i_total_summarised%>%
  filter(measure %in% c("N_lf_weekly"),
         strategy %in% c("children"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975))+
  geom_ribbon(alpha = 0.4, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Symptomatic cases")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_baseline, file = "p_lf_weekly_baseline.png", width = 24, height = 10, units = "cm")



### Weekly Lassa fever cases by age group
p_lf_weekly_baseline_ag = df_infections_weekly_vaccinated_i_ag_summarised%>%
  filter(measure %in% c("N_lf_weekly"),
         strategy %in% c("children"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = ag, fill = ag))+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Symptomatic cases")+xlab("Year")+
  scale_fill_manual(name = "Age group", values = ag_cols_map)+
  scale_colour_manual(name = "Age group", values = ag_cols_map)+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_baseline_ag, file = "p_lf_weekly_baseline_ag.png", width = 24, height = 10, units = "cm")




### CUMULATIVE CASES ###

### Cumulative Lassa fever cases, total
p_lf_weekly_cumul_baseline = df_infections_weekly_vaccinated_i_total_cumul_summarised%>%
  filter(measure %in% c("N_lf_weekly_cumsum"),
         strategy %in% c("children"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975))+
  geom_ribbon(alpha = 0.4, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Cumulative symptomatic cases")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_cumul_baseline, file = "p_lf_weekly_cumul_baseline.png", width = 10, height = 10, units = "cm")


### Cumulative Lassa fever cases by age group
p_lf_weekly_cumul_baseline_ag = df_infections_weekly_vaccinated_i_ag_cumul_summarised%>%
  filter(measure %in% c("N_lf_weekly_cumsum"),
         strategy %in% c("children"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = ag, fill = ag))+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Cumulative symptomatic cases")+xlab("Year")+
  scale_fill_manual(name = "Age group", values = ag_cols_map)+
  scale_colour_manual(name = "Age group", values = ag_cols_map)+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_cumul_baseline_ag, file = "p_lf_weekly_cumul_baseline_ag.png", width = 12, height = 10, units = "cm")



############################
### VACCINE IMPACT PLOTS ###
############################

### CASES AVERTED VACCINATING EVERYONE ###

### Weekly
p_lf_weekly_vaccine_combined = df_infections_weekly_vaccinated_i_total_summarised_beforeAfterVaccine%>%
  filter(measure %in% c("N_lf_weekly", "N_lf_weekly_remaining"),
         VE_symptoms == 0.7)%>%
  mutate(measure_VE = case_when(measure == "N_lf_weekly" ~ measure,
                                measure == "N_lf_weekly_remaining" ~ paste0(measure, "_", VE_symptoms),
                                T ~ "ERROR"))%>%
  mutate(measure_VE = factor(measure_VE, 
                          levels = c("N_lf_weekly", "N_lf_weekly_remaining_0.7", "N_lf_weekly_remaining_0.9"),
                          labels = c("No vaccine", "With vaccine\n(ages 2+, VE=70%)", "With vaccine\n(ages 2+, VE=90%)")))%>%
  mutate(date_proxy = Year + ((week-1)/52))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = measure_VE, fill = measure_VE))+
  geom_hline(yintercept = 0)+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Symptmomatic cases")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  scale_colour_discrete("")+
  scale_fill_discrete("")+
  geom_hline(yintercept = 0, colour = "grey")+
  theme(legend.position = "bottom")

ggsave(p_lf_weekly_vaccine_combined, file = "p_lf_weekly_vaccine_combined.png", width = 24, height = 10, units = "cm")


### Cumulative
p_lf_weekly_vaccine_cumul_combined = df_infections_weekly_vaccinated_i_total_cumul_summarised_beforeAfterVaccine%>%
  filter(measure %in% c("N_lf_weekly_cumsum", "N_lf_weekly_remaining_cumsum"),
         VE_symptoms == 0.7)%>%
  mutate(measure_VE = case_when(measure == "N_lf_weekly_cumsum" ~ measure,
                                measure == "N_lf_weekly_remaining_cumsum" ~ paste0(measure, "_", VE_symptoms),
                                T ~ "ERROR"))%>%
  mutate(measure_VE = factor(measure_VE, 
                             levels = c("N_lf_weekly_cumsum", "N_lf_weekly_remaining_cumsum_0.7", "N_lf_weekly_remaining_cumsum_0.9"),
                             labels = c("No vaccine", "With vaccine\n(ages 2+, VE=70%)", "With vaccine\n(ages 2+, VE=90%)")))%>%
  mutate(date_proxy = Year + ((week-1)/52))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = measure_VE, fill = measure_VE))+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+  
  geom_ribbon(alpha = 0.25, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Cumulative symptomatic cases")+xlab("Year")+
  scale_y_continuous(labels = comma)+
  scale_colour_discrete("")+
  scale_fill_discrete("")+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_vaccine_cumul_combined, file = "p_lf_weekly_vaccine_cumul_combined.png", width = 12, height = 10, units = "cm")


### CASES AVERTED BY VACCINE GROUP ###

### Weekly
p_lf_weekly_vaccine_strategy = df_infections_weekly_vaccinated_i_total_summarised%>%
  filter(measure %in% c("N_lf_weekly_averted"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         strategy = factor(strategy, levels = strategy_levels, labels = strategy_labels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = strategy, fill = strategy))+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+  
  geom_ribbon(alpha = 0.35, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Symptomatic cases averted")+xlab("Year")+
  scale_fill_manual(name = "Vaccination target", values = strategy_cols_maps)+
  scale_colour_manual(name = "Vaccination target", values = strategy_cols_maps)+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")+
  theme(legend.position = "bottom")

ggsave(p_lf_weekly_vaccine_strategy, file = "p_lf_weekly_vaccine_strategy.png", width = 24, height = 10, units = "cm")



### Cumulative
p_lf_weekly_vaccine_cumul_strategy = df_infections_weekly_vaccinated_i_total_cumul_summarised%>%
  filter(measure %in% c("N_lf_weekly_averted_cumsum"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         strategy = factor(strategy, levels = strategy_levels, labels = strategy_labels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = strategy, fill = strategy))+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+  
  geom_ribbon(alpha = 0.35, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Cumulative symptomatic cases averted")+xlab("Year")+
  scale_fill_manual(name = "Vaccination target", values = strategy_cols_maps)+
  scale_colour_manual(name = "Vaccination target", values = strategy_cols_maps)+
  scale_y_continuous(labels = comma)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_vaccine_cumul_strategy, file = "p_lf_weekly_vaccine_cumul_strategy.png", width = 12, height = 10, units = "cm")



### CASES AVERTED ACROSS AGE GOUPS AND STRATEGIES

### Weekly
p_lf_weekly_vaccine_strategy_ag = df_infections_weekly_vaccinated_i_ag_summarised%>%
  filter(measure %in% c("N_lf_weekly_averted"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         strategy = factor(strategy, levels = strategy_levels, labels = strategy_labels),
         ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = ag, fill = ag))+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+  
  geom_ribbon(alpha = 0.35, colour = NA)+
  geom_line()+
  theme_classic()+
  ylab("Symptomatic cases averted")+xlab("Year")+
  scale_fill_manual(name = "Age group of\ncases averted", values = ag_cols_map)+
  scale_colour_manual(name = "Age group of\ncases averted", values = ag_cols_map)+
  scale_y_continuous(labels = comma)+
  facet_wrap(facets = vars(strategy), nrow = 4)+
  geom_hline(yintercept = 0, colour = "grey")+
  theme(legend.position = "right")

ggsave(p_lf_weekly_vaccine_strategy_ag, file = "p_lf_weekly_vaccine_strategy_ag.png", width = 18, height = 18, units = "cm")



### Cumul
p_lf_weekly_vaccine_cumul_strategy_ag = df_infections_weekly_vaccinated_i_ag_cumul_summarised%>%
  filter(measure %in% c("N_lf_weekly_averted_cumsum"),
         VE_symptoms == 0.7)%>%
  mutate(date_proxy = Year + ((week-1)/52),
         strategy = factor(strategy, levels = strategy_levels, labels = strategy_labels),
         ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = date_proxy, y = mean, ymin = q025, ymax = q975, colour = ag, fill = ag))+
  annotate("rect", xmin = 2025+(vacc_first_day/365), xmax = 2025+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2026+(vacc_first_day/365), xmax = 2026+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+
  annotate("rect", xmin = 2027+(vacc_first_day/365), xmax = 2027+(vacc_last_day/365), ymin = 0, ymax = Inf, alpha = 0.3, fill = "grey", colour = NA)+  geom_ribbon(alpha = 0.35, colour = NA)+
  geom_line()+
  theme_classic()+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  ylab("Total symptomatic cases averted\ndue to vaccination (cumulative)")+xlab("Year")+
  scale_fill_manual(name = "Age group of\ncases averted", values = ag_cols_map)+
  scale_colour_manual(name = "Age group of\ncases averted", values = ag_cols_map)+
  #scale_y_continuous(labels = comma)+
  facet_wrap(facets = vars(strategy), ncol = 4)+
  geom_hline(yintercept = 0, colour = "grey")

ggsave(p_lf_weekly_vaccine_cumul_strategy_ag, file = "p_lf_weekly_vaccine_cumul_strategy_ag.png", width = 22, height = 10, units = "cm")
ggsave(p_lf_weekly_vaccine_cumul_strategy_ag, file = "p_lf_weekly_vaccine_cumul_strategy_ag_resize.png", width = 18, height = 9, units = "cm")

ggsave(p_lf_weekly_vaccine_cumul_strategy_ag, file = "p_lf_weekly_vaccine_cumul_strategy_ag.tiff", width = 22, height = 10, units = "cm")
ggsave(p_lf_weekly_vaccine_cumul_strategy_ag, file = "p_lf_weekly_vaccine_cumul_strategy_ag_resize.tiff", width = 18, height = 9, units = "cm")

