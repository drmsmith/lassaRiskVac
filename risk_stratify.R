

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


#######################
### LOAD PARAMETERS ###
#######################

# demography
files_demography = list.files("demography/")
for(file_i in files_demography){load(paste0("demography/",file_i))}

################################################################
### Infections with draws on which to build hospitalisations ###
################################################################

### this will build on underlying infections in each age group,
### and adds variable representing the different Monte Carlo draws

### LOAD FINAL 2025 INFECTIONS FINAL AGES

df_infections_finalAges = loadRData("infections/projections_annual/df_infections_projected_finalAges.Rdata")%>%
  filter(Year == 2025,
         n_draw < 501)

n_draws = max(unique(df_infections_finalAges$n_draw))

df_draws = data.frame(n_draw = 1:n_draws)

########################
### HOSPITALISATIONS ###
########################

### Simulate prob hosp (overall population level)
### Load in aggregate population-level estimates from Smith et al.

### PROB HOSP
set.seed(20241002)

p_hosp_mean = 0.008708567
p_hosp_sd = 0.001486631
parvec_prob_hosp = rnorm(n_draws, p_hosp_mean, p_hosp_sd)

df_prob_hosp = data.frame(n_draw = 1:n_draws, prob_hosp = parvec_prob_hosp)


############################################################
### Load annual age- and sex-stratified hospitalisations ###
############################################################

df_hospital_nigeria = read.csv("parameters_data/age_groups_hospitalisations_NCDC.csv")

### Group across years

df_hospital_nigeria = df_hospital_nigeria%>%
  group_by(AgeGroup)%>%
  summarise(Male = sum(Male),
            Female = sum(Female))%>%
  mutate(AgeGroup = factor(AgeGroup,
                           levels = c("<10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", ">70")))

## NB: <10 is really <=10

# Define the mapping of hospital age groups to individual ages
age_mapping_hospital <- list(
  "<10"  = 0:10,
  "11-20"  = 11:20,
  "21-30" = 21:30,
  "31-40" = 31:40,
  "41-50" = 41:50,
  "51-60" = 51:60,
  "61-70" = 61:70,
  ">70" = 71:80  # 80 is max in underlying population data
)


#########################################################################
### Determine proportion of population in each hospital-age-sex group ###
#########################################################################

if(length(age_mapping_hospital) != 8){warning("STOP! Need to update infection age allocation")}

hospital_group_mins = c(age_mapping_hospital[[1]][1],
                        age_mapping_hospital[[2]][1],
                        age_mapping_hospital[[3]][1],
                        age_mapping_hospital[[4]][1],
                        age_mapping_hospital[[5]][1],
                        age_mapping_hospital[[6]][1],
                        age_mapping_hospital[[7]][1],
                        age_mapping_hospital[[8]][1])


df_population_finalAges_adjustHospital = df_population%>%
  filter(Year == 2025)%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, Age, UN_scaled_subnational )%>%
  filter(Sex %in% c("Male", "Female"))%>%
  mutate(ag = case_when(Age >= hospital_group_mins[1] & Age < hospital_group_mins[2] ~ names(age_mapping_hospital[1]),
                        Age >= hospital_group_mins[2] & Age < hospital_group_mins[3] ~ names(age_mapping_hospital[2]),
                        Age >= hospital_group_mins[3] & Age < hospital_group_mins[4] ~ names(age_mapping_hospital[3]),
                        Age >= hospital_group_mins[4] & Age < hospital_group_mins[5] ~ names(age_mapping_hospital[4]),
                        Age >= hospital_group_mins[5] & Age < hospital_group_mins[6] ~ names(age_mapping_hospital[5]),
                        Age >= hospital_group_mins[6] & Age < hospital_group_mins[7] ~ names(age_mapping_hospital[6]),
                        Age >= hospital_group_mins[7] & Age < hospital_group_mins[8] ~ names(age_mapping_hospital[7]),
                        Age >= hospital_group_mins[8] ~ names(age_mapping_hospital[8])))


df_population_hospital_within_ag = df_population_finalAges_adjustHospital%>%
  group_by(Country, GID_0, Region, GID_1, Sex, ag)%>%
  mutate(PopProp = UN_scaled_subnational / sum(UN_scaled_subnational))


######################################################
### Adjust hospital age groups to final age groups ###
######################################################

### Note that because hospital data are pooled across several states, the population pyramid
### from the state reporting the most cases (Ondo) was used to scale hospitalisations
### into desired age groups

District_HospitalData = c("NGA.29_1")

# Initialize an empty tibble for the new final data
df_hospital_nigeria_allAges <- tibble()

# Loop through each row in the summed data
for (i in seq_len(nrow(df_hospital_nigeria))) {
  row <- df_hospital_nigeria[i, ]
  ages <- age_mapping_hospital[[as.character(row$AgeGroup)]]
  
  # Determine district and district-specific population weighting
  District <- District_HospitalData
  
  PopProps <- filter(df_population_hospital_within_ag, 
                     GID_1 == District,
                     Age %in% ages)
  
  # Extract vector of proportion of infections within each age-sex group occurring in each stratum
  PopPropsMale <- PopProps[which(PopProps[,"Sex"] == "Male"),"PopProp"]
  PopPropsFemale <- PopProps[which(PopProps[,"Sex"] == "Female"),"PopProp"]
  
  
  # Multiply hospitalisations by proportion occurring in each stratum
  MaleHospital <- row$Male * PopPropsMale
  FemaleHospital <- row$Female * PopPropsFemale

  
  # Create a row for each age
  for (age in ages) {
    new_row <- row[c("AgeGroup", "Male", "Female")]
    new_row$age <- age
    new_row$hospital_male <- MaleHospital[which(age == ages),]
    new_row$hospital_female <- FemaleHospital[which(age == ages),]
    df_hospital_nigeria_allAges <- bind_rows(df_hospital_nigeria_allAges, new_row)
  }
}



### Allocate to final age groups
if(length(age_groups_final) != 6){warning("STOP! Need to update age allocation")}

df_hospital_nigeria_finalAges = df_hospital_nigeria_allAges%>%
  mutate(ag = case_when(age >= age_group_mins[1] & age < age_group_mins[2] ~ age_groups_final[1],
                        age >= age_group_mins[2] & age < age_group_mins[3] ~ age_groups_final[2],
                        age >= age_group_mins[3] & age < age_group_mins[4] ~ age_groups_final[3],
                        age >= age_group_mins[4] & age < age_group_mins[5] ~ age_groups_final[4],
                        age >= age_group_mins[5] & age < age_group_mins[6] ~ age_groups_final[5],
                        age >= age_group_mins[6] ~ age_groups_final[6]))%>%
  mutate(COUNTRY = "Nigeria")%>%
  group_by(COUNTRY, ag)%>%
  summarise(hospital_male = sum(hospital_male),
            hospital_female = sum(hospital_female))%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  arrange(ag)


### stop if the post-processed totals do not match pre-processed totals
hospitalTotalsPre = sum(df_hospital_nigeria$Male) + sum(df_hospital_nigeria$Female)
hospitalTotalsPost = sum(df_hospital_nigeria_finalAges$hospital_male) + sum(df_hospital_nigeria_finalAges$hospital_female)
stopifnot(abs(hospitalTotalsPost - hospitalTotalsPre) < 0.02)

###############################################################
### Age-specific hospital proportions: Dirichlet distribute ###
###############################################################

### METHOD: Apply age-sex proportions from Nigeria to every district
### and apply this in Guinea, Liberia and Sierra Leone

### Step 1: calculate total expected hospitalisations in each district based on 
### infections and Monte Carlo draws of prob_hosp

df_hospital_totals = df_infections_finalAges%>%
  group_by(Country, Region, GID_1, n_draw)%>%
  summarise(N_infection = sum(N_infection))%>%
  left_join(df_prob_hosp, by = "n_draw")%>%
  mutate(N_hospital = N_infection * prob_hosp)%>%
  ### remove NAs (because not enough Monte Carlo sims)
  na.omit()



# check total % of infections hospitalised equals mean prob_hosp
check_prob_hosp = mean(df_prob_hosp$prob_hosp)
check_prop_hosp = sum(df_hospital_totals$N_hospital)/sum(df_hospital_totals$N_infection)
stopifnot(abs(check_prob_hosp - check_prop_hosp) < 0.001)

### Step 2: Generate Dirichlet samples with 100 runs to get probabilities for each age-sex group

# Dirichlet distribution parameter
alpha <- 1

# Hospitalised cases in order of Male first, then Female 
trans1 <- c(df_hospital_nigeria_finalAges$hospital_male, 
            df_hospital_nigeria_finalAges$hospital_female) 
trans <- matrix(c(trans1), ncol = length(trans1), byrow = TRUE)

### Step 3: Draw from Dirichlet distribution
set.seed(20241002)
tp.sample <- hesim::rdirichlet_mat(n = n_draws, alpha + trans)
draws_hospital_all <- matrix(tp.sample, ncol = length(age_groups_final)*2, nrow = n_draws, byrow = TRUE)

### Step 4: Ensure the generated probabilities align with expectations
expected_probs <- trans1 / sum(trans1)
observed_probs <- colMeans(draws_hospital_all)
stopifnot(abs(expected_probs - observed_probs) < 0.02)

### Step 5: Format age-sex hospital probability draws
df_age_sex_groups = data.frame(Sex = c(rep("Male", nrow(df_hospital_nigeria_finalAges)), rep("Female", nrow(df_hospital_nigeria_finalAges))),
                               ag = c(rep(df_hospital_nigeria_finalAges$ag, 2)))%>%
  t()

if(length(age_groups_final) != 6){warning("STOP! Must update age group allocation")}

df_draws_hospital_all = data.frame(draws_hospital_all)%>%
  bind_cols(., df_draws)%>%
  pivot_longer(-c(n_draw), values_to = "PropHosp")%>%
  mutate(Sex = case_when(name %in% c(paste0("X",1:length(age_groups_final))) ~ "Male",
                        T ~ "Female"),
         ag = case_when(name %in% paste0("X", c(1, 1 + length(age_groups_final))) ~ age_groups_final[1],
                        name %in% paste0("X", c(2, 2 + length(age_groups_final))) ~ age_groups_final[2],
                        name %in% paste0("X", c(3, 3 + length(age_groups_final))) ~ age_groups_final[3],
                        name %in% paste0("X", c(4, 4 + length(age_groups_final))) ~ age_groups_final[4],
                        name %in% paste0("X", c(5, 5 + length(age_groups_final))) ~ age_groups_final[5],
                        T ~ age_groups_final[6]))%>%
  dplyr::select(-name)

# check within-draw probabilities sum to 1
check_within_draw_hosp_probs = df_draws_hospital_all%>%group_by(n_draw)%>%summarise(sum=sum(PropHosp))
stopifnot(abs(sum(check_within_draw_hosp_probs$sum) - nrow(check_within_draw_hosp_probs)) < 0.0001)

### Step 6: calculate age- and sex-specific hospitalisations from national totals in Nigeria and above proportions
### and back-calculate age- and sex-specific infection-hospitalisation rates
### the same age-sex distribution is used nationally, so any district can be used
df_hospital_totals_stratified_nigeria = df_hospital_totals%>%
  filter(Country == "Nigeria", Region == "Ondo")%>%
  left_join(., df_draws_hospital_all, by = "n_draw", relationship = "many-to-many")%>%
  mutate(N_hospital_str = N_hospital*PropHosp)%>%
  dplyr::select(-c(N_infection, prob_hosp))

df_infections_stratified_nigeria = df_infections_finalAges%>%
  filter(Country == "Nigeria", Region == "Ondo", Year == 2025, n_draw <= n_draws)%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, ag, n_draw, N_infection)

df_infection_hospital_risk = df_infections_stratified_nigeria%>%
  left_join(., df_hospital_totals_stratified_nigeria, 
            by = c("Country", "Region", "GID_1", "Sex", "ag", "n_draw"))%>%
  mutate(prob_hosp_str = N_hospital_str/N_infection)

# check N_hospital identical before and after stratification
check_N_hosp_pre = sum(df_hospital_totals_stratified_nigeria$N_hospital_str)
check_N_hosp_post = sum(df_infection_hospital_risk$N_hospital_str)
stopifnot(abs(check_N_hosp_pre - check_N_hosp_post) < 0.001)

###########################################
### FINAL DATA SET IS LOCATION AGNOSTIC ###
###########################################

df_infection_hospital_risk_str = df_infection_hospital_risk%>%
  dplyr::select(-c(Country, GID_0, Region, GID_1, N_infection, N_hospital, N_hospital_str))

##################################################
### Plot hospitalisation age-sex distributions ###
##################################################

p_prop_hosp_ag = df_infection_hospital_risk_str%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = PropHosp, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.3, alpha = 0.3)+
  theme_bw()+
  facet_wrap(facets = vars(Sex))+
  xlab("Age group")+ylab("Proportion of Lassa fever hospitalisations")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,NA)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_prob_hosp_ag = df_infection_hospital_risk_str%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = prob_hosp_str, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.3, alpha = 0.3)+
  theme_bw()+
  facet_wrap(facets = vars(Sex))+
  xlab("Age group")+ylab("Infection-hospitalisation risk")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,NA)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_hosp_risk_str = ggarrange(p_prop_hosp_ag,
          p_prob_hosp_ag,
          ncol = 2, labels = c("A", "B"),
          legend = "none",
          align = "hv")
p_hosp_risk_str

ggsave(p_prop_hosp_ag+theme(legend.position = "none"), file = "p_prop_hosp_ag.png", width = 10, height = 10, units = "cm")
ggsave(p_prob_hosp_ag+theme(legend.position = "none"), file = "p_prob_hosp_ag.png", width = 10, height = 10, units = "cm")
ggsave(p_hosp_risk_str, file = "p_hosp_risk_str.png", width = 16, height = 10, units = "cm")

#######################
### SAVE FINAL DATA ###
#######################

write.csv(df_infection_hospital_risk_str, "df_infection_hospital_risk_str.csv")





#################################
### AGE-SPECIFIC SYMPTOM RISK ###
#################################

### For scenario analysis where age-sex symptom risk follows hospital age-sex risk

################
### SYMPTOMS ###
################

### Simulate prob symptoms (overall population level)
### Load in aggregate population-level estimates from Smith et al.

p_symptoms_mean = -1.466337
p_symtpoms_sd = 0.3698002

### Check values
# check <- inv.logit(rnorm(10000, mean=(p_symptoms_metaprop$TE.random),
#                              (p_symptoms_metaprop$seTE.random)))
# 
# quantile(check, probs=c(0.025,0.5,0.975))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_prob_symptoms = inv.logit(rnorm(n_draws_montecarlo, mean=p_symptoms_mean, sd=p_symtpoms_sd))

df_prob_symptoms = data.frame(n_draw = 1:n_draws, prob_symptoms = parvec_prob_symptoms)


###############################################
### BASE CASE: SAME RISK FOR ALL AGE GROUPS ###
###############################################

df_infection_symptom_risk = df_infection_hospital_risk%>%
  dplyr::select(c(Sex, ag, n_draw, N_infection))%>%
  group_by(n_draw)%>%
  summarise(N_infection = sum(N_infection))%>%
  left_join(., df_prob_symptoms, by = "n_draw")%>%
  mutate(N_symptoms_total = N_infection * prob_symptoms)%>%
  dplyr::select(n_draw, N_symptoms_total)

df_infection_symptom_risk_str = df_infection_hospital_risk_str%>%
  left_join(df_prob_symptoms, by = "n_draw")%>%
  rename(prob_symptoms_str = prob_symptoms)%>%
  dplyr::select(-c(PropHosp, prob_hosp_str))


##############################################
### ALT SCENARIO: INCREASING RISK WITH AGE ###
##############################################

### mean is 20%, vast majority of infections in 2-14 year olds, so consider
### x0.5 in <2, x1 in 2-14, x1.5 in 15-24, x2 in 25-34, x2.5 in 35-49, x3 in 50+
### and then multiply all prob_symptoms_str by the coefficient that reproduces
### the total number of symptoms predicted by the age-invariant estimate

df_infection_symptom_risk_str_scen3_unweighted = df_infection_symptom_risk_str%>%
  mutate(prob_symptoms_str = case_when(ag == "<2" ~ prob_symptoms_str * 0.5,
                                       ag == "2-14" ~ prob_symptoms_str,
                                       ag == "15-24" ~ prob_symptoms_str * 1.5,
                                       ag == "25-34" ~ prob_symptoms_str * 2,
                                       ag == "35-49" ~ prob_symptoms_str * 2.5,
                                       ag == "50+" ~ prob_symptoms_str * 3,
                                       T ~ NA))%>%
  left_join(., df_infection_hospital_risk%>%dplyr::select(c(Sex, ag, n_draw, N_infection)),
            by = c("Sex", "ag", "n_draw"))%>%
  left_join(., df_infection_symptom_risk, by = c("n_draw"))


### LOOP THROUGH TO SCALE AGE-SPECIFIC INFECTIONS TO TOTALS ###

### Set objective function to minimize 
objective_function <- function(x) {
  sum(vec_probs_symptoms_str_unweighted * vec_infections * x) - N_symptoms_total
}

list_infection_symptom_risk_str_scen3 = list()

### Loop through every draw of prob_symps and scale for all ages to reproduce total symps
set.seed(20241129)
for(draw_i in 1:max(df_infection_symptom_risk_str_scen3_unweighted$n_draw)){
  
  df_i = df_infection_symptom_risk_str_scen3_unweighted%>%filter(n_draw == draw_i)
  
  ### what is the age-specific symptom risk before weighting to reproduce N_symptoms_total
  vec_probs_symptoms_str_unweighted = df_i$prob_symptoms_str
  
  ### how many infections in each age-sex group
  vec_infections = df_i$N_infection
  
  ### the final number of symptoms to reproduce
  N_symptoms_total = unique(df_i$N_symptoms_total)
  
  if(length(N_symptoms_total)>1){warning("ERROR: multiple total sytmpom counts per draw")}
  
  ### use optim to minimize squared error of 
  optimal_x <- optim(c(10), 
                     fn = function(x) objective_function(x)^2,
                     method = "BFGS")
  
  df_i_weighted = df_i%>%mutate(prob_symptoms_str = prob_symptoms_str * optimal_x$par,
                                N_symptoms_str = N_infection * prob_symptoms_str)
  
  ### Ensure the total unstratified number of symptoms is reproduced by new age-specific risk
  stopifnot(abs(N_symptoms_total - sum(df_i_weighted$N_symptoms_str)) < 1)
  
  list_infection_symptom_risk_str_scen3[[draw_i]] <- df_i_weighted
}

df_infection_symptom_risk_str_scen3 = bind_rows(list_infection_symptom_risk_str_scen3)%>%
  dplyr::select(Sex, ag, n_draw, prob_symptoms_str)

### In one single simulation, prob_symptoms_str > 1 in aged 50+, so we truncate that to 1
df_infection_symptom_risk_str_scen3$prob_symptoms_str[df_infection_symptom_risk_str_scen3$prob_symptoms_str>1] <- 1

#######################################################################
### CHECK TO SEE THERE AREN'T CASES WHERE PROB_HOSP > PROB_SYMPTOMS ###
#######################################################################

### BECAUSE IN FINAL MODEL, NUMBER MILD/MODERATE IS PROB SYMPTOMS - PROB HOSP

##################
### SCENARIO 1 ###
##################

df_infection_symptom_hospital_scen1 = df_infection_hospital_risk%>%dplyr::select(c(Sex, ag, n_draw, N_infection))%>%
  left_join(., df_infection_symptom_risk_str, by = c("ag", "Sex", "n_draw"))%>%
  left_join(., df_infection_hospital_risk_str, by = c("ag", "Sex", "n_draw"))%>%
  mutate(prob_mild_str = prob_symptoms_str - prob_hosp_str)


##################
### SCENARIO 3 ###
##################

df_infection_symptom_hospital_scen3 = df_infection_hospital_risk%>%dplyr::select(c(Sex, ag, n_draw, N_infection))%>%
  left_join(., df_infection_symptom_risk_str_scen3, by = c("ag", "Sex", "n_draw"))%>%
  left_join(., df_infection_hospital_risk_str, by = c("ag", "Sex", "n_draw"))%>%
  mutate(prob_mild_str = prob_symptoms_str - prob_hosp_str)



#################################################################
### PLOT BASE CASE VERSUS SCENARIO ANALYSIS FOR PROB_SYMPTOMS ###
#################################################################

p_prob_symptoms_ag_scen1 = df_infection_symptom_risk_str%>%
  filter(Sex == "Male")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = prob_symptoms_str, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.5, alpha = 0.3)+
  theme_bw()+
  xlab("Age group")+ylab("Infection-symptom risk")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_prob_symptoms_ag_scen3 = df_infection_symptom_risk_str_scen3%>%
  filter(Sex == "Male")%>%
  mutate(ag = factor(ag, levels = ag_levels))%>%
  ggplot(., aes(x = ag, y = prob_symptoms_str, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.5, alpha = 0.3)+
  theme_bw()+
  xlab("Age group")+ylab("Infection-symptom risk")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p_symptom_risk_str = ggarrange(p_prob_symptoms_ag_scen1,
                               p_prob_symptoms_ag_scen3,
                            ncol = 2, labels = c("A", "B"),
                            legend = "none",
                            align = "hv")
p_symptom_risk_str

ggsave(p_symptom_risk_str, file = "p_symptom_risk_str.png", width = 16, height = 12, units = "cm")

### summary stats:
df_prob_symptoms%>%
  summarise(mean = mean(prob_symptoms),
            lower = quantile(prob_symptoms, 0.025),
            upper = quantile(prob_symptoms, 0.975))

df_infection_symptom_risk_str_scen3%>%
  group_by(Sex, ag)%>%
  summarise(mean = mean(prob_symptoms_str),
            lower = quantile(prob_symptoms_str, 0.025),
            upper = quantile(prob_symptoms_str, 0.975))

###################################################
### PLOT PROB HOSP, PROB SYMPTOMS AND PROB MILD ###
###################################################

p_prob_symptoms_ag_severities_scen1 = df_infection_symptom_hospital_scen1%>%
  filter(Sex == "Male")%>%
  dplyr::select(-c(N_infection, PropHosp))%>%
  pivot_longer(-c(Sex, ag, n_draw), values_to = "Probability", names_to = "Level")%>%
  mutate(ag = factor(ag, levels = ag_levels),
         Level = factor(Level,
                        levels = c("prob_symptoms_str", "prob_hosp_str", "prob_mild_str"),
                        labels = c( "Any symptoms", "Severe symptoms","Mild symptoms")))%>%
  ggplot(., aes(x = ag, y = Probability, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.5, alpha = 0.3)+
  theme_bw()+
  xlab("Age group")+ylab("Probability given infection")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(facets = vars(Level))

p_prob_symptoms_ag_severities_scen3 = df_infection_symptom_hospital_scen3%>%
  filter(Sex == "Male")%>%
  dplyr::select(-c(N_infection, PropHosp))%>%
  pivot_longer(-c(Sex, ag, n_draw), values_to = "Probability", names_to = "Level")%>%
  mutate(ag = factor(ag, levels = ag_levels),
         Level = factor(Level,
                        levels = c("prob_symptoms_str", "prob_hosp_str", "prob_mild_str"),
                        labels = c( "Any symptoms", "Severe symptoms","Mild symptoms")))%>%
  ggplot(., aes(x = ag, y = Probability, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, lwd = 0.3)+
  geom_jitter(height = 0, width = 0.25, size = 0.5, alpha = 0.3)+
  theme_bw()+
  xlab("Age group")+ylab("Probability given infection")+
  scale_fill_manual("Age group", values = ag_cols_map)+
  scale_colour_manual("Age group", values = ag_cols_map)+
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(facets = vars(Level))

p_symptom_risk_severities_str = ggarrange(p_prob_symptoms_ag_severities_scen1,
                                          p_prob_symptoms_ag_severities_scen3,
                               nrow = 2, labels = c("A", "B"),
                               legend = "none",
                               align = "hv")
p_symptom_risk_severities_str

ggsave(p_symptom_risk_severities_str, file = "p_symptom_risk_severities_str.png", width = 14, height = 16, units = "cm")
  

#######################
### SAVE FINAL DATA ###
#######################

write.csv(df_infection_symptom_risk_str, "df_infection_symptom_risk_str.csv")
write.csv(df_infection_symptom_risk_str_scen3, "df_infection_symptom_risk_str_scen3.csv")


#############
### DEATH ###
#############

### Case fatality in retrospective cohort study:
# Okokhere et al., Lancet ID 2018
# https://www.sciencedirect.com/science/article/pii/S147330991830121X?via%3Dihub#ecomp10

read_excel("parameters_data/CFR_okokhere.xlsx")

### Case fatality in prospective cohort study:
# Duvignaud et al., Lancet GH 2021
# https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30518-0/fulltext

df_cfr_lascope_main = read_excel("parameters_data/CFR_duvignaud.xlsx")

# Lascope: we observed no association between mortality and sex

### replace Lascope data in children with follow-up data from paediatric-specific study -- much larger sample
### remove neonatal deaths (<1 month) since these are modelled separately later in the pipeline

df_cfr_lascope_paeds = data.frame(ag = c("<1", "1_11"),
                                  N = c(11, 76),
                                  D_exact = c(1, 1))%>%
  mutate(CFR_exact = D_exact / N)

df_cfr_lascope = bind_rows(df_cfr_lascope_paeds,
                           df_cfr_lascope_main%>%dplyr::filter(! ag %in% c("<1", "1_11")))

#########################################################
### DEATH OPTION 1: CALCULATE CFR WITHOUT UNCERTAINTY ###
#########################################################


########################
### UPATE AGE GROUPS ###
########################

# Define the mapping of hospital age groups to individual ages
age_mapping_cfr <- list(
  "<1"  = 0,
  "1_11"  = 1:11,
  "12_17" = 12:17,
  "18_44" = 18:44,
  "45_59" = 45:59,
  "60+" = 60:80  # 80 is max in underlying population data
)

# Define minimum age for each cfr age bracket
cfr_group_mins = c(age_mapping_cfr[[1]][1],
                   age_mapping_cfr[[2]][1],
                   age_mapping_cfr[[3]][1],
                   age_mapping_cfr[[4]][1],
                   age_mapping_cfr[[5]][1],
                   age_mapping_cfr[[6]][1])

# Define full population age groups following CFR age brackets
df_population_finalAges_adjustCFR = df_population%>%
  filter(Year == 2025, Sex %in% c("Male", "Female"))%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, Age, UN_scaled_subnational )%>%
  mutate(ag = case_when(Age >= cfr_group_mins[1] & Age < cfr_group_mins[2] ~ names(age_mapping_cfr[1]),
                        Age >= cfr_group_mins[2] & Age < cfr_group_mins[3] ~ names(age_mapping_cfr[2]),
                        Age >= cfr_group_mins[3] & Age < cfr_group_mins[4] ~ names(age_mapping_cfr[3]),
                        Age >= cfr_group_mins[4] & Age < cfr_group_mins[5] ~ names(age_mapping_cfr[4]),
                        Age >= cfr_group_mins[5] & Age < cfr_group_mins[6] ~ names(age_mapping_cfr[5]),
                        Age >= cfr_group_mins[6] ~ names(age_mapping_cfr[6])))

df_population_finalAges_adjustCFR_combineSexes = df_population_finalAges_adjustCFR%>%
  group_by(Country, GID_0, Region, GID_1, Age, ag)%>%
  summarise(UN_scaled_subnational = sum(UN_scaled_subnational))

df_population_cfr_within_ag = df_population_finalAges_adjustCFR_combineSexes%>%
  group_by(Country, GID_0, Region, GID_1, ag)%>%
  mutate(PopProp = UN_scaled_subnational  / sum(UN_scaled_subnational))


#################################################
### Adjust CFR age groups to final age groups ###
#################################################

### Note that LASCOPE was conducted in Owo, Ondo State, so this "district" is applied

District_CFRData = c("NGA.29_1")

# Initialize an empty tibble for the new final data
df_CFR_allAges <- tibble()

# Loop through each row in the summed data
for (i in seq_len(nrow(df_cfr_lascope))) {
  row <- df_cfr_lascope[i, ]
  ages <- age_mapping_cfr[[as.character(row$ag)]]

  # Determine district and district-specific population weighting
  District <- District_CFRData

  PopProps <- filter(df_population_cfr_within_ag,
                     GID_1 == District,
                     Age %in% ages)


  # Extract vector of proportion of infections within each age-sex group occurring in each stratum
  PopProps <- PopProps[,"PopProp"]


  # Multiply infections by proportion occurring in each stratum
  Cases <- row$N * PopProps
  Deaths <- row$D_exact * PopProps

  # Create a row for each age
  for (age in ages) {
    new_row <- row[c("ag", "N", "D_exact")]
    new_row$age <- age
    new_row$Case <- Cases[which(age == ages),]
    new_row$Death <- Deaths[which(age == ages),]
    df_CFR_allAges <- bind_rows(df_CFR_allAges, new_row)
  }
}


### Allocate to final age groups
if(length(age_groups_final) != 6){warning("STOP! Need to update age allocation")}

df_CFR_finalAges = df_CFR_allAges%>%
  mutate(ag = case_when(age >= age_group_mins[1] & age < age_group_mins[2] ~ age_groups_final[1],
                        age >= age_group_mins[2] & age < age_group_mins[3] ~ age_groups_final[2],
                        age >= age_group_mins[3] & age < age_group_mins[4] ~ age_groups_final[3],
                        age >= age_group_mins[4] & age < age_group_mins[5] ~ age_groups_final[4],
                        age >= age_group_mins[5] & age < age_group_mins[6] ~ age_groups_final[5],
                        age >= age_group_mins[6] ~ age_groups_final[6]))%>%
  group_by(ag)%>%
  summarise(Case = sum(Case),
            Death = sum(Death))%>%
  mutate(CFR = Death/Case,
         ag = factor(ag, levels = age_groups_final))%>%
  arrange(ag)


### stop if the post-processed death totals do not match pre-processed totals
deathTotalsPre = sum(df_cfr_lascope$D_exact)
deathTotalsPost = sum(df_CFR_finalAges$Death)
stopifnot(abs(deathTotalsPost - deathTotalsPre) < 0.02)


### DISCUSSION POINT: these adjusted numbers consistent with case fatality in pediatric from LASCOPE follow-up:
# LASCOPE, Duvignaud et al., J Ped Infect Dis Soc 2024
# https://academic.oup.com/jpids/advance-article/doi/10.1093/jpids/piae083/7738269?login=true
CFR_0_4 = 4/45
CFR_5_14 = 3/78




#######################################################
### DEATH OPTION 2: CALCULATE CFR WITH BOOSTRAPPING ### 
#######################################################

### BOOSTRAP NEW ###

n_bootstrap = 500

list_CFR_lascopeAges_bootstrap = list()

set.seed(20241002)
for(b in 1:n_bootstrap){
  
  print(paste0("on run ", b, " of ", n_bootstrap, " bootstrap resamples"))
  
  df_cfr_bootstrap = data.frame()
  
  ############################################
  ### BOOTSTRAP SIMULATE DEATHS IN LASCOPE ###
  ############################################
  
  for(i in 1:nrow(df_cfr_lascope)){
    
    # extract data for each age group
    ag_i = df_cfr_lascope$ag[i]
    N_i = df_cfr_lascope$N[i]
    D_i = df_cfr_lascope$D_exact[i]
    
    # create binary vector of deaths
    vec_deaths_binary = c(rep(0, N_i - D_i), rep(1, D_i))
    
    # sample with replacement
    vec_bootstrap = sample(vec_deaths_binary, length(vec_deaths_binary), replace = T)
    
    D_j = sum(vec_bootstrap)
    
    df_cfr_bootstrap_i = data.frame(ag = ag_i,
                           N = N_i,
                           D_exact = D_j,
                           CFR_exact = D_j/N_i,
                           boot = b)
    
    df_cfr_bootstrap = rbind(df_cfr_bootstrap, df_cfr_bootstrap_i)
    
  }
  
  list_CFR_lascopeAges_bootstrap[[b]] <- df_cfr_bootstrap
}


df_CFR_lascopeAges_bootstrap = bind_rows(list_CFR_lascopeAges_bootstrap)


########################
### UPATE AGE GROUPS ###
########################

# Define the mapping of hospital age groups to individual ages
age_mapping_cfr <- list(
  "<1"  = 0,
  "1_11"  = 1:11,
  "12_17" = 12:17,
  "18_44" = 18:44,
  "45_59" = 45:59,
  "60+" = 60:80  # 80 is max in underlying population data
)

# Define minimum age for each cfr age bracket
cfr_group_mins = c(age_mapping_cfr[[1]][1],
                   age_mapping_cfr[[2]][1],
                   age_mapping_cfr[[3]][1],
                   age_mapping_cfr[[4]][1],
                   age_mapping_cfr[[5]][1],
                   age_mapping_cfr[[6]][1])

# Define full population age groups following CFR age brackets
df_population_lascopeAges_adjustCFR = df_population%>%
  filter(Year == 2025)%>%
  dplyr::select(Country, GID_0, Region, GID_1, Sex, Age, UN_scaled_subnational)%>%
  filter(Sex %in% c("Male", "Female"))%>%
  mutate(ag = case_when(Age >= cfr_group_mins[1] & Age < cfr_group_mins[2] ~ names(age_mapping_cfr[1]),
                        Age >= cfr_group_mins[2] & Age < cfr_group_mins[3] ~ names(age_mapping_cfr[2]),
                        Age >= cfr_group_mins[3] & Age < cfr_group_mins[4] ~ names(age_mapping_cfr[3]),
                        Age >= cfr_group_mins[4] & Age < cfr_group_mins[5] ~ names(age_mapping_cfr[4]),
                        Age >= cfr_group_mins[5] & Age < cfr_group_mins[6] ~ names(age_mapping_cfr[5]),
                        Age >= cfr_group_mins[6] ~ names(age_mapping_cfr[6])))

df_population_lascopeAges_adjustCFR_combineSexes = df_population_lascopeAges_adjustCFR%>%
  group_by(Country, GID_0, Region, GID_1, Age, ag)%>%
  summarise(UN_scaled_subnational = sum(UN_scaled_subnational))

df_population_cfr_within_ag = df_population_lascopeAges_adjustCFR_combineSexes%>%
  group_by(Country, GID_0, Region, GID_1, ag)%>%
  mutate(PopProp = UN_scaled_subnational / sum(UN_scaled_subnational))

#################################################
### Adjust CFR age groups to final age groups ###
#################################################

### Note that LASCOPE was conducted in Owo, Ondo State, so this "district" is applied

District_CFRData = c("NGA.29_1")

# Initialize an empty tibble for the new final data
list_CFR_allAges <- list()

# Loop through each row in the summed data
for (i in seq_len(nrow(df_CFR_lascopeAges_bootstrap))) {
  
  if(i%%100 == 0){print(paste0(" on row ", i, " of ", nrow(df_CFR_lascopeAges_bootstrap)))}
  
  row <- df_CFR_lascopeAges_bootstrap[i, ]
  ages <- age_mapping_cfr[[as.character(row$ag)]]
  
  # Determine district and district-specific population weighting
  District <- District_CFRData
  
  PopProps <- filter(df_population_cfr_within_ag, 
                     GID_1 == District,
                     Age %in% ages)
  
  
  # Extract vector of proportion of infections within each age-sex group occurring in each stratum
  PopProps <- PopProps[,"PopProp"]
  
  
  # Multiply infections by proportion occurring in each stratum
  Cases <- row$N * PopProps
  Deaths <- row$D_exact * PopProps
  
  boot_i <- row$boot
  
  df_CFR_allAges_i = tibble()
  
  # Create a row for each age
  for (age in ages) {
    new_row <- row[c("ag", "N", "D_exact")]
    new_row$age <- age
    new_row$Case <- Cases[which(age == ages),]
    new_row$Death <- Deaths[which(age == ages),]
    new_row$n_draw <- boot_i
    df_CFR_allAges_i <- bind_rows(df_CFR_allAges_i, new_row)
  }
  
  list_CFR_allAges[[i]] <- df_CFR_allAges_i
}


df_CFR_allAges = bind_rows(list_CFR_allAges)

################################################
### FINAL BOOSTRAPPED, AGE-ADJUSTED CFR DATA ###
################################################

### Allocate to final age groups
if(length(age_groups_final) != 6){warning("STOP! Need to update age allocation")}

df_CFR_finalAges_bootstrap = df_CFR_allAges%>%
  mutate(ag = case_when(age >= age_group_mins[1] & age < age_group_mins[2] ~ age_groups_final[1],
                        age >= age_group_mins[2] & age < age_group_mins[3] ~ age_groups_final[2],
                        age >= age_group_mins[3] & age < age_group_mins[4] ~ age_groups_final[3],
                        age >= age_group_mins[4] & age < age_group_mins[5] ~ age_groups_final[4],
                        age >= age_group_mins[5] & age < age_group_mins[6] ~ age_groups_final[5],
                        age >= age_group_mins[6] ~ age_groups_final[6]))%>%
  group_by(ag, n_draw)%>%
  summarise(Case = sum(Case),
            Death = sum(Death))%>%
  mutate(CFR = Death/Case,
         ag = factor(ag, levels = age_groups_final))%>%
  arrange(ag)


### SUMMARY OF BOOTSTRAP MEANS
df_CFR_finalAges_bootstrap_means = df_CFR_finalAges_bootstrap%>%
  group_by(ag)%>%
  summarise(mean = mean(CFR),
            min = quantile(CFR, 0.025),
            max = quantile(CFR, 0.975))

### PLOT BOOTSTRAPPED DATA ###

p_CFR_finalAges = df_CFR_finalAges_bootstrap%>%
  mutate(ag = factor(ag, levels = ag_levels,
                     labels = c("<2*", ag_levels[2:length(ag_levels)])))%>%
  ggplot(., aes(x = ag, y = CFR, fill = ag, colour = ag))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA)+
  geom_jitter(height = 0, width = 0.25, size = 0.3, alpha = 0.3)+
  theme_bw()+
  xlab("Age group")+ylab("Hospital case-fatality risk")+
  scale_colour_manual(values = c("<2*" = "#e41a1c", ag_cols_map[2:length(ag_cols_map)]))+
  scale_fill_manual(values = c("<2*" = "#e41a1c", ag_cols_map[2:length(ag_cols_map)]))+
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(p_CFR_finalAges, file = "p_CFR_finalAges.png", width = 8, height = 10, units = "cm")




##########################################
### APPLY HIGHER CFR IN PREGNANT WOMEN ###
##########################################

### Do not account for male vs. female: no significant difference found in LASCOPE

df_CFR_finalAges_bootstrap%>%
  mutate(CFR_odds = CFR/(1-CFR))


### OR of death given pregnancy from Kayem et al.
# https://academic.oup.com/trstmh/article/114/5/385/5775496?login=true
OR_death_preg_mean = 2.86
OR_death_preg_l = 1.77
OR_death_preg_u = 4.63


### estimate st. dev and simulate 100 draws assuming log-normal distribution
OR_death_preg_sd_log = ((log(OR_death_preg_mean) - log(OR_death_preg_l))/1.96 + (log(OR_death_preg_u) - log(OR_death_preg_mean))/1.96)/2
set.seed(20241005)
vec_OR_death_preg=exp(rnorm(n_draws, log(OR_death_preg_mean), OR_death_preg_sd_log))

### apply to df with n_draw
df_OR_death_preg = data.frame(OR_death_preg = vec_OR_death_preg,
                              n_draw = 1:n_draws)

### Turn OR into CFR among pregnant women
df_CFR_finalAges_pregOnly = df_CFR_finalAges_bootstrap%>%
  left_join(., df_OR_death_preg, by = "n_draw")%>%
  # baseline odds of death from CFR
  mutate(odds_death_baseline = CFR/(1-CFR))%>%
  # calculate CFR_preg from baseline odds and OR
  mutate(CFR_preg = (odds_death_baseline*OR_death_preg)/(1+(odds_death_baseline*OR_death_preg)))


### Offset CFR in non-pregnant women, by accounting for anticipated number of deaths in pregnant women
### and recovering overall CFR after reducing non-pregnant CFR
### using pregnancy data from Ondo (setting of LASCOPE)
df_CFR_finalAges_scaleNotPreg = df_CFR_finalAges_pregOnly%>%
  left_join(., df_PropPregnant%>%
              filter(Region == "Ondo", Sex == "Female"),
            by = "ag")%>%
  mutate(P_Hosp_Female_Preg = PropPregnant * duration_pregnant,
         P_Hosp_Female_NotPreg = 1 - P_Hosp_Female_Preg)%>%
  mutate(P_Deaths_Female_Preg = P_Hosp_Female_Preg*CFR_preg,
         P_Deaths_Female_NotPreg = CFR - P_Deaths_Female_Preg)%>%
  mutate(CFR_notpreg = P_Deaths_Female_NotPreg/P_Hosp_Female_NotPreg)%>%
  dplyr::select(-c(Case, Death, OR_death_preg, odds_death_baseline, 
                   P_Hosp_Female_Preg, P_Hosp_Female_NotPreg, 
                   P_Deaths_Female_Preg, P_Deaths_Female_NotPreg))


### Oragnise data for male, female and pregnant female
df_CFR_finalAges_Male = df_CFR_finalAges_scaleNotPreg%>%
  mutate(Sex = "Male", 
         prob_death_str = CFR)%>%
  dplyr::select(-c(CFR, CFR_preg, CFR_notpreg))

df_CFR_finalAges_Female = df_CFR_finalAges_scaleNotPreg%>%
  mutate(Sex = "Female_NotPreg", 
         prob_death_str = CFR_notpreg)%>%
  dplyr::select(-c(CFR, CFR_preg, CFR_notpreg))
  
df_CFR_finalAges_FemalePreg = df_CFR_finalAges_scaleNotPreg%>%
  mutate(Sex = "Female_Preg", 
         prob_death_str = CFR_preg)%>%
  dplyr::select(-c(CFR, CFR_preg, CFR_notpreg))


df_CFR_finalAges_preg = bind_rows(df_CFR_finalAges_Male, 
                                  df_CFR_finalAges_Female, 
                                  df_CFR_finalAges_FemalePreg)

### Save data
# write.csv(df_CFR_finalAges_preg, "df_CFR_finalAges_preg.csv")


### Plot CFRs for WCBA vs. PW
p_CFR_WCBA_vs_PW = df_CFR_finalAges_preg%>%
  
  filter(ag %in% age_groups_final[3:5])%>%
  mutate(Sex = factor(Sex, levels = sex_preg_levels, labels = sex_preg_labels))%>%
  ggplot(., aes(x = ag, y = prob_death_str, colour = Sex, fill = Sex))+
  geom_hline(yintercept = 0, colour = "grey")+
  geom_boxplot(alpha = 0.1, outlier.shape = NA, position = position_dodge())+
  geom_jitter(size = 0.3, alpha = 0.2, position = position_jitterdodge(jitter.height = 0, jitter.width = 0.4))+
  theme_bw()+
  xlab("Age group")+ylab("Case-fatality risk")+
  scale_colour_manual("", values = sex_preg_cols_map)+
  scale_fill_manual("", values = sex_preg_cols_map)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(p_CFR_WCBA_vs_PW, file = "p_CFR_WCBA_vs_PW.png", width = 14, height = 10, units = "cm")

