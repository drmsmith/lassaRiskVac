###########################################
### CLINICAL PARAMETERs GENERATION FILE ###
###########################################
### This file generates the parameter distributions used as inputs that are
### varied in Monte Carlo simulations in the health-economic model

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


#########################################
### NUMBER OF MONTE CARLO SIMULATIONS ###
#########################################
### specification of the number of simulations run
### (and hence size of parameter vectors generated for Monte Carlo model)

n_draws_montecarlo = 500


###############################
### PROBABILITY OF SYMPTOMS ###
###############################

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

##################################################
### PROBABILITY OF SEEKING CARE GIVEN SYMPTOMS ###
##################################################

# estimates from: https://link.springer.com/article/10.1186/s12936-015-1048-x

### any treatment
p_treatment_any_mean = 0.5979
p_treatment_any_sd = 0.02755102

### government treatment
p_treatment_gvt_mean = 0.4891
p_treatment_gvt_sd = 0.02806122

### Check values
# qnorm(c(0.025, 0.975), p_treatment_any_mean, p_treatment_any_sd)
# qnorm(c(0.025, 0.975), p_treatment_gvt_mean, p_treatment_gvt_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_prob_treat_comm_any = rnorm(n_draws_montecarlo, p_treatment_any_mean, p_treatment_any_sd)
parvec_prob_treat_comm_gvt = rnorm(n_draws_montecarlo, p_treatment_gvt_mean, p_treatment_gvt_sd)

################################################## 
### PROBABILITIES OF HOSPITALIZATION AND DEATH ###
##################################################

### NB: age- and sex- specific probabilities are calculated in Outcomes.R

### Load in aggregate population-level estimates from Smith et al.

### PROB HOSP
set.seed(20241002)

p_hosp_mean = 0.008708567
p_hosp_sd = 0.001486631
parvec_prob_hosp = rnorm(n_draws_montecarlo, p_hosp_mean, p_hosp_sd)


### PROB DEATH
set.seed(20241002)

p_death_alpha = 6.236571
p_death_beta = 32.50682

parvec_prob_death = rbeta(n_draws_montecarlo, p_death_alpha, p_death_beta)

##############
### p_snhl ###
##############

### Cummins et al.: Prospective audiometric evaluation of 69 hospitalised febrile patients in Sierra Leone
parvec_p_snhl = rbinom(n_draws_montecarlo, 49, 14/49)/49

##################
### FETAL LOSS ###
##################

############################
### FETAL LOSS: BASELINE ###
############################

### Use UN estimates of stillbirth for W Africa in 2019
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8417352/
p_fl_base_mean = 22.8/1000
p_fl_base_l = 19.8/1000
p_fl_base_u = 27.7/1000

# estimates from logarithmic scale so transform to simulate random draws
p_fl_base_sd_log = ((log(p_fl_base_mean) - log(p_fl_base_l))/1.96 + (log(p_fl_base_u) - log(p_fl_base_mean))/1.96)/2
set.seed(20241005)

parvec_p_fl_base=exp(rnorm(n_draws_montecarlo, log(p_fl_base_mean), p_fl_base_sd_log))


#########################
### FETAL LOSS: LASSA ###
#########################

### KAYEM data represent fetal loss (all deaths occurring before or during death and delivery)
### RERUN META-ANALYSIS FROM KAYEM USING DATA FROM DUVIGNAUD 2021
# in lascope, of 14 documented pregnancy outcomes, there were 6 spontaneous miscarriages and 1 intrauterine death

n_monson = 18
N_monson = 20
n_okogbenin = 19
N_okogbenin = 31
n_frame = 2
N_frame = 32
n_keane = 18
N_keane = 27
n_price = 47
N_price = 56
n_duvignaud = 7
N_duvignaud = 14

### prepare raw data
df_meta_p_fl = data.frame(
  study = c("Monson M.H. et al. 1987", 
            "Okogbenin S.A. et al. 2019", 
            "Frame J.D. 1989", 
            "Keane E. & Giles H.M. 1977", 
            "Price M. et al. 1988", 
            "Duvignaud A. et al. 2021"),
  n = c(n_monson, n_okogbenin, n_frame, n_keane, n_price, n_duvignaud),
  N = c(N_monson, N_okogbenin, N_frame, N_keane, N_price, N_duvignaud)
)%>%
  mutate(p = n/N)

library(meta)
meta_p_fl_res <- metaprop(
  event = n,    # Number of events
  n = N,             # Sample sizes
  data = df_meta_p_fl,       # Dataset
  sm = "PLOGIT",        # Freeman-Tukey transformation
  method = "Inverse",   # Random-effects model
  studlab = study    # Study labels
)

summary_meta_res_p_fl = summary(meta_p_fl_res)

if(render_plots){
  forest(meta_p_fl_res, xlab = "Proportion", leftlabs = c("Study", "Events", "Total"))
}

### Extract pooled proportion and 9%% CI
logit_pooled_prop <- meta_p_fl_res$TE.random
logit_ci_lower <- meta_p_fl_res$lower.random
logit_ci_upper <- meta_p_fl_res$upper.random

# Back-transform from logit scale to proportion scale using the inverse logit function
p_fl_total_mean <- 1 / (1 + exp(-logit_pooled_prop))
p_fl_total_se <- meta_p_fl_res$seTE.random
p_fl_total_variance <- p_fl_total_se*p_fl_total_se
p_fl_total_l <- 1 / (1 + exp(-logit_ci_lower))
p_fl_total_u <- 1 / (1 + exp(-logit_ci_upper))

### UNIFORM
parvec_p_fl_total_unif = runif(n_draws_montecarlo, p_fl_total_l, p_fl_total_u)

### METHOD OF MOMENTS
p_fl_total_std_dev <- (p_fl_total_u - p_fl_total_l) / (2 * 1.96)
p_fl_total_variance_mom <- p_fl_total_std_dev^2

# Method of moments estimates for alpha and beta
p_fl_total_alpha_mom <- p_fl_total_mean * ((p_fl_total_mean * (1 - p_fl_total_mean) / p_fl_total_variance_mom) - 1)
p_fl_total_beta_mom <- (1 - p_fl_total_mean) * ((p_fl_total_mean * (1 - p_fl_total_mean) / p_fl_total_variance_mom) - 1)


# Draw samples from the Beta distribution
parvec_p_fl_total_beta <- rbeta(n_draws_montecarlo, shape1 = p_fl_total_alpha_mom, shape2 = p_fl_total_beta_mom)

# Check quantiles to see how well they align
quantile(parvec_p_fl_total_beta, probs=c(0.025, 0.5, 0.975))





##############################
### FETAL LOSS: FINAL PARS ###
##############################

### Difference between baseline fetal loss and post-Lassa fetal loss

parvec_p_fl_lassa = parvec_p_fl_total_beta - parvec_p_fl_base


######################
### NEONATAL DEATH ###
######################

################################
### NEONATAL DEATH: BASELINE ###
################################

### Use UN report estimates from Nigeria in 2020
### 49.7 (44.5 -- 55.1)
# https://data.unicef.org/resources/levels-and-trends-in-child-mortality-2020/#:~:text=The%20report%20finds%20that%20while,five%20deaths%20will%20be%20newborns.

p_nnd_base_mean = 49.7/1000
p_nnd_base_l = 44.5/1000
p_nnd_base_u = 55.1/1000

# estimates from logarithmic scale so transform to simulate random draws
p_nnd_base_sd_log = ((log(p_nnd_base_mean) - log(p_nnd_base_l))/1.96 + (log(p_nnd_base_u) - log(p_nnd_base_mean))/1.96)/2
set.seed(20241005)

parvec_p_nnd_base=exp(rnorm(n_draws_montecarlo, log(p_nnd_base_mean), p_nnd_base_sd_log))

#############################
### NEONATAL DEATH: LASSA ###
#############################

### KAYEM data represent neonatal death (all deaths occurring from delivery to 1 month of life)
### including deaths occurring from a few hours after birth to 18 days (10 out of 31 across 3 studies)
### RERUN META-ANALYSIS FROM KAYEM INCLUDING DATA FROM DUVIGNAUD 2024
### in paediatric lascope, of 7 documented neonatal cases (<1 month), there were 3 deaths

n_price = 7
N_price = 14
n_okogbenin = 1
N_okogbenin = 12
n_keane = 2
N_keane = 5
n_duvignaud = 3
N_duvignaud = 7

### prepare raw data
df_meta_p_nnd = data.frame(
  study = c("Price M.E. et al. 1988", 
            "Okogbenin S.A. et al. 2019",
            "Keane E. & Giles H.M. 1977",
            "Duvignaud A. et al. 2024"),
  n = c(n_price, n_okogbenin, n_keane, n_duvignaud),
  N = c(N_price, N_okogbenin, N_keane, N_duvignaud)
)%>%
  mutate(p = n/N)

library(meta)
meta_p_nnd_res <- metaprop(
  event = n,    # Number of events
  n = N,             # Sample sizes
  data = df_meta_p_nnd,       # Dataset
  sm = "PLOGIT",        # Freeman-Tukey transformation
  method = "Inverse",   # Random-effects model
  studlab = study    # Study labels
)

summary_meta_res_p_nnd = summary(meta_p_nnd_res)

if(render_plots){
  forest(meta_p_nnd_res, xlab = "Proportion", leftlabs = c("Study", "Events", "Total"))
}


### Extract pooled proportion and 9%% CI
logit_pooled_prop <- meta_p_nnd_res$TE.random
logit_ci_lower <- meta_p_nnd_res$lower.random
logit_ci_upper <- meta_p_nnd_res$upper.random

# Back-transform from logit scale to proportion scale using the inverse logit function
p_nnd_total_mean <- 1 / (1 + exp(-logit_pooled_prop))
p_nnd_total_se <- meta_p_nnd_res$seTE.random
p_nnd_total_variance <- p_nnd_total_se*p_nnd_total_se
p_nnd_total_l <- 1 / (1 + exp(-logit_ci_lower))
p_nnd_total_u <- 1 / (1 + exp(-logit_ci_upper))

### UNIFORM
parvec_p_nnd_total_unif = runif(n_draws_montecarlo, p_nnd_total_l, p_nnd_total_u)

### METHOD OF MOMENTS
p_nnd_total_std_dev <- (p_nnd_total_u - p_nnd_total_l) / (2 * 1.96)
p_nnd_total_variance_mom <- p_nnd_total_std_dev^2

# Method of moments estimates for alpha and beta
p_nnd_total_alpha_mom <- p_nnd_total_mean * ((p_nnd_total_mean * (1 - p_nnd_total_mean) / p_nnd_total_variance_mom) - 1)
p_nnd_total_beta_mom <- (1 - p_nnd_total_mean) * ((p_nnd_total_mean * (1 - p_nnd_total_mean) / p_nnd_total_variance_mom) - 1)


# Draw samples from the Beta distribution
parvec_p_nnd_total_beta <- rbeta(n_draws_montecarlo, shape1 = p_nnd_total_alpha_mom, shape2 = p_nnd_total_beta_mom)

# Check quantiles to see how well they align
quantile(parvec_p_nnd_total_beta, probs=c(0.025, 0.5, 0.975))*100

##################################
### NEONATAL DEATH: FINAL PARS ###
##################################

parvec_p_nnd_lassa = parvec_p_nnd_total_beta - parvec_p_nnd_base

#################
### DURATIONS ###
#################

#########################
### DURATION OF FEVER ###
#########################

# From: https://www.sciencedirect.com/science/article/pii/S1201971220324905?via%3Dihub

d_fever_mean = 3.53
d_fever_sd = 1.99/sqrt(261)

### Check values
# qnorm(c(0.025,0.975), d_fever_mean, d_fever_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_dur_fever = rnorm(n_draws_montecarlo, d_fever_mean, d_fever_sd)

################################
### DURATION ILL PRE-HOSPITAL###
################################

### these and following duration estimates from the LASCOPE study
# https://www.sciencedirect.com/science/article/pii/S2214109X20305180?via%3Dihub

d_ill_prehosp_mean = 9.333333 
d_ill_prehosp_SE = 0.1969521 

## Check values
# qnorm(c(0.025,0.975), d_ill_prehosp_mean, d_ill_prehosp_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_dur_ill_prehosp = rnorm(n_draws_montecarlo, d_ill_prehosp_mean, d_ill_prehosp_SE)

############################
### DURATION HOSP (DIED) ###
############################

d_hosp_died_mean = 3.333333 
d_hosp_died_SE = 0.4745692 

### Check values
# qnorm(c(0.025,0.975), d_hosp_died_mean, d_hosp_died_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_dur_hosp_died = rnorm(n_draws_montecarlo, d_hosp_died_mean, d_hosp_died_SE)

################################
### DURATION HOSP (SURVIVED) ###
################################

d_hosp_survived_mean = 12
d_hosp_survived_SE = 0.1749207 

### Check values
# qnorm(c(0.025,0.975), d_hosp_survived_mean, d_hosp_survived_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_dur_hosp_survived = rnorm(n_draws_montecarlo, d_hosp_survived_mean, d_hosp_survived_SE)


#################################
### CHARACTERIZE HEARING LOSS ###
#################################

df_snhl = read_excel("parameters_data/review_hearing_loss.xlsx", sheet = 3)

### Include only better ear and measurements with follow-up
df_snhl_analysis = df_snhl%>%
  dplyr::select(-c(R, R_cat, L, L_cat, Worse))%>%
  dplyr::filter(FU == 1)

### Mean change
df_snhl_change = df_snhl_analysis%>%
  mutate(patient = factor(patient),
         Better = as.numeric(as.character(Better)))%>%
  group_by(patient)%>%
  mutate(Change = lead(Better) - Better)%>%
  dplyr::filter(measure == "P")

### Extract key points for presentation (p), delta/change (d) and follow-up (fu)
snhl_p = mean(df_snhl_change$Better)
snhl_d = mean(df_snhl_change$Change)
snhl_fu = snhl_p+snhl_d

### Time points for presentation and follow-up
snhl_t_p = 0
snhl_t_fu = 1

### for decay rate y = A exp (-k x)
# where A = initial value, k is decay rate

snhl_k = -log(snhl_fu/snhl_p)/(snhl_t_fu-snhl_t_p)

### Derive when snhl_x < 20 to define mean duration of SNHL
### (MAT = 20 is the threshold of hearing loss)
snhl_dur = log(20/snhl_p)/-snhl_k

### Exponential PDF
f_snhl_decay_pdf <- function(x, snhl_p, snhl_k) {
  snhl_p * exp(-snhl_k * x)
}

f_snhl_decay_pdf(seq(0,2,by=0.1), snhl_p, snhl_k)


######################################################################
### DO THIS IN BOOTSTRAP FRAMEWORK TO RESAMPLE FROM AVAILABLE DATA ###
######################################################################

set.seed(20241031)

vec_rows = 1:nrow(df_snhl_change)

n_boots = 10000

list_boot_snhl = list()
list_boot_snhl_fits = list()

for(i in 1:n_boots){
  print(paste0("on bootstrap iteration ", i))
  
  vec_rows_boot = sample(vec_rows, size = nrow(df_snhl_change), replace = T)
  df_boot = df_snhl_change[vec_rows_boot,]
  
  ### Extract key points for presentation (p), delta/cahnge (d) and follow-up (fu)
  boot_p = mean(df_boot$Better)
  boot_d = mean(df_boot$Change)
  boot_fu = boot_p+boot_d
  
  ### Time points for pesentation and follow-up
  boot_t_p = 0
  boot_t_fu = 1
  
  ### for decay rate y = A exp (-k x)
  # where A = initial value, k is decay rate
  
  boot_k = -log(boot_fu/boot_p)/(boot_t_fu-boot_t_p)
  
  ### Derive when snhl_x < 20 to define mean duration of SNHL
  boot_dur = log(20/boot_p)/-boot_k
  
  ### PDF
  vec_horizon_boot = seq(0,10,by=0.1)
  vec_fit_boot = f_snhl_decay_pdf(vec_horizon_boot, boot_p, boot_k)
  
  list_boot_snhl[[i]] =data.frame(iter = i,
                        boot_p = boot_p, 
                        boot_d = boot_d, 
                        boot_fu = boot_fu, 
                        boot_k = boot_k, 
                        boot_dur = boot_dur)
  
  list_boot_snhl_fits[[i]] = data.frame(iter = i,
                                        t = vec_horizon_boot,
                                        mat = vec_fit_boot)
}

df_snhl_boot = do.call(rbind, list_boot_snhl)
df_snhl_fits_boot = do.call(rbind, list_boot_snhl_fits)

### Calculate bounds of distribution carried forward
snhl_dur_boot_lower = quantile(df_snhl_boot$boot_dur, 0.025)
snhl_dur_boot_upper = quantile(df_snhl_boot$boot_dur, 0.975)

### Final distribution:
df_snhl_dur_final = df_snhl_boot%>%filter(boot_dur >= snhl_dur_boot_lower & boot_dur <= snhl_dur_boot_upper)
df_snhl_fits_final = df_snhl_fits_boot%>%filter(boot_dur >= snhl_dur_boot_lower & boot_dur <= snhl_dur_boot_upper)

print(paste0("mean duration: ", round(mean(df_snhl_dur_final$boot_dur),2), " (95% CI:",
             round(min(df_snhl_dur_final$boot_dur),2), " - ", 
             round(max(df_snhl_dur_final$boot_dur),2), ")"))

#############################################
### FINAL VECTOR OF HEARING LOSS DURATION ###
#############################################

set.seed(20241102)
parvec_dur_snhl = sample(df_snhl_dur_final$boot_dur, n_draws_montecarlo)

### and future hearing loss discounted 
discRate = 0.035

# discounted duration of snhl if >1 year
parvec_dur_snhl_disc = ((1/discRate)*(1-exp(-discRate*parvec_dur_snhl)))


#############
### DALYs ###
#############

########################
### Fever disutility ###
########################

daly_fever_mean = 0.051
daly_fever_sd = 0.01071429

### Check values
# qnorm(c(0.025,0.975), daly_fever_mean, daly_fever_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_disutility_fever = rnorm(n_draws_montecarlo, daly_fever_mean, daly_fever_sd)


##################################
### Hospitalization disutility ###
##################################

### Load Excel file with estimated number of patients with each symptom, and extract rows with associated disability
# NB: hearing loss excluded as considered as chronic sequelae
df_daly_hospital_raw = read.csv("parameters_data/DALYs_weighting_v3_summarized.csv") %>%
  mutate(baseline_p = baseline_n/baseline_N,
         post_p = post_n/post_N)

vec_daly_overall_max = c()

n_samples = 100000

set.seed(20241002)
for (x in 1:n_samples) {
  if (x%%1000 == 0) { print(x) }
  
  ### BASELINE
  # which symptoms does each individual have at baseline
  vec_symptoms_baseline = rbinom(n = nrow(df_daly_hospital_raw), 
                                 size = 1,
                                 prob = df_daly_hospital_raw$baseline_p)
  
  # max baseline disutility; if no symptoms, take as zero
  if (sum(vec_symptoms_baseline) == 0) {
    daly_baseline_max = 0
  } else {
    daly_baseline_max = max(df_daly_hospital_raw$disutility[which(vec_symptoms_baseline == 1)])
  }
  
  ### POST BASELINE
  # which symptoms does each individual have post-baseline
  vec_symptoms_post = rbinom(n = nrow(df_daly_hospital_raw), 
                             size = 1,
                             prob = df_daly_hospital_raw$post_p)
  
  # max post-baseline disutility; if no symptoms, take as zero
  if (sum(vec_symptoms_baseline) == 0) {
    daly_post_max = 0
  } else {
    daly_post_max = max(df_daly_hospital_raw$disutility[which(vec_symptoms_post == 1)])
  }
  
  ### OVERALL ACROSS POST AND BASELINE
  daly_overall_max = (daly_baseline_max + daly_post_max) / 2
  
  ### SAVE OUTPUTS TO VECTOR
  vec_daly_overall_max[x] = daly_overall_max
}

df_dalys_hospitalization = data.frame(timepoint = "overall", method = "max", DALY = vec_daly_overall_max)

df_dalys_hospitalization %>% group_by(timepoint, method) %>%
  summarise(lower = quantile(DALY, 0.025),
            mean = mean(DALY),
            upper = quantile(DALY, 0.975))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20241002)
parvec_disutility_hospital = sample(vec_daly_overall_max, n_draws_montecarlo)



###############################
### HEARING LOSS DISUTILITY ###
###############################

### Calculate based on MAT at baseline, and draw random value from associated distribution

### GBD daly weights associated with hearing loss, prioritise estimate with ringing;
# https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)00516-X/fulltext#app-1

### Cummins et al.: Tinnitus in 14/17 Lassa fever patients with deafness (82.3%)
### Ficenec et al.: Tinnitus in 13/47 LF patients (compared to 8/47 with hearing loss)

# mild with ringing
daly_hearingloss_mild_mean = 0.021
daly_hearingloss_mild_lower = 0.012
daly_hearingloss_mild_upper = 0.036
daly_hearingloss_mild_sd = c((daly_hearingloss_mild_mean - daly_hearingloss_mild_lower)/1.96 + (daly_hearingloss_mild_upper - daly_hearingloss_mild_mean)/1.96)/2

# moderate with ringing
daly_hearingloss_moderate_mean = 0.074
daly_hearingloss_moderate_lower = 0.048
daly_hearingloss_moderate_upper = 0.107
daly_hearingloss_moderate_sd = c((daly_hearingloss_moderate_mean - daly_hearingloss_moderate_lower)/1.96 + (daly_hearingloss_moderate_upper - daly_hearingloss_moderate_mean)/1.96)/2

# moderately severe with ringing
daly_hearingloss_moderatesevere_mean = 0.167
daly_hearingloss_moderatesevere_lower = 0.114
daly_hearingloss_moderatesevere_upper = 0.231
daly_hearingloss_moderatesevere_sd = c((daly_hearingloss_moderatesevere_mean - daly_hearingloss_moderatesevere_lower)/1.96 + (daly_hearingloss_moderatesevere_upper - daly_hearingloss_moderatesevere_mean)/1.96)/2

# severe with ringing
daly_hearingloss_severe_mean = 0.261
daly_hearingloss_severe_lower = 0.174
daly_hearingloss_severe_upper = 0.361
daly_hearingloss_severe_sd = c((daly_hearingloss_severe_mean - daly_hearingloss_severe_lower)/1.96 + (daly_hearingloss_severe_upper - daly_hearingloss_severe_mean)/1.96)/2

# profound with ringing
daly_hearingloss_profound_mean = 0.277
daly_hearingloss_profound_lower = 0.182
daly_hearingloss_profound_upper = 0.388
daly_hearingloss_profound_sd =  c((daly_hearingloss_profound_mean - daly_hearingloss_profound_lower)/1.96 + (daly_hearingloss_profound_upper - daly_hearingloss_profound_mean)/1.96)/2

# complete with ringing
daly_hearingloss_complete_mean = 0.316
daly_hearingloss_complete_lower = 0.211
daly_hearingloss_complete_upper = 0.436
daly_hearingloss_complete_sd =  c((daly_hearingloss_complete_mean - daly_hearingloss_complete_lower)/1.96 + (daly_hearingloss_complete_upper - daly_hearingloss_complete_mean)/1.96)/2

### function to randomly draw a hearing loss disutility value based on audiometry (MAT)
### and corresponding GBD classifications:
f_snhl_disutility = function(mat, distr){
  
  if(distr == "normal"){
    # mat 20-34: mild
    if(mat < 35){sev = "Mild"; snhl_dis = rnorm(1, daly_hearingloss_mild_mean, daly_hearingloss_mild_sd)}
    
    # mat 35-49: moderate
    if(mat >= 35 & mat < 50){sev = "Moderate"; snhl_dis = rnorm(1, daly_hearingloss_moderate_mean, daly_hearingloss_moderate_sd)}
    
    # mat 50-64: moderately severe
    if(mat >= 50 & mat < 65){sev = "Moderately severe"; snhl_dis = rnorm(1, daly_hearingloss_moderatesevere_mean, daly_hearingloss_moderatesevere_sd)}
    
    # mat 65-79: severe
    if(mat >= 65 & mat < 80){sev = "Severe"; snhl_dis = rnorm(1, daly_hearingloss_severe_mean, daly_hearingloss_severe_sd)}
    
    # mat 80-94: profound
    if(mat >= 80 & mat < 95){sev = "Profound"; snhl_dis = rnorm(1, daly_hearingloss_profound_mean, daly_hearingloss_profound_sd)}
    
    # mat 95+: complete
    if(mat >= 95){sev = "Complete"; snhl_dis = rnorm(1, daly_hearingloss_complete_mean, daly_hearingloss_complete_sd)}
  }
  
  if(distr == "uniform"){
    # mat 20-34: mild
    if(mat < 35){sev = "Mild"; snhl_dis = runif(1, daly_hearingloss_mild_lower, daly_hearingloss_mild_upper)}
    
    # mat 35-49: moderate
    if(mat >= 35 & mat < 50){sev = "Moderate"; snhl_dis = runif(1, daly_hearingloss_moderate_lower, daly_hearingloss_moderate_upper)}
    
    # mat 50-64: moderately severe
    if(mat >= 50 & mat < 65){sev = "Moderately severe"; snhl_dis = runif(1, daly_hearingloss_moderatesevere_lower, daly_hearingloss_moderatesevere_upper)}
    
    # mat 65-79: severe
    if(mat >= 65 & mat < 80){sev = "Severe"; snhl_dis = runif(1, daly_hearingloss_severe_lower, daly_hearingloss_severe_upper)}
    
    # mat 80-94: profound
    if(mat >= 80 & mat < 95){sev = "Profound"; snhl_dis = runif(1, daly_hearingloss_profound_lower, daly_hearingloss_profound_upper)}
    
    # mat 95+: complete
    if(mat >= 95){sev = "Complete"; snhl_dis = runif(1, daly_hearingloss_complete_lower, daly_hearingloss_complete_upper)}
  }
  
  
  return(c("Severity" = sev, "Disutility" = snhl_dis))
}

##############################################################################################
### Calculate disutility using GBD distributions from final bootstrapped SNHL distribution ###
##############################################################################################

set.seed(20241031)

df_snhl_dis_gbd = data.frame(do.call(rbind, 
                                     lapply(df_snhl_dur_final$boot_p, f_snhl_disutility, distr = "normal")))%>%
  mutate(Disutility = as.numeric(as.character(Disutility)))
df_snhl_dis_gbd$iter = df_snhl_dur_final$iter
df_snhl_dis_gbd$mat = df_snhl_dur_final$boot_p

####################################
### FIT GAM OF DISUTILITY TO MAT ###
####################################

### Gamma link function for continuous positive response variable
gam_disutility = gam(Disutility ~ s(mat, k = 3), family = Gamma(link = "log"), data = df_snhl_dis_gbd,
                     method = "REML")

df_disutility_predict = data.frame(mat = seq(20, 95, by = 0.1))
row.names(df_disutility_predict) <- NULL

predict_disutility = predict(gam_disutility, newdata = df_disutility_predict, type = "response", se.fit = TRUE)

df_disutility_predict$Disutility = predict_disutility$fit
df_disutility_predict$lower = predict_disutility$fit - predict_disutility$se.fit*1.96
df_disutility_predict$upper = predict_disutility$fit + predict_disutility$se.fit*1.96

########################################################################
### DRAW FINAL DISUTILITY VALUES AS FUNCTION OF SIMULATED MAT VALUES ###
########################################################################

### predict disutility on the input data for the gam model (i.e. on simulated MAT values)
predict_disutility_final = predict(gam_disutility, type = "response", se.fit = TRUE)

disutility_final_mean = predict_disutility_final$fit
disutility_final_se = predict_disutility_final$se.fit

disutility_final_shape =  (disutility_final_mean/disutility_final_se)^2
disutility_final_rate = disutility_final_shape/disutility_final_mean

disutility_draws_final = rgamma(length(disutility_final_mean),
                                shape = disutility_final_shape,
                                rate = disutility_final_rate)

### create final disutility dataframe with predicted disutilites based on simulated MAT
df_snhl_dis_final = df_snhl_dis_gbd
df_snhl_dis_final$Disutility = disutility_draws_final

##############################################
### SNHL DISUTILITY FINAL PARAMETER VECTOR ###
##############################################

### Calculate bounds of distribution carried forward
snhl_dis_final_lower = quantile(disutility_draws_final, 0.025)
snhl_dis_final_upper = quantile(disutility_draws_final, 0.975)

### Final distribution:
df_snhl_dis_final_vec = df_snhl_dis_final%>%filter(Disutility >= snhl_dis_final_lower & Disutility <= snhl_dis_final_upper)

set.seed(20241102)
parvec_disutility_snhl = sample(df_snhl_dis_final_vec$Disutility, n_draws_montecarlo)

#########################################
### DALYS PER PATIENT FROM DISUTILITY ###
#########################################

parvec_DALYperpatient_fever = parvec_dur_fever * parvec_disutility_fever/365
parvec_DALYperpatient_hosp_survived = (parvec_dur_ill_prehosp + parvec_dur_hosp_survived)*parvec_disutility_hospital/365
parvec_DALYperpatient_hosp_died = (parvec_dur_ill_prehosp + parvec_dur_hosp_died)*parvec_disutility_hospital/365


###############################################
### PLOT SNHL DATA, DURATION AND DISUTILITY ###
###############################################

if(render_plots){
  ### RAW DATA
  p_snhl_raw_data = df_snhl%>%
    dplyr::select(patient, measure, FU, Better, Worse)%>%
    pivot_longer(-c(patient, measure, FU), names_to = "Ear", values_to = "MAT")%>%
    mutate(measure = factor(measure,
                            levels = c("P", "FU"),
                            labels = c("Presentation", "1-year follow-up")),
           MAT = case_when(MAT == "NA" ~ NA,
                           T ~ MAT),
           MAT = as.numeric(MAT),
           patient = factor(patient),
           `Follow-up` = factor(FU, levels = c(0, 1), labels = c("No", "Yes")))%>%
    ggplot(., aes(x = measure, y = MAT, colour = Ear, shape = `Follow-up`, 
                  group = interaction(patient, Ear)))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_point()+
    geom_line()+
    theme_bw()+
    scale_colour_manual(values = c("orange", "lightgrey"))+
    ylab("Mean auditory threshold, dB (observed)") + xlab("Audiometry timing")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_discrete(expand = c(0.2,0))+
    scale_y_continuous(limits = c(0, 125), breaks = c(0, 20, 40, 60, 80, 100, 120))
  
  ### BOOTSTRAPPED DECAY
  
  p_snhl_boot = df_snhl_fits_boot%>%
    filter(iter <= 200)%>%
    ggplot(., aes(x = t, y = mat, group = iter))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_line(alpha = 0.1)+
    theme_bw()+
    ylab("Mean auditory threshold, dB (predicted mean)") + xlab("")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_hline(yintercept = 20, colour = "red", linetype = 1)+
    scale_x_continuous(breaks = c(0,2,4,6,8,10))+xlab("Years since presentation")+
    scale_y_continuous(limits = c(0, 125), breaks = c(0, 20, 40, 60, 80, 100, 120))
  
  ### COMBINE RAW DATA AND BOOTSTRAPPED DECAY FITS
  p_snhl_obs_pred = plot_grid(p_snhl_raw_data, p_snhl_boot, ncol = 2, labels = c("A", "B"), rel_widths = c(0.8, 1))
  
  ggsave(p_snhl_obs_pred, file = "p_snhl_obs_pred.png", width = 18, height = 16, units = "cm")
  
  ### DURATION DISTRIBUTION
  p_snhl_dur_pred = df_snhl_dur_final%>%
    ggplot(., aes(x = boot_dur))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_histogram(bins = 25, colour = "black", fill = "orange")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Mean duration of hearing loss (years)")+ylab("Count")+xlim(0,NA)+
    scale_y_continuous(labels = comma)
  
  ggsave(p_snhl_dur_pred, file = "p_snhl_dur_pred.png", width = 12, height = 8, units = "cm")
  
  ### GBD DISUTILITY DISTRIBUTION ###
  ### Simulated disutility
  p_snhl_dis_gbd_distr =  df_snhl_dis_gbd%>%
    mutate(Severity = factor(Severity, levels = c("Mild", "Moderate", "Moderately severe", "Severe", "Profound")))%>%
    ggplot(., aes(x = Disutility, fill = Severity))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_histogram(bins = 25, colour = "black", position = "stack")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Hearing loss disutility")+ylab("Count")+xlim(0,NA)+
    scale_y_continuous(labels = comma)
  
  ### DISUTILITY RELATIONSHIP TO AUDITORY THRESHOLD, WITH GAM FIT
  p_snhl_dis_gbd_mat_scatter_gam = df_snhl_dis_gbd%>%
    mutate(Severity = factor(Severity, levels = c("Mild", "Moderate", "Moderately severe", "Severe", "Profound")))%>%
    ggplot(., aes(x = mat, y = Disutility, colour = Severity))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_point(size = 1, alpha = 0.3)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Mean auditory threshold (dB) at presentation")+ylab("Hearing loss disutility")+
    geom_ribbon(data = df_disutility_predict, mapping = aes(ymin = lower, ymax = upper), colour = "grey", alpha = 0.5)+
    geom_line(data = df_disutility_predict, colour = "black")+
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))
  
  
  ### FINAL DISUTILITY DISTRIBUTION ###
  ### Simulated disutility
  p_snhl_dis_final_distr = df_snhl_dis_final%>%
    mutate(Severity = factor(Severity, levels = c("Mild", "Moderate", "Moderately severe", "Severe", "Profound")))%>%
    ggplot(., aes(x = Disutility, fill = Severity))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_histogram(bins = 25, colour = "black", position = "stack")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Hearing loss disutility")+ylab("Count")+xlim(0,NA)+
    scale_y_continuous(labels = comma)
  
  ### DISUTILITY RELATIONSHIP TO AUDITORY THRESHOLD
  p_snhl_dis_final_mat_scatter = df_snhl_dis_final%>%
    mutate(Severity = factor(Severity, levels = c("Mild", "Moderate", "Moderately severe", "Severe", "Profound")))%>%
    ggplot(., aes(x = mat, y = Disutility, colour = Severity))+
    geom_hline(yintercept = 0, colour = "grey")+
    geom_point(size = 1, alpha = 0.3)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    xlab("Mean auditory threshold (dB) at presentation")+ylab("Hearing loss disutility")
  
  ### COMBINE SIMULATED DISUTILITY FROM MAT SCATTERPLOT WITH FINAL DISTRIBUTION
  p_snhl_scatter_hist = ggarrange(p_snhl_dis_gbd_mat_scatter_gam, p_snhl_dis_final_distr,
                                  nrow = 2, common.legend = T, legend = "right", align = "hv",
                                  labels = c("A", "B"))
  
  ggsave(p_snhl_scatter_hist, file = "p_snhl_scatter_hist.png", width = 16, height = 12, units = "cm")
  
  
}



#################################
### FINAL PARAMETER DATAFRAME ###
#################################



df_params_montecarlo = data.frame(n_draw = 1:n_draws_montecarlo,
                                  prob_symptoms = parvec_prob_symptoms,
                                  prob_treat_comm_any = parvec_prob_treat_comm_any,
                                  prob_treat_comm_gvt = parvec_prob_treat_comm_gvt,
                                  prob_hosp = parvec_prob_hosp,
                                  prob_death = parvec_prob_death,
                                  prob_snhl = parvec_p_snhl,
                                  prob_fl_lassa = parvec_p_fl_lassa,
                                  prob_nnd_lassa = parvec_p_nnd_lassa,
                                  dur_fever = parvec_dur_fever,
                                  dur_ill_prehosp = parvec_dur_ill_prehosp,
                                  dur_hosp_survived = parvec_dur_hosp_survived,
                                  dur_hosp_died = parvec_dur_hosp_died,
                                  dur_snhl = parvec_dur_snhl,
                                  dur_snhl_disc = parvec_dur_snhl_disc,
                                  disutility_fever = parvec_disutility_fever,
                                  disutility_hospital = parvec_disutility_hospital,
                                  disutility_snhl = parvec_disutility_snhl,
                                  DALYperpatient_fever = parvec_DALYperpatient_fever,
                                  DALYperpatient_hosp_survived = parvec_DALYperpatient_hosp_survived,
                                  DALYperpatient_hosp_died = parvec_DALYperpatient_hosp_died)

### Save dat
# save(df_params_montecarlo, file = "params_montecarlo.Rdata")
