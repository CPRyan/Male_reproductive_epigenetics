###################
# Health
source(here::here("/Code/survey_measures", "01d_health.R"))
names(health)

health_vars <-health %>% select(uncchdid, smoke, drink)

###################
# Genetics
source(here::here("/Code/survey_measures", "01e_testost.R"))
names(testost)

genetic_vars <-testost %>% select(uncchdid, age_blood05, icpc1:icpc10)

###################
# BMI
source(here::here("/Code/survey_measures", "01a_anthro.R"))

bmi_vars <-anthro %>% select(uncchdid, bmi)



###################
# SES
ses <-read_csv(here::here("Data/Other", "SEAvars_males_sum83_05.csv")) %>% 
  filter(female == "0") %>% 
  as_character(uncchdid)

ses_vars <-ses %>% select(uncchdid, SEAsum_83_05)


###################
control_vars <-left_join(health_vars, genetic_vars, by = "uncchdid")
control_vars <-left_join(control_vars, bmi_vars, by = "uncchdid")
control_vars <-left_join(control_vars, ses_vars, by = "uncchdid")


write_csv(control_vars, here::here("Data/Other", "control_vars.csv"))
