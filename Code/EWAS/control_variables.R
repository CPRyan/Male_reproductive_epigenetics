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


###################
# date_interview05

date_interview05 <- read_dta(here::here("Data/zip_child_2005/screen.dta")) 

date_interview05 <-date_interview05 %>%   
  rename_all(tolower) %>% 
  mutate(date_interview05 = lubridate::ymd(paste(date_interview05$yrntrvw, date_interview05$montrvw, date_interview05$dayntrvw, sep = "/"))) %>% 
  select(uncchdid, date_interview05)

###################


###################
# DOB - 
# Pull off basebrgy and basewman from an anthro file or similar
unc_baseinfo <-read_dta(here::here("Data/zip_child_2005/anthdiet.DTA")) %>% 
  select(uncchdid, basebrgy, basewman)

# Find mom in original 1983 records using basebrgy and basewman
bday83 <-read_dta("Data/zip_mother_1983_86/mpreg.dta") %>% 
  rename_all(tolower)

# Find baby bday
bday83 <-bday83 %>% 
  mutate(ic_dob = lubridate::ymd(paste(bday83$yrbir121, bday83$mobir121, bday83$dybir121, sep = "/"))) %>% 
  select(basebrgy, basewman, ic_dob) 

# merge with uncchdid
bday83 <-left_join(unc_baseinfo, bday83, by = c("basebrgy", "basewman")) %>% 
  as_character(uncchdid) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) 




###################
control_vars <-left_join(health_vars, genetic_vars, by = "uncchdid")
control_vars <-left_join(control_vars, bmi_vars, by = "uncchdid")
control_vars <-left_join(control_vars, ses_vars, by = "uncchdid")
control_vars <-left_join(bday83, control_vars, by = "uncchdid")

# Check dates
left_join(control_vars %>% 
  mutate(date_blood = ic_dob+age_blood05) %>% 
    select(date_blood, uncchdid), 
  chk.bld.draw, by = "uncchdid") %>% filter(date_blood!=blood.draw.date)
# The dates are correct for all but one guy. There is a 20 day difference, not likely enough to make a difference. I'm not going to hunt this down beyond this. 


write_csv(control_vars, here::here("Data/Other", "control_vars.csv"))
