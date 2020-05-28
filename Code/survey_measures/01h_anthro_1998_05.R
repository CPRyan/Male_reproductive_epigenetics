###################################################################################################################################
###################################################################################################################################
# Anthropometrics
###################################################################################################################################
here <-here::here()
###################################################################################################################################


# Load dnam individuals 

dnam <-read_csv(here::here("Data/Other", "uncchdid_w_DNAm.csv"))
names(dnam)<-"uncchdid"
# Remove the _R1 and _R2
dnam$uncchdid <-substr(dnam$uncchdid, 1, 5)
dim(dnam)

# LOad the blood draw dates, which has the sex. Merge and keep sex for DNAm

blood.draw.dates <- read_csv(here::here("Data/Other","blood.draw.dates.csv"))[,-1]
blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)


chk.bld.draw <-left_join(dnam, blood.draw.dates, by = "uncchdid") %>% 
  select(-c(dayblood:yearblood))

# Merge only male predicted so I don't lose the mispredictions from EAGE
head(chk.bld.draw)


# Calculations from: https://nutritionalassessment.mumc.nl/en/anthropometry
###################################################################################################################################
# Dates of Survey needed 
survey98_date <-read_dta(here::here("Data/zip_Boys_1998/schoolng.dta")) 

survey98_date <-survey98_date %>%
  mutate(date_interview98 = lubridate::ymd(paste(survey98_date$yrintrvw, survey98_date$mointrvw, survey98_date$dyintrvw, sep = "/"))) %>% 
  as_character(uncchdid) %>% 
  select(uncchdid, date_interview98) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid)
  
######################

# Note that I'm missing suprailiac for 1998, so I used another DW equation.


anthro98 <-read_dta(here::here("Data/zip_Boys_1998/chldiet.dta")) %>% 
  as_character(uncchdid) %>% 
  rename_all(tolower) %>% 
  mutate(whr = waist / hip) %>% 
  mutate(tricep_mean = rowMeans(select(., starts_with("tricep")), na.rm = TRUE)) %>%  # mean of 3 measurements in mm
  mutate(subscap_mean = rowMeans(select(., starts_with("subscap")), na.rm = TRUE)) %>%  # mean of 3 measurements in mm
  mutate(arm_musc_circ = armcircm*10 - (tricep_mean * 3.14)) %>% # Upper arm muscle circ. (S) in mm: S = c - ( T * 3.14) 
  mutate(arm_musc_area = arm_musc_circ^2 / 12.56) %>% # Upper arm muscle area (M) in mm² : M = S² / 12.56
  mutate(bmi = weight/(height/100)^2) %>% 
  select(uncchdid, weight, height, bmi, waist, hip, whr, armcircm, subscap_mean, tricep_mean, arm_musc_area) %>% 
  na_if(., -9) %>% 
  mutate(skinfold_sum = subscap_mean +  tricep_mean) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid)

anthro98 <-left_join(survey98_date, anthro98, by = "uncchdid")

names(anthro98)


# Calculate the density, fat-free mass, etc.
# Constants from Durnin and Womersley, 1974, (Males, Age 17-19 - guys were 13-15, so some error there)
# Linear regressionequationsfor the estimation of body density x 10^3(kg/m^3)
# density = c - m x log skinfold

dw_c <-1.1561
dw_m <-0.0711
# bfperc = ((4.95/density )-4.5)*100
# fatmass = (( bfperc *.01)*weight)
# ffm05 = weight-fatmass

anthro98 <-anthro98 %>% 
  mutate(logfolds = log(skinfold_sum), 
         density = dw_c - (dw_m * logfolds), 
         bfperc = ((4.95/density )-4.5)*100,
         fatmass = (( bfperc *.01)*weight),
         fatfree_mass = weight-fatmass)




anthro_min98 <-anthro98 %>% 
  select(-weight, -armcircm, -waist, -hip, -skinfold_sum, -logfolds, -density) %>% 
  as_character(uncchdid)


anthro98 <-anthro_min98; rm(anthro_min98)


############################################

anthro_time <-left_join(anthro, anthro98, by = "uncchdid", suffix = c("", "98"))

anthro_time <-anthro_time %>% 
  mutate(muscle_accretion = height - height98,
         height_accretion = fatfree_mass - fatfree_mass98, 
         arm_accretion = arm_musc_area - arm_musc_area98, 
         fat_accretion = fatmass - fatmass98, 
         time_bt_98_05 = blood.draw.date - date_interview98)



############################################

anthro_time_stacked <-anthro_time %>% 
  gather(key = "key", value = "value", c(height98, height, fatfree_mass98, fatfree_mass, arm_musc_area98, arm_musc_area)) %>% 
  mutate(facetz = if_else(str_detect(key, "height"), "height", 
                          if_else(str_detect(key, "musc"), "arm_muscle", 
                                  "fatfree_mass"))) %>%
  mutate(date = if_else(str_detect(key, "98"), date_interview98, blood.draw.date)) %>% 
  select(uncchdid, key, value, facetz, date)


  
anthro_time_stacked %>% 
  ggplot(., aes(x = reorder(key, desc(key)), y = value, group = uncchdid, color = facetz))+
  geom_boxplot(aes(group = key), color = "gray", alpha = 0.1)+
  geom_point(alpha = 0.3)+
  geom_line(alpha = 0.2)+
  facet_wrap(~facetz, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  ggpubr::theme_pubr()


anthro_time_stacked  %>% 
  ggplot(., aes(x = date, y = value, group = uncchdid, color = facetz))+
  geom_boxplot(aes(group = key), color = "gray", alpha = 0.1)+
  geom_point(alpha = 0.5)+
  geom_line(alpha = 0.5)+
  facet_wrap(~facetz, scales = "free")+
  scale_color_brewer(palette = "Set1")+
  ggpubr::theme_pubr()
