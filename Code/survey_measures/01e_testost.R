# OTher
library(lubridate)
################################
# Functions:
################################

# Function to turn times (645, 1120) into times (6:45)
turn_to_time <- function(x) {
  mins  <-  substr(x, nchar(x)-1, nchar(x))
  hour  <-  substr(x, 0, nchar(x)-2)
  time  <-  paste0(hour, ':', mins)
  hms::parse_hm(time)
}

prop_na <-function(x, prop) filter(sum(!is.na(x))>=prop)

################################
# Loading Data and Cleaning:
###############################
#
##########
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
######



# AR - includes genetic PC-scores for men. Includes AR. Includes other relevant info. 
AR <-read_dta(here::here("Data/Biomarker/AR.dta")) %>% 
  mutate(sex = if_else(icsex == "1", "male", "female")) %>%
  mutate(uncchdid = sjlabelled::as_character(uncchdid)) %>%
  filter(!is.na(uncchdid) & uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) %>% 
  select(-icsex, -mother, -uncmomid, -starts_with("mom"), -c(allele2_num:AR_IC_allele2)) %>% 
  select(uncchdid, -uncid, everything(.))


#####

# Blood. Includes time and day of blood sampling. Grip strength. Glucose.
blood <-read_dta(here::here("Data/Biomarker/BLOOD.DTA")) %>%
  mutate(uncchdid = uncid) %>% 
  mutate(uncchdid = sjlabelled::as_character(uncchdid)) %>%
  filter(!is.na(uncchdid) & uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) %>%
  mutate(rightgrip_mean = rowMeans(select(., starts_with("rightgrip")), na.rm = TRUE)) %>%
  mutate(leftgrip_mean = rowMeans(select(., starts_with("leftgrip")), na.rm = TRUE)) %>%
  select(-c(foodtype:othermed6, leftgrip1:rightgrip3, monthspreg)) %>% 
  select(uncchdid, everything(.))

blood<-blood %>% 
  mutate(date_blood = ymd(paste(blood$yearblood+2000, blood$monthblood, blood$dayblood, sep = "/"))) %>%
  mutate(time_blood = turn_to_time(blood$timeblood)) %>% 
  mutate(wakeup_time = turn_to_time(blood$wakeuptime)) %>% 
  select(-c(uncid:yearblood)) %>% 
  select(uncchdid, date_blood, time_blood, wakeup_time, glucose, bloodtype, rh, rightgrip_mean, leftgrip_mean) 



#####

#Testosterone
T <-read_dta(here::here("Data/Biomarker/Testosterone 05 09 14 master file 9.22.17_No2_w_2014etc.dta")) %>%
  mutate(sex = if_else(icsex == "1", "male", "female")) %>%
  mutate(uncchdid = sjlabelled::as_character(uncchdid)) %>%
  filter(!is.na(uncchdid) & uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) 


names(T)
length(names(T)) # Too many!!!

# T[,1:100] %>% 
#   summarytools::dfSummary()
# T[,100:200] %>% 
#   summarytools::dfSummary()
# T[,200:ncol(T)] %>% 
#   summarytools::dfSummary()

# Alternatively: 
T %>% 
  select(-contains("14"))

newT <-T %>% 
  select(uncchdid, birthday, salivacollectiondate05, anyoffspring05, youngch05, hholdkids05, fath05c, livewicd05, duramar05, 
         maristat05, numbmard05, amt05, amtime05, pmt05, pmtime05, barcode05,  ickcal05, icpcfat05, dhinc05, ownhouse05, crowd05, assets05, popdensc05, educsc05, urbanic05,
         maristat09, numbmard09, duramar09, fath09c, anyoffspring09, livewic09, oldch09, youngch09, caretime09, hrstotalcare09, cosleep09, amt09, amtime09, pmt09, pmtime09) %>% 
  na_if(., -9) 


# Easily subset by using 09 or 05 like so
newT %>% 
  select(uncchdid, birthday, contains("05"))



####################################

####################################
testost <-left_join(
  left_join(blood, AR, by = 'uncchdid'), 
  newT, by = "uncchdid") %>% 
  select(uncchdid, birthday, basewman, uncid, barcodeid, date_blood, salivacollectiondate05, everything(.)) %>% 
  mutate(age_saliva = salivacollectiondate05-birthday) %>% 
  mutate(age_blood05 = date_blood - birthday)


testost %>% 
  summarytools::dfSummary()



####################################
# Load Blood Testosterone File
####################################
library(haven)
all_cohort_data <- read_dta(here::here("Data/Biomarker", "Full_cohort_for_Meaghan.dta")) %>% 
  as_character(uncchdid) %>% 
  rename(blood_testosterone = testost)




testost <-left_join(testost,
                    all_cohort_data %>% select(uncchdid, contains("testo")) %>% 
                      mutate(log_blood_t = log(blood_testosterone)),
                             by = "uncchdid")

testost %>% 
  filter(!is.na(blood_testosterone))
# Nobody missing blood T

testost %>% 
  ggplot(., aes(x = blood_testosterone)) + 
  geom_histogram(fill = "gray50") +
  ggpubr::theme_pubr()

testost %>% 
  ggplot(., aes(x = log_blood_t)) + 
  geom_histogram(fill = "gray50") +
  ggpubr::theme_pubr()

          