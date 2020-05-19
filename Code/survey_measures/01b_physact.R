
# Physact2
###################################################################################################################################
# Load data
######################################################################################################################################################################################################################################################################

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


physact2 <- read_dta(here::here("Data/zip_child/physact2.DTA")) %>% 
  rename_all(tolower) %>% 
  na_if(., -9) %>%   
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid)

#################################################################################
## Functions
#################################################################################

all_na <- function(x) any(!is.na(x))

# Easier for ifelse() statement chain
# thatssorandom::ie()
# devtools::install_github("edwinth/thatssorandom")
# https://www.r-bloggers.com/a-wrapper-around-nested-ifelse/


#################################################################################
### Other physical activities
#################################################################################

other <-physact2 %>% 
  select(uncchdid, othrpact, othract1:othract6, xmoact1:xmoact6, minacta1:minacta6) %>% 
  select_if(all_na) 


other %>% print(n = Inf) %>% summarytools::dfSummary()

# Are there other physical activities that IC regularly participates in > 1 a month
# otherpact == 1
# If Yes, what kind of activity?
# otheract_level == CODE 

# 1 - Biking
# 2 - Dancing
# 3 - Cleaning the house (unspecified)
# 4 - Marching (CAT/ROTC)
# 5 - Playing badminton
# 6 - Playing basketball / baseball / softball
# 7 - Playing billiard
# 8 - Playing table tennis (pingpong)
# 9 - Playing volleyball
# 10 - Playing with the kite
# 11 - Washing clothes
# 12 - Working in the garden/ cultivating the garden
# 13 - Working out in a gym
# 14 - Swimming
# 15 - Jogging/shuttle run
# 16 - Karate
# 17 - Fetching water
# 18 - Ironing clothes
# 19 - Repairing vehicles
# 20 - Cleaning the barangay (unspecified)
# 21 - Serenading (involves moving from place to place & singing) 
# 22 - Fishing
# 23 - Scrubbing the floor
# 24 - Walking (ushering in church)
# 25 - Skating / skateboard
# 26 - Marketing
# 27 - Tending livestock
# 28 - Practice with the band
# 29 - Cooking
# 30 - Bowling
# 31 - Gathering / chopping firewood 
# 32 - Washing dishes
# 33 - Hiking
# 34 - Carpentry
# 35 - Playing darts
# 36 - Playing guitar
# 37 - Grinding
# 38 - Carrying a child
# 39 - Soccer/football/kickball
# 40 - Skim boarding
# 41 - Skipping rope
# 42 - Lawn tennis
# -9 - NA

# Guys played 
# 6 = basketball/baseball/softball  (3, 3)
# 7 = billiards (1, 3)
# 8 = pingpong  (2, 3)
# 9 = volleyball (3, 3)
# 14 = swimming (3, 1)
# 29 = cooking (1, 3)
# 33 = hiking (2, 2)

#############################################################################
#############################################################################

other %>% filter(uncchdid == 21783)
# The hiker reports equivalent to 24 hours, 3 x a month hiking. 
# Fix. 
# https://www.alltrails.com/philippines/cebu/cebu-city
# Average hike for top trails in cebu (rounded to quarter hour) is mean(4, 5.25, 11.0, 1.75, 2.5) = 4
other[other$uncchdid == 21783, "minacta1"] <-4*60

# Physical component

# level_factor <-c(`6` = "3", `7` = "1", `8` = "2", `9` = "3", `14` = "3", `29` = "1", `33` = "2", .default = NA)
# level_factor <-c(`6` = "vigorous", `7` = "light", `8` = "moderate", `9` = "vigorous", `14` = "vigorous", `29` = "light", `33` = "moderate", .default = NA)
# level_factor <-c(`6` = "basketball", `7` = "billiards", `8` = "pingpong", `9` = "volleyball", `14` = "swimming", `29` = "cooking", `33` = "hiking", .default = NA)


# Using MET
# Ainsworth, B. E., Haskell, W. L., Herrmann, S. D., Meckes, N., Bassett, D. R. J., Tudor-Locke, C., … Leon, A. S. (2011). 2011 Compendium of Physical Activities: A Second Update of Codes and MET Values. Medicine & Science in Sports & Exercise, 43(8), 1575–1581. https://doi.org/10.1249/MSS.0b013e31821ece12

level_factor <-c(`6` = "6.5", `7` = "2.5", `8` = "4.0", `9` = "5.5", `14` = "7.0", `29` = "3.3", `33` = "6.0", .default = NA)

# Physical activity
other <-other %>% 
  mutate(othract_scale1 = recode_factor(othract1, !!!level_factor)) %>% 
  mutate(othract_scale2 = recode_factor(othract2, !!!level_factor)) %>% 
  mutate(othract_scale3 = recode_factor(othract3, !!!level_factor)) %>% 
  mutate(othract_invest1 = as_numeric(othract_scale1) * xmoact1 * minacta1) %>% 
  mutate(othract_invest2 = as_numeric(othract_scale2) * xmoact2 * minacta2) %>% 
  mutate(othract_invest3 = as_numeric(othract_scale3) * xmoact3 * minacta3) %>%
  rowwise() %>% 
  mutate(sum_other_invest = sum(othract_invest1, othract_invest2, othract_invest3, na.rm = TRUE))
  

# Social Component
soc_level_factor <-c(`6` = "10", `7` = "4", `8` = "4", `9` = "12", `14` = "1", `29` = "4", `33` = "2", .default = NA)


other <-other %>% 
  mutate(othract_socscale1 = recode_factor(othract1, !!!soc_level_factor)) %>% 
  mutate(othract_socscale2 = recode_factor(othract2, !!!soc_level_factor)) %>% 
  mutate(othract_socscale3 = recode_factor(othract3, !!!soc_level_factor)) %>% 
  mutate(othract_socinvest1 = as_numeric(othract_socscale1) * xmoact1 * minacta1) %>% 
  mutate(othract_socinvest2 = as_numeric(othract_socscale2) * xmoact2 * minacta2) %>% 
  mutate(othract_socinvest3 = as_numeric(othract_socscale3) * xmoact3 * minacta3) %>%
  rowwise() %>% 
  mutate(sum_other_socinvest = sum(othract_socinvest1, othract_socinvest2, othract_socinvest3, na.rm = TRUE)) %>% 
  select(uncchdid, othrpact, othract1:othract3, 
         othract_invest1:othract_invest3, sum_other_invest, 
         othract_socinvest1:othract_invest3, sum_other_socinvest) %>% 
  print(n = Inf)                                             





other %>% 
  filter(othrpact  == 1) %>% 
  select(uncchdid, othract1:othract3, sum_other_invest, sum_other_socinvest) %>% 
  gather(., key = "key", value = "value", c(sum_other_invest, sum_other_socinvest)) %>% 
  ggplot(., aes(x = value, fill = key))+
  geom_density(alpha = 0.2)+
  theme_bw()


names <-c(`6` = "basketball", `7` = "billiards", `8` = "pingpong", `9` = "volleyball", `14` = "swimming", `29` = "cooking", `33` = "hiking", .default = NA)

other %>% 
  filter(othrpact  == 1) %>% 
  mutate(new_names = recode_factor(othract1,  !!!names)) %>%  
  ggplot(., aes(x = sum_other_invest, y = sum_other_socinvest, color = new_names))+
  geom_point()+
  theme_bw()


#################################################################################
### Workout activities
#################################################################################
weights <-physact2 %>% 
  select(uncchdid, workout, workact1:workact6, xmowork1:xmowork6, minwact1:minwact6) %>% 
  filter(workout > 0) %>% 
  select_if(all_na)


weights %>% print(n = Inf) 
# Only 5 dudes workout. All lift weights
# None do more than 1 type of workout

names(weights)



#################################################################################
### Sedentary activities
#################################################################################
sedent <-physact2 %>% 
  select(uncchdid, sedenact, sedeact1:sedeact6, xmoseda1:xmoseda6, minacts1:minacts6) %>% 
  select_if(all_na) 

sedent %>% print(n = Inf)

names(sedent)



#################################################################################
### Work/domestic life activities
#################################################################################

worklife <-physact2 %>% 
  select(worknow, physwork, physhact, wellwork, wellhact, streswor, streshac)

worklife %>% print(n = Inf)













# Measure of workout
Workout (0 = no, >0 yes)

# Strenuous or not
workact * xmowork * minwact

High:
1 - Jogging
2 - Lifting weights 
3 - Sit-up
4 - Push-up
5 - Dancing
11 - Hola-hoop

Low: 
6 - Stretching
7 - Bending
8 - Aerobics
9 - Kick boxing 
10 - Walking





# Sum of strenuous and sum of light


##########################

# Measure of sedentary life
# Social or not
# Number of times
# Sum of social or not

# Measure of worklife
# Physical labor
# Social labor (getting along with others)
# Psychosocial labor (stress)

# Otheract






