# REPRODUC
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

#############################
# load repro09
#############################


master <- as.character(unzip(here::here("Data/zip_males.zip"), list = TRUE)$Name)

repro09 <-read_dta(unz(here::here("Data/zip_males.zip"), "reproduc.dta"))



mepro <-repro09 %>% 
  rename_all(tolower) %>% 
  mutate(date_interview = ymd(paste(repro$yrntrvw, repro$montrvw, repro$dayntrvw, sep = "/"))) %>% 
  mutate(uncchdid = as_character(uncchdid)) %>% 
  select(uncchdid, date_interview, romantic, ageroman, presrela, sexinter, agesexin, numbsex, sexpaid, paidsex, samesexc, forcesex, familysx, groupsex, pregnant, numbpreg, oftensex, sexpast, daysex, maristat, numbmard, agecrush, agedate, agecourt) %>% 
  na_if(., -9) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) 


                   