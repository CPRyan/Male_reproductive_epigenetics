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

###########################################################

repro09 <- read_dta(here::here("Data/zip_males_2009/reproduc.dta"))

repro09 %>% select(contains("int"), contains("roman"), contains("preg"), contains("prg"), contains("ch"))


mepro09 <-repro09 %>% 
  rename_all(tolower) %>% 
  mutate(uncchdid = as_character(uncchdid)) %>% 
  select(uncchdid, sexinter, agesexin, steady, pregnant, timesprg, haschild) %>% 
  na_if(., -9) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) 

mepro09
# Ok I'm losing 12 guys here. Meh. 
