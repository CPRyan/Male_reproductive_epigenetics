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

set.seed(10)

mepro09 <-mepro09 %>% 
  mutate(agesexin_simul = ifelse(is.na(agesexin), floor(runif(10, min=25, max=35)), agesexin), # Random integer bt. 25 and 35 from a normal distribution
         steady = ifelse(is.na(steady), 0, steady), 
         pregnant = ifelse(is.na(pregnant), 0, pregnant), 
         timesprg = ifelse(is.na(timesprg), 0, timesprg)) 

mepro09 %>% 
  ggplot(., aes(x = agesexin_simul))+
  geom_histogram()


# Ok I'm still losing 12 guys here. 

###########################################################
# Need the date of the survey to get their age in 2009.
screen09 <-read_dta(here::here("Data/zip_males_2009/screen.dta"))

screen09 <-screen09 %>% 
  mutate(interview_date09 = lubridate::ymd(paste(screen09$yrntrvw, screen09$montrvw, screen09$dayntrvw, sep = "/"))) %>% 
  as_character(uncchdid) %>% 
  select(uncchdid, interview_date09)
  

repro09 <-left_join(mepro09, screen09, by = "uncchdid")
