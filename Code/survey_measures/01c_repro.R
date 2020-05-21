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

repro <- read_dta(here::here("Data/REPRODUC.DTA"))

mepro <-repro %>% 
  rename_all(tolower) %>% 
  mutate(date_interview05 = lubridate::ymd(paste(repro$yrntrvw, repro$montrvw, repro$dayntrvw, sep = "/"))) %>% 
  mutate(uncchdid = as_character(uncchdid)) %>% 
  select(uncchdid, date_interview05, romantic, ageroman, presrela, sexinter, agesexin, numbsex, sexpaid, paidsex, samesexc, forcesex, familysx, groupsex, pregnant, numbpreg, oftensex, sexpast, daysex, maristat, numbmard, agecrush, agedate, agecourt) %>% 
  na_if(., -9) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) 

head(mepro)

#################################

code <- function(x) {
  a <-get_label(mepro)[x]
  b <-get_labels(mepro)[x]
  c <-get_values(mepro)[x]
  print(c(a,b,c))
}


##################################################################
#################################
#################################

# Scale of pairbonding activity
mepro <-mepro %>% 
  mutate(maristat = rio::factorize(maristat)) %>% 
  mutate(evermar = fct_collapse(maristat, 
                                married_paired = c("Legally married", "Not legally married"),
                                not_married = c("Never married",  "Separated", "Widowed"))) %>%
  mutate(new_numbmarr = if_else(!is.na(numbmard), as_numeric(numbmard), 0)) %>% 
#  select(romantic, presrela, evermar, new_numbmarr) %>%
  mutate(pairbond_score  = as.numeric(romantic)-1 + as.numeric(evermar)-1)

# Scale of sexual activity
sexact <-c("numbsex","oftensex", "new_numbpreg", "sexpast", "daysex")

mepro <-mepro %>% 
  mutate(numbsex =     if_else(!is.na(numbsex),  as_numeric(numbsex), 0)) %>% 
  mutate(sexmonth = as.numeric(if_else(!is.na(oftensex), as_numeric(oftensex), 0))) %>%
  # mutate(sexmonth = if_else(sexmonth > 1 &  sexmonth == 2, 4, 
  #                           if_else(sexmonth > 1 & sexmonth == 3, 8, 
  #                                   sexmonth))) %>% 
  mutate(sexweek = if_else(!is.na(daysex),   as_numeric(daysex), 0)) %>%
  mutate(maristat = rio::factorize(maristat)) %>% 
  mutate(evermar = fct_collapse(maristat, 
                                married_paired = c("Legally married", "Not legally married"),
                                not_married = c("Never married",  "Separated", "Widowed"))) %>% 
  mutate(new_numbpreg = if_else(!is.na(numbpreg), as_numeric(numbpreg), 0)) %>% 
#  select(numbsex, sexmonth, evermar, new_numbpreg) %>%
  mutate(sexactivity_score = as.numeric(numbsex) + as.numeric(new_numbpreg) + as.numeric(sexmonth)) 



# Scale of high risk sexual activity
# 
risk_sex <-c("sexpaid", "paidsex", "forcesex", "familysx", "groupsex")

mepro <-mepro %>% 
#  select(risk_sex) %>% 
  mutate(sexpaid = if_else(!is.na(sexpaid), as_numeric(sexpaid), 0)) %>%
  mutate(paidsex = if_else(!is.na(paidsex), as_numeric(paidsex), 0)) %>%
  mutate(forcesex = if_else(!is.na(forcesex), as_numeric(forcesex), 0)) %>%
  mutate(familysx = if_else(!is.na(familysx), as_numeric(familysx), 0)) %>%
  mutate(groupsex = if_else(!is.na(groupsex), as_numeric(groupsex), 0)) %>%
  mutate(risksex_score = as.numeric(sexpaid)*8 + as.numeric(forcesex)*10 + as.numeric(familysx)*6 + as.numeric(groupsex)*3) 

# physiological risk (std), social risk (caught, police, family) (1-5, 1-5)
# Construct validity - does variable/score x predict number intercourse/pregnancies?
# Predict sex partners

mepro <-mepro %>% 
  select(uncchdid, date_interview05, romantic, ageroman, sexinter, agesexin, agecrush, agedate, agecourt, presrela, evermar, new_numbmarr, numbsex, sexmonth, evermar, new_numbpreg, sexpaid, paidsex, forcesex, familysx, groupsex, pairbond_score, sexactivity_score, risksex_score)


mepro %>% 
  summarytools::dfSummary()
##################################################################
#################################
#################################

mepro %>%   
  gather(., key = "key", value = "value", c(pairbond_score, sexactivity_score, risksex_score)) %>% 
  ggplot(., aes(x = value, fill = key))+
  geom_density(alpha = 0.2)+
  theme_bw()


mepro %>% 
  ggplot(., aes(x = pairbond_score, y = sexactivity_score, color = risksex_score))+
  geom_boxplot(aes(group = pairbond_score), color = "gray70")+
  ggbeeswarm::geom_beeswarm(cex = 1.1)+
  theme_bw()



# Could just create a sexual activity score and a pairbond score. Do the risk score separately. 

repro <-mepro %>% as_character(uncchdid); rm(mepro)
