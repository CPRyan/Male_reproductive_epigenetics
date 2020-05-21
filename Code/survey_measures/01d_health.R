# Health
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
health <- read_dta(here::here("Data/zip_child_2005/moreheal.DTA")) %>% 
  rename_all(tolower) %>% 
  select(uncchdid, smoke, ofensmok, drink, oftendri)%>% 
  na_if(., -9) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) %>% 
  as_character(uncchdid)


health %>% 
  summarytools::dfSummary()

health %>% select(uncchdid, ofensmok, oftendri) %>% gather("habit", "freq", 2:3) %>% 
  ggplot(., aes(x = freq, fill = habit))+
  geom_density()+
  facet_wrap(~habit)


health %>% 
  filter(oftendri > 0) %>% 
  ggplot(., aes(oftendri, ofensmok))+
  # ggpubr::stat_regline_equation()+
  ggpubr::stat_cor()+
  stat_smooth(method = "lm")+
  geom_point()
