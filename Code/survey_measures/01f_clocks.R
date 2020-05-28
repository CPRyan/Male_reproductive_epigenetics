select <-dplyr::select

clocks <-read_csv(here::here("Data/DNAmAge_new", "QC_Norm_LisaMcEwan_meth_DatMini3.output.csv")) %>% 
  select(-SampleID) %>% 
  select(uncchdid, Female, Age, everything(.)) %>% 
  mutate(uncchdid = as_character(uncchdid)) %>% 
  filter(uncchdid %in% chk.bld.draw[chk.bld.draw$icsex == "1=male",]$uncchdid) %>% 
  select(-c(ProbabilityFrom.X.Vasc.Endoth.Umbilical.:ProbabilityFrom.Uterine.Endomet)) 


