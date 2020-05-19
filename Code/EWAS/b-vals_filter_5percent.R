source(here("Code/functions", "EWAS_functions.R"))

#########
#########
# Males and Females. 

mvals <-data.table::fread(file = here::here("Data/DNAm", "mvals_cebu_final_Xincl.csv")) 

mvals <-mvals %>% column_to_rownames("V1")

##########

bvals <-m2beta(mvals)


bvals.perc <-apply(bvals, 1, quantile, probs = c(0.1,0.9), na.rm = TRUE)

var.probes <-( ( bvals.perc["90%",] - bvals.perc["10%" ,] ) * 100) > 5

sum(var.probes)
# 143052

names_var_probes <-names(var.probes[var.probes == TRUE]) %>% 
  as.data.frame() %>% 
  mutate(`.` = as_character(`.`)) %>% 
  rename(., "probe" = `.`)


write_csv(names_var_probes, here::here("Data/DNAm", "names_var_probes_m_and_f_xyincl.csv"))



#######################################
# Males only. 
#######################################

mvals <-data.table::fread(file = here::here("Data/DNAm", "mvals_cebu_final_Xincl.csv"),
                          select = c("V1", men_w_eage$uncchdid, "20727_R2")) 

mvals <-mvals %>% column_to_rownames("V1")

##########

bvals <-m2beta(mvals)


bvals.perc <-apply(bvals, 1, quantile, probs = c(0.1,0.9), na.rm = TRUE)

var.probes <-( ( bvals.perc["90%",] - bvals.perc["10%" ,] ) * 100) > 5

sum(var.probes)
# 142777

names_var_probes <-names(var.probes[var.probes == TRUE]) %>% 
  as.data.frame() %>% 
  mutate(`.` = as_character(`.`)) %>% 
  rename(., "probe" = `.`)


write_csv(names_var_probes, here::here("Data/DNAm", "names_var_probes_menonly_xyincl.csv"))




