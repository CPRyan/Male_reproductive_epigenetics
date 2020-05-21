# clock_analyses
library(tidyverse)
library(haven)
library(sjlabelled)
library(broom)
library(pander)
#################
# Anthro
#################

here <-here::here()

source(here::here("/Code/survey_measures", "01a_anthro.R"))
source(here::here("/Code/survey_measures", "01h_anthro_1998_05.R"))
source(here::here("/Code/survey_measures", "01f_clocks.R"))

############ 
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid)) %>% select(-bmi)
names(control_vars)

a <-left_join(control_vars, anthro_time, # bmi is in the control vars
              by = "uncchdid") %>% na.omit()

################
# Build the model structure
################
a_clocks <-left_join(a, clocks, by = "uncchdid")

a_gathered <-a_clocks %>% 
  select(-c(subscap_mean:tricep_mean, icpc4:icpc10)) %>%
  select(uncchdid:fatfree_mass, date_interview98, muscle_accretion:fat_accretion, CD8T:Gran, PlasmaBlast, CD8.naive, CD4.naive, IEAA, EEAA, AgeAccelPheno, AgeAccelGrim) %>% 
  gather(data = ., key = "variable", value = "value", c("height", "whr", "bmi", "arm_musc_area", "bfperc", "fatmass", "fatfree_mass", "muscle_accretion", "height_accretion", "arm_accretion", "fat_accretion")) %>% 
  select(uncchdid, variable, value, everything(.))

a_nest <-a_gathered %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  nest()

# ###################
# # IEAA
# ###################
# a_nest %>% 
#   mutate(model = map(data, ~lm(IEAA ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied)%>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")
# 
# ###################
# # EEAA
# ###################
# a_nest %>% 
#   mutate(model = map(data, ~lm(EEAA ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")
# 
# ###################
# # AgeAccelGrim
# ###################
# a_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelGrim ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")
# 
# 
# ###################
# # PhenoAge
# ###################
# a_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelPheno ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")
#   
# #################

# All

IEAA_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(IEAA ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "IEAA") %>% 
  unnest(tidied)  %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
EEAA_a <-a_nest %>% 
    mutate(model = map(data, ~lm(EEAA ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05  + date_interview98, data = .x)),
           tidied = map(model, tidy)) %>%
  add_column(clock_val = "EEAA") %>% 
unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
  
grim_a <-a_nest %>% 
    mutate(model = map(data, ~lm(AgeAccelGrim ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05  + date_interview98, data = .x)),
           tidied = map(model, tidy)) %>%
    add_column(clock_val = "AgeAccelGrim") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
pheno_a <-a_nest %>% 
    mutate(model = map(data, ~lm(AgeAccelPheno ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
           tidied = map(model, tidy)) %>%
    add_column(clock_val = "AgeAccelPheno") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value)) 
 



CD8T_a <-a_nest %>% 
  mutate(model = map(data, ~lm(CD8T ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD8T") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

CD4T_a <-a_nest %>% 
  mutate(model = map(data, ~lm(CD4T ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD4T") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

Mono_a <-a_nest %>% 
  mutate(model = map(data, ~lm(Mono ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Mono") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

NK_a <-a_nest %>% 
  mutate(model = map(data, ~lm(NK ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "NK") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))



Gran_a <-a_nest %>% 
  mutate(model = map(data, ~lm(Gran ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Gran") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))



PlasmaBlast_a <-a_nest %>% 
  mutate(model = map(data, ~lm(PlasmaBlast ~ value + smoke + drink +  icpc1:icpc3 + SEAsum_83_05 + date_interview98, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "PlasmaBlast") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))




  
rbind(IEAA_a, EEAA_a, grim_a, pheno_a) %>% 
  mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
  ggplot(., aes(x = estimate, y = variable, col = sigs))+
  geom_point()+
  scale_color_manual(values = c("gray", "red"))+
  scale_x_continuous(limits = c(-0.7, 0.7))+
  geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
  geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
  ggpubr::theme_classic2()+
  facet_wrap(~clock_val)+
  theme(legend.position="top")

rbind(CD4T_a, CD8T_a, Mono_a, NK_a, PlasmaBlast_a, Gran_a) %>% 
  mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
  ggplot(., aes(x = estimate, y = variable, col = sigs))+
  geom_point()+
  scale_color_manual(values = c("gray", "red"))+
  #  scale_x_continuous(limits = c(-0.7, 0.7))+
  geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
  geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
  ggpubr::theme_classic2()+
  facet_wrap(~clock_val)+
  theme(legend.position="top")


###############################################################
###############################################################
