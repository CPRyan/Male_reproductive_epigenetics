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
  mutate(age_interview98 = date_interview98 - ic_dob) %>%
  select(uncchdid:fatfree_mass, age_interview98, age_blood05, time_bt_98_05, muscle_accretion:fat_accretion, CD8T:Gran, PlasmaBlast, CD8.naive, CD4.naive, DNAmAge, DNAmAgeHannum, DNAmPhenoAge, DNAmGrimAge) %>% 
  gather(data = ., key = "variable", value = "value", c("height", "whr", "bmi", "arm_musc_area", "bfperc", "fatmass", "fatfree_mass", "muscle_accretion", "height_accretion", "arm_accretion", "fat_accretion")) %>% 
  select(uncchdid, variable, value, everything(.))

a_nest <-a_gathered %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  nest()

# ###################
# # DNAmAge
# ###################
# a_nest %>% 
#   mutate(model = map(data, ~lm(DNAmAge ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
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

# #################

# All
outcome <- "DNAmAge"
variables <-c("value", "smoke", "drink", "icpc1:icpc3", "SEAsum_83_05", "age_blood05", "age_interview98")
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))


DNAmAge_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "DNAmAge") %>% 
  unnest(tidied)  %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
outcome <- "DNAmAgeHannum"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

DNAmAgeHannum_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "DNAmAgeHannum") %>% 
unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
 
outcome <- "DNAmGrimAge"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

grim_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
    add_column(clock_val = "DNAmGrimAge") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))
  
outcome <- "DNAmPhenoAge"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

pheno_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
    add_column(clock_val = "DNAmPhenoAge") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value)) 
 

outcome <- "CD8T"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

CD8T_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD8T") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

outcome <- "CD4T"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

CD4T_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD4T") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

outcome <- "Mono"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

Mono_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Mono") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

outcome <- "NK"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

NK_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "NK") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))


outcome <- "Gran"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

Gran_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Gran") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

outcome <- "PlasmaBlast"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

PlasmaBlast_a <-a_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "PlasmaBlast") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))



clocks <-rbind(DNAmAge_a, DNAmAgeHannum_a, grim_a, pheno_a) %>% 
  mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
  ggplot(., aes(x = estimate, y = variable, col = sigs))+
  geom_point()+
  scale_color_manual(values = c("gray", "red"))+
  # scale_x_continuous(limits = c(-0.7, 0.7))+
  geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
  geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
  ggpubr::theme_classic2()+
  facet_wrap(~clock_val, scales = "free_x")+
  theme(legend.position="top")

cells <-rbind(CD4T_a, CD8T_a, Mono_a, NK_a, PlasmaBlast_a, Gran_a) %>% 
  mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
  ggplot(., aes(x = estimate, y = variable, col = sigs))+
  geom_point()+
  scale_color_manual(values = c("gray", "red"))+
  #  scale_x_continuous(limits = c(-0.7, 0.7))+
  geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
  geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
  ggpubr::theme_classic2()+
  facet_wrap(~clock_val, scales = "free_x")+
  theme(legend.position="top")


###############################################################
###############################################################
