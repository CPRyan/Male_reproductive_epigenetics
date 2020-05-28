# clock_analyses
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(pander)
#################
# testost
#################

# here <-here::here()
select <-dplyr::select


source(here::here("/Code/survey_measures", "01e_testost.R"))
source(here::here("/Code/survey_measures", "01f_clocks.R"))


colSums(is.na(testost))
############ 
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid))
names(control_vars)

t <-left_join(control_vars, testost, by = c(intersect(names(control_vars), names(testost)))) 


################
# Build the model structure
################
t_clocks <-left_join(t, clocks, by = "uncchdid")

t_clocks <-t_clocks %>%
  select(uncchdid:age_blood05, bmi, SEAsum_83_05, salivacollectiondate05, age_saliva, birthday, age_saliva, amt05, wakeup_time, amtime05, pmt05, pmtime05, log_blood_t, icpc1:icpc10,
         anyoffspring05, fath05c, hholdkids05, maristat05, icpcfat05,
         rightgrip_mean, leftgrip_mean, CD8T:Gran, PlasmaBlast, CD8.naive, CD4.naive, DNAmAge, DNAmAgeHannum, DNAmGrimAge, DNAmPhenoAge) 

t_gathered <-t_clocks %>%
  gather(data = ., key = "variable", value = "value", c("amt05", "pmt05", "log_blood_t", "anyoffspring05", "fath05c", "hholdkids05", "icpcfat05", "rightgrip_mean")) %>% 
  select(uncchdid, variable, value, everything(.))

############

t_nest <-t_gathered %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  tidyr::nest()

#saveRDS(t_nest, file = "t_nest_reprex.RDS")

#############

###################
# IEAA
###################
# t_nest %>% 
#   mutate(model = map(data, ~lm(IEAA ~ value + age_saliva + amtime05 + pmtime05 + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied)%>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   #  scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")

###################
# EEAA
###################
# t_nest %>% 
#   mutate(model = map(data, ~lm(EEAA ~ value + age_saliva + amtime05 + pmtime05 + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   #  scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")

###################
# AgeAccelGrim
###################
# t_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelGrim ~ value + age_saliva + amtime05 + pmtime05 +  amtime05 + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   #  scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")


###################
# PhenoAge
###################
# t_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelPheno ~ value + age_saliva + amtime05 + pmtime05 + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
#   filter(term == "value") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
#   #  scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")

#################

# All
variables <-c("value", "smoke", "drink", "icpc1", "icpc2", "icpc3", "amtime05", "pmtime05", "age_saliva", "SEAsum_83_05", "bmi")




outcome <- "DNAmAge"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

DNAmAge_t <-t_nest %>%
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
DNAmAgeHannum_t <-t_nest %>% 
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

grim_t <-t_nest %>% 
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

pheno_t <-t_nest %>% 
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
CD8T_t <-t_nest %>% 
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
CD4T_t <-t_nest %>% 
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
Mono_t <-t_nest %>% 
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
NK_t <-t_nest %>% 
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
Gran_t <-t_nest %>% 
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
PlasmaBlast_t <-t_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "PlasmaBlast") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))

clocks <-rbind(DNAmAge_t, DNAmAgeHannum_t, grim_t, pheno_t) %>% 
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


cells <-rbind(CD4T_t, CD8T_t, Mono_t, NK_t, PlasmaBlast_t, Gran_t) %>% 
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


