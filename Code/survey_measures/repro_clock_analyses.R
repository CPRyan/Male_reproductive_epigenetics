# clock_analyses
library(broom)
library(pander)
#################
# Repro
#################

here <-here::here()

source(here::here("/Code/survey_measures", "01c_repro.R"))
source(here::here("/Code/survey_measures", "01f_clocks.R"))

############ 
# Other info incorporate
control_vars <-read_csv(here::here("Data/Other", "control_vars.csv")) %>% mutate(uncchdid = as_character(uncchdid))
names(control_vars)

r <-left_join(control_vars, repro, by = "uncchdid") 

################
# Build the model structure
################
r_clocks <-left_join(r, clocks, by = "uncchdid")

r_gathered <-r_clocks %>% 
  select(uncchdid:icpc3, bmi, SEAsum_83_05, age_blood05, romantic, sexinter, presrela, evermar, numbsex, new_numbpreg, CD8T:Gran, PlasmaBlast, CD8.naive, CD4.naive, DNAmAge, DNAmAgeHannum, DNAmPhenoAge, DNAmGrimAge) %>%
  mutate(log_numbsex = log(numbsex+1)) %>% 
  gather(data = ., key = "variable", value = "value", c("romantic", "sexinter", "presrela", "evermar", "numbsex", "new_numbpreg")) %>% 
  select(uncchdid, variable, value, everything(.))



r_nest <-r_gathered %>%
  as_numeric(value) %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  nest()




###################
# DNAmAge
###################
# r_nest %>% 
#   mutate(model = map(data, ~lm(DNAmAge ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied)%>% 
#   filter(term == "value1") %>%
#   mutate(adjusted = p.adjust(p.value)) %>%
#   mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
#   ggplot(., aes(x = estimate, y = variable,  col = sigs))+
#   geom_point()+
#   scale_color_manual(values = c("gray", "red"))+
# #  scale_x_continuous(limits = c(-0.7, 0.7))+
#   geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
#   geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
#   ggpubr::theme_classic2()+
#   theme(legend.position="top")
#
#################

# All
variables <-c("value", "smoke", "drink", "icpc1", "icpc2", "icpc3", "SEAsum_83_05", "age_blood05")


outcome <- "DNAmAge"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
DNAmAge_r <-r_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "DNAmAge") %>% 
  unnest(tidied)  %>% 
  filter(str_detect(term, "value")) %>%
  mutate(adjusted = p.adjust(p.value))

outcome <- "DNAmAgeHannum"
f <- as.formula(
  paste(outcome, 
        paste(variables, collapse = " + "), 
        sep = " ~ "))

DNAmAgeHannum_r <-r_nest %>% 
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

grim_r <-r_nest %>% 
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

pheno_r <-r_nest %>% 
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

CD8T_r <-r_nest %>% 
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

CD4T_r <-r_nest %>% 
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

Mono_r <-r_nest %>% 
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

NK_r <-r_nest %>% 
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

Gran_r <-r_nest %>% 
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

PlasmaBlast_r <-r_nest %>% 
  mutate(model = map(data, .f = ~lm(f, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "PlasmaBlast") %>% 
  unnest(tidied) %>% 
  filter(term == "value") %>%
  mutate(adjusted = p.adjust(p.value))



clocks <-rbind(DNAmAge_r, DNAmAgeHannum_r, grim_r, pheno_r) %>% 
  mutate(sigs = if_else(adjusted < 0.05, "significant", "not-significant") ) %>% 
  ggplot(., aes(x = estimate, y = variable, col = sigs))+
  geom_point()+
  scale_color_manual(values = c("gray", "red"))+
  #scale_x_continuous(limits = c(-0.7, 0.7))+
  geom_vline(xintercept = 0, col = "gray", linetype = "dashed")+
  geom_pointrange(aes(xmin = estimate - std.error , xmax = estimate + std.error))+
  ggpubr::theme_classic2()+
  facet_wrap(~clock_val)+
  theme(legend.position="top")

cells <-rbind(CD4T_r, CD8T_r, Mono_r, NK_r, PlasmaBlast_r, Gran_r) %>% 
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






