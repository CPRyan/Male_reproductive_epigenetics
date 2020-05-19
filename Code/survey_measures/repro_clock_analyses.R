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
  select(uncchdid:icpc3, bmi, SEAsum_83_05, romantic, sexinter, presrela, evermar, numbsex, new_numbpreg, CD8T:Gran, PlasmaBlast, CD8.naive, CD4.naive, IEAA, EEAA, AgeAccelPheno, AgeAccelGrim) %>% 
  gather(data = ., key = "variable", value = "value", 10:15) %>% 
  select(uncchdid, variable, value, everything(.))



r_nest <-r_gathered %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  nest()




###################
# IEAA
###################
# r_nest %>% 
#   mutate(model = map(data, ~lm(IEAA ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
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
# ###################
# # EEAA
# ###################
# r_nest %>% 
#   mutate(model = map(data, ~lm(EEAA ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
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
# ###################
# # AgeAccelGrim
# ###################
# r_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelGrim ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
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
# 
# ###################
# # PhenoAge
# ###################
# r_nest %>% 
#   mutate(model = map(data, ~lm(AgeAccelPheno ~ value + smoke + drink + icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
#          tidied = map(model, tidy)) %>% 
#   unnest(tidied) %>% 
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

#################

# All

IEAA_r <-r_nest %>% 
  mutate(model = map(data, ~lm(IEAA ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "IEAA") %>% 
  unnest(tidied)  %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

EEAA_r <-r_nest %>% 
  mutate(model = map(data, ~lm(EEAA ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "EEAA") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))


grim_r <-r_nest %>% 
  mutate(model = map(data, ~lm(AgeAccelGrim ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "AgeAccelGrim") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

pheno_r <-r_nest %>% 
  mutate(model = map(data, ~lm(AgeAccelPheno ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "AgeAccelPheno") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value)) 




CD8T_r <-r_nest %>% 
  mutate(model = map(data, ~lm(CD8T ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD8T") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

CD4T_r <-r_nest %>% 
  mutate(model = map(data, ~lm(CD4T ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "CD4T") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

Mono_r <-r_nest %>% 
  mutate(model = map(data, ~lm(Mono ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Mono") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

NK_r <-r_nest %>% 
  mutate(model = map(data, ~lm(NK ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "NK") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))



Gran_r <-r_nest %>% 
  mutate(model = map(data, ~lm(Gran ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "Gran") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))



PlasmaBlast_r <-r_nest %>% 
  mutate(model = map(data, ~lm(PlasmaBlast ~ value + smoke + drink +  icpc1:icpc3 + bmi + SEAsum_83_05, data = .x)),
         tidied = map(model, tidy)) %>%
  add_column(clock_val = "PlasmaBlast") %>% 
  unnest(tidied) %>% 
  filter(term == "value1") %>%
  mutate(adjusted = p.adjust(p.value))

rbind(IEAA_r, EEAA_r, grim_r, pheno_r) %>% 
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


rbind(CD4T_r, CD8T_r, Mono_r, NK_r, PlasmaBlast_r, Gran_r) %>% 
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
