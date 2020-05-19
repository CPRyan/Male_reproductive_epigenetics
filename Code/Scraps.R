
####################################
library(ggpubr)

# Just curious - check smoking reported and smoking DNAm
full_mage %>% 
  mutate(ofensmok = if_else(ofensmok < 0, 0, ofensmok)) %>% 
  ggplot(., aes(ofensmok, DNAmPACKYRSAdjAge))+
  ggpubr::stat_cor()+
  stat_smooth(method = "lm", color = "gray30", fill = "gray80")+
  ggbeeswarm::geom_beeswarm(color = "#e41a1c")+
  scale_color_brewer(palette = "Set1")+
  theme_pubr()


full_mage %>% 
  ggplot(., aes(x = presrela, y = IEAA, group = presrela, color = as_factor(presrela)))+
  ggbeeswarm::geom_beeswarm()+
  geom_boxplot(alpha = 0.2)+
  scale_color_brewer(palette = "Set1")+
  theme_pubr()


full_mage %>%  
  ggplot(., aes(x = agesexin, y = DNAmGrimAgeAdjAge))+
  ggpubr::stat_cor()+
  ggbeeswarm::geom_beeswarm(color = "#e41a1c", alpha = 0.8)+
  stat_smooth(method = "lm", color = "gray30", fill = "gray85")+
  scale_color_brewer(palette = "Set1")+
  theme_pubr()


####################################



a_nested <-a_gathered %>% 
  select(uncchdid:icpc3, IEAA) %>% 
  na.omit() %>% 
  group_by(variable) %>% 
  nest() 

# Check what it looks like. 
a_nested$data[[1]]

# Pull off the fits. 
a_fits <-a_nested %>% 
  mutate(model = map(data, ~lm(IEAA ~ value + smoke + drink + age_blood05 + icpc1:icpc3, data = .x)))

a_fits %>%
  mutate(coef = map(model, tidy)) %>%
  unnest(coef) %>% 
  filter(term == "value") %>% 
  print(n = Inf)
