# This loads the SES variables from Thom, used in McDade, Ryan et al. 2019. 


library(haven)
SEAvars <- read_dta("Data/Other/SEAvars_DNAm.dta")

# Each year is labelled as 
# "Mean unstandardized items" although they look pretty standardized to me...
SEAvars %>% 
  select(-female, -seatotal) %>% 
  gather(key = "measure", value = "value", c(2:5)) %>% 
  ggplot(., aes(x = value, color = measure, fill = measure))+
  geom_density(alpha = 0.2)+
  ggpubr::theme_pubr()


SEAvars %>% 
  select(-seatotal) %>% 
  gather(key = "measure", value = "value", c(3:6)) %>%
  mutate(measure = fct_relevel(measure, "sea05", after = Inf)) %>% 
  ggplot(., aes(x = measure, y = value, group = uncchdid, color = factor(female)))+
  geom_point(alpha = 0.1)+
  geom_line(alpha = 0.2)+
  scale_color_manual(values = c("navyblue", "red"))+
  ggpubr::theme_pubr()


SEAvars %>% 
  select(-seatotal) %>% 
  filter(female == "0") %>% 
  gather(key = "measure", value = "value", c(3:6)) %>%
  mutate(measure = fct_relevel(measure, "sea05", after = Inf)) %>% 
  ggplot(., aes(x = measure, y = value, group = uncchdid, color = factor(female)))+
  geom_point(alpha = 0.1)+
  geom_line(alpha = 0.2)+
  scale_color_manual(values = c("navyblue", "red"))+
  ggpubr::theme_pubr()



SEAvars %>% 
  filter(female == "0") %>%
  select(-uncchdid, -female) %>%
  mutate(SEAsum_83_05 = sea83+sea05) %>% 
  psych::pairs.panels()

SEAvars <-SEAvars %>% 
  mutate(SEAsum_83_05 = sea83+sea05)


write_csv(SEAvars, here::here("Data/Other", "SEAvars_males_sum83_05.csv"))
