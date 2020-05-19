
library(broom)
library(purrr)

mtcars %>% 
  group_by(am) %>% 
  nest()%>%
  mutate(fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, tidy)) %>% 
  unnest(results)
  