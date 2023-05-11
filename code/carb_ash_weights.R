library(tidyverse)

# import data
data = readxl::read_excel("data/dry_ash weight_carbstore.xlsx", sheet = "Sheet2") %>% janitor::clean_names()

# graphs
data %>% 
  drop_na() %>% 
  ggplot(aes(x = time_point, y = dry_weight_g,
             group = name,
             color = media,
             shape = name))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2)
             )+
  labs(x = "",
       y = "dry weight (g)",
       title = "Dry weight")+
  theme_bw()

data %>% 
  drop_na() %>% 
  ggplot(aes(x = time_point, y = ash_free_wt_g_l,
             group = name,
             color = media,
             shape = name))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2)
  )+
  labs(x = "Time point",
       y = "g/mL",
       title = "Ash free weight")+
 
  theme_bw()

