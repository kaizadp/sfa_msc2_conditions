
## making summary plots for CO2 and Novacyte 
## kfp, Oct 2023

# CO2 summary heatmaps ----
co2_summary = 
  co2_samples %>% 
  group_by(Condition, Hours, substrate) %>% 
  dplyr::summarise(mean_co2 = mean(co2_ppm)) %>% 
  order_conditions()

# plot all hours
co2_summary %>% 
 # filter(Hours %in% c("68", "139")) %>% 
  mutate(mean_co2 = round(mean_co2)) %>% 
  ggplot(aes(x = Hours, y = Condition, fill = mean_co2))+
  geom_tile(alpha = 1, color = "black")+
  coord_equal()+
  geom_text(aes(label = mean_co2), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("gley", 5))+
  ##  scale_fill_stepsn(colors = soilpalettes::soil_palette("redox2", 12),
  ##                    breaks = c(0, 2000, 5000, 10000, 40000, 50000))+
  facet_wrap(~substrate, nrow = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())
#  guides(fill = guide_legend(override.aes = list(alpha=0.9)))


# plot only 94, 163 hr
co2_summary %>% 
  filter(Hours %in% c("94", "163")) %>% 
  mutate(mean_co2 = round(mean_co2)) %>% 
  ggplot(aes(x = Hours, y = Condition, fill = mean_co2))+
  geom_tile(alpha = 0.8, color = "black")+
  coord_equal()+
  geom_text(aes(label = mean_co2), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("redox2", 5))+
  ##  scale_fill_stepsn(colors = soilpalettes::soil_palette("redox2", 12),
  ##                    breaks = c(0, 2000, 5000, 10000, 40000, 50000))+
#  scale_fill_gradient(low = "grey90", high = "steelblue2")+                    
  facet_wrap(~substrate, nrow = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())

#
# CO2 comparison heatmaps ----

co2_summary_full = 
  co2_summary %>% 
  rename(Condition_x = Condition,
         mean_co2_x = mean_co2) %>% 
  full_join(co2_summary %>% 
              rename(Condition_y = Condition,
                     mean_co2_y = mean_co2)) %>% 
  mutate(percent_change = 100*(mean_co2_y - mean_co2_x)/mean_co2_x,
         percent_change = round(percent_change, 2))  

co2_summary_full_wide = 
  co2_summary_full %>% 
  dplyr::select(Condition_x, Condition_y, percent_change) %>% 
  pivot_wider(names_from = "Condition_y", values_from = "percent_change")

co2_summary_full %>% 
  mutate(percent_change = round(percent_change)) %>% 
  filter(Hours == "94") %>% 
  ggplot(aes(x = Condition_x, y = Condition_y,
             fill = percent_change))+
  geom_tile()+
  coord_equal()+
  geom_text(aes(label = percent_change), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradient2(low = "firebrick", high = "steelblue1", mid = "white")+
  labs(x = "Condition",
       y = "Reference condition",
       fill = "% change in CO2",
       title = "CO2 - comparisons",
       subtitle = "94 hours",
       caption = "colors represent percent change of a given condition compared to the reference. 
       + values indicate an increase, 
       - values indicate a decrease")+
  facet_wrap(~substrate)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
# NOVACYTE 

# NOVA summary heatmaps ----
nova_summary = 
  nova_samples %>% 
  group_by(Condition, Hours, substrate) %>% 
  dplyr::summarise(mean_abs = mean(Absorbance_bl_corrected)) %>% 
  order_conditions()

# plot all hours
nova_summary %>% 
  # filter(Hours %in% c("68", "139")) %>% 
  mutate(mean_abs = round(mean_abs)) %>% 
  ggplot(aes(x = Hours, y = Condition, fill = mean_abs))+
  geom_tile(alpha = 1, color = "black")+
  coord_equal()+
  geom_text(aes(label = mean_abs), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("crait", 5))+
  ##  scale_fill_stepsn(colors = soilpalettes::soil_palette("redox2", 12),
  ##                    breaks = c(0, 2000, 5000, 10000, 40000, 50000))+
  facet_wrap(~substrate, nrow = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())
#  guides(fill = guide_legend(override.aes = list(alpha=0.9)))


# plot only 94, 163 hr
nova_summary %>% 
  filter(Hours %in% c("96", "168")) %>% 
  mutate(mean_abs = round(mean_abs)) %>% 
  ggplot(aes(x = Hours, y = Condition, fill = mean_abs))+
  geom_tile(alpha = 0.8, color = "black")+
  coord_equal()+
  geom_text(aes(label = mean_abs), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("redox2", 5))+
  ##  scale_fill_stepsn(colors = soilpalettes::soil_palette("redox2", 12),
  ##                    breaks = c(0, 2000, 5000, 10000, 40000, 50000))+
  #  scale_fill_gradient(low = "grey90", high = "steelblue2")+                    
  facet_wrap(~substrate, nrow = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())

#
# NOVA comparison heatmaps ----

nova_summary_full = 
  nova_summary %>% 
  rename(Condition_x = Condition,
         mean_abs_x = mean_abs) %>% 
  full_join(nova_summary %>% 
              rename(Condition_y = Condition,
                     mean_abs_y = mean_abs)) %>% 
  mutate(percent_change = 100*(mean_abs_y - mean_abs_x)/mean_abs_x,
         percent_change = round(percent_change, 2))  

nova_summary_full_wide = 
  nova_summary_full %>% 
  dplyr::select(Condition_x, Condition_y, percent_change) %>% 
  pivot_wider(names_from = "Condition_y", values_from = "percent_change")

nova_summary_full %>% 
  mutate(percent_change = round(percent_change)) %>% 
  filter(Hours == "96") %>% 
  ggplot(aes(x = Condition_x, y = Condition_y,
             fill = percent_change))+
  geom_tile()+
  coord_equal()+
  geom_text(aes(label = percent_change), color = "black", size = 3.5, fontface = "bold")+
  scale_fill_gradient2(low = "firebrick", high = "steelblue1", mid = "white")+
  labs(x = "Condition",
       y = "Reference condition",
       fill = "% change in Absorbance",
       title = "Novacyte - comparisons",
       subtitle = "96 hours",
       caption = "colors represent percent change of a given condition compared to the reference. 
       + values indicate an increase, 
       - values indicate a decrease")+
  facet_wrap(~substrate)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#
# NOVACYTE 


