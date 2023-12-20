
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
#ggsave("3-images/figures_2023-10-20/co2_heatmap_summary.png", height = 5, width = 17)


# plot only 94, 163 hr
co2_summary %>% 
  filter(Hours %in% c("96", "168")) %>% 
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
  dplyr::select(substrate, Hours, Condition_x, Condition_y, percent_change) %>% 
  pivot_wider(names_from = "Condition_y", values_from = "percent_change")

plot_co2_comparison_heatmap = function(co2_summary_full){
  
  co2_summary_full %>% 
    mutate(percent_change = round(percent_change)) %>% 
    filter(Hours == "96") %>% 
    ggplot(aes(x = Condition_x, y = Condition_y,
               fill = percent_change))+
    geom_tile()+
    coord_equal()+
    geom_text(aes(label = percent_change), color = "black", size = 5, fontface = "bold")+
    scale_fill_gradient2(low = "firebrick", high = "steelblue1", mid = "white")+
    labs(x = "Condition",
         y = "Reference condition",
         fill = "% change in CO2",
         title = "CO2 - comparisons",
         subtitle = "96 hours")+
    facet_wrap(~substrate)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          plot.caption = element_text(size = 14, hjust = 0.5,face = "italic"))+
    theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=14), #change legend title font size
          legend.text = element_text(size=14)) #change legend text font size
  
}

co2_comparisons_all = plot_co2_comparison_heatmap(co2_summary_full)+
  labs(caption = "colors represent percent change of a given condition compared to the reference. 
      percentage is the y axis / x axis 
      No 15C data for CMC and Chitin due to unreliable tempature
       + values indicate an increase, 
       - values indicate a decrease")
co2_comparisons_chitin = plot_co2_comparison_heatmap(co2_summary_full %>% filter(substrate == "Chitin"))
co2_comparisons_cmc = plot_co2_comparison_heatmap(co2_summary_full %>% filter(substrate == "CMC"))
co2_comparisons_nag = plot_co2_comparison_heatmap(co2_summary_full %>% filter(substrate == "NAG"))
co2_comparisons_trehalose = plot_co2_comparison_heatmap(co2_summary_full %>% filter(substrate == "Trehalose"))

#ggsave("3-images/figures_2023-10-20/co2_heatmap_comparisons.png")


#
# NOVACYTE 

# NOVA summary heatmaps ----
nova_summary = 
  nova_samples %>% 
  group_by(Condition, Hours, substrate) %>% 
  dplyr::summarise(mean_abs = mean(Absorbance)) %>% 
  #dplyr::summarise(mean_abs = mean(Absorbance_bl_corr)) %>% 
  #if we want plank corrections
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
#ggsave("3-images/figures_2023-10-20/nova_heatmap_summary.png", height = 5, width = 18)


# plot only 94, 163 hr
nova_summary %>% 
  filter(Hours %in% c("96", "168")) %>% 
  mutate(mean_abs = round(mean_abs)) %>% 
  ggplot(aes(x = Hours, y = Condition, fill = mean_abs))+
  geom_tile(alpha = 0.8, color = "black")+
  coord_equal()+
  geom_text(aes(label = mean_abs), color = "black", size = 5, fontface = "bold")+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("redox2", 5))+
  ##  scale_fill_stepsn(colors = soilpalettes::soil_palette("redox2", 12),
  ##                    breaks = c(0, 2000, 5000, 10000, 40000, 50000))+
  #  scale_fill_gradient(low = "grey90", high = "steelblue2")+                    
  facet_wrap(~substrate, nrow = 1)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0.5,face = "italic"))+
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size

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
  dplyr::select(substrate, Hours, Condition_x, Condition_y, percent_change) %>% 
  pivot_wider(names_from = "Condition_y", values_from = "percent_change")

plot_nova_comparison_heatmap = function(nova_summary_full){
  nova_summary_full %>% 
    mutate(percent_change = round(percent_change)) %>% 
    filter(Hours == "96") %>% 
    ggplot(aes(x = Condition_x, y = Condition_y,
               fill = percent_change))+
    geom_tile()+
    coord_equal()+
    geom_text(aes(label = percent_change), color = "black", size = 5, fontface = "bold")+
    scale_fill_gradient2(low = "firebrick", high = "steelblue1", mid = "white")+
    labs(x = "Condition",
         y = "Reference condition",
         fill = "% change in Absorbance",
         title = "Novacyte - comparisons",
         subtitle = "96 hours"
         )+
    
    facet_wrap(~substrate)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
nova_comparisons_all = plot_nova_comparison_heatmap(nova_summary_full)+
  labs(caption = "colors represent percent change of a given condition compared to the reference. 
      percentage is the y axis / x axis
       + values indicate an increase, - values indicate a decrease,
       No 15C data for CMC and Chitin due to unreliable tempature")
nova_comparisons_chitin = plot_nova_comparison_heatmap(nova_summary_full %>% filter(substrate == "Chitin"))
nova_comparisons_cmc = plot_nova_comparison_heatmap(nova_summary_full %>% filter(substrate == "CMC"))
nova_comparisons_nag = plot_nova_comparison_heatmap(nova_summary_full %>% filter(substrate == "NAG"))
nova_comparisons_trehalose = plot_nova_comparison_heatmap(nova_summary_full %>% filter(substrate == "Trehalose"))
#ggsave("3-images/figures_2023-10-20/nova_heatmap_comparisons.png")


#
# NOVACYTE 



#
# combined CO2 and Nova ----
# 
combined = 
  co2_summary %>%  
  left_join(nova_summary)

# plot CO2 vs. nova
combined %>% 
  ggplot(aes(y = mean_co2, x = mean_abs, color = Hours))+
  geom_point(size = 3)+
  geom_path(aes(group = Condition), color = "black")+
  labs(x ="Cell counts",
       y = "CO2 (ppm)",
       title = "CO2 Vs Biomass",
      # subtitle = "",
       caption = "all substrates resperation and biomass Correlation")+
  facet_wrap(~substrate+Condition)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14))+
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
       legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size
#ggsave("3-images/figures_2023-10-20/co2_vs_nova_all.png")

combined %>% 
  filter(substrate %in% c("CMC", "Chitin")) %>% 
  ggplot(aes(y = mean_co2, x = mean_abs, color = Hours))+
  geom_point(size = 5)+ 
  geom_path(aes(group = Condition), color = "black")+
  labs(x ="Cell counts",
       y = "CO2 (ppm)",
    title = "CO2 and Biomass",
       # subtitle = "",
       caption = "Chitin and CMC resperation Vs biomass
       correlation
    no blank corrections")+
  facet_wrap(~substrate+Condition)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0.5,face = "italic"))+
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size
#ggsave("3-images/figures_2023-10-20/co2_vs_nova_cmc_chitin.png")

combined %>% 
  filter(!substrate %in% c("CMC", "Chitin")) %>% 
  ggplot(aes(y = mean_co2, x = mean_abs, color = Hours))+
  geom_point(size = 5)+
  geom_path(aes(group = Condition), color = "black")+
  labs(x ="Cell counts",
       y = "CO2 (ppm)",
       title = "CO2 and Biomass",
       # subtitle = "",
       caption = "resperation Vs biomass
       correlation
       No Blank corrections")+
  facet_wrap(~substrate+Condition)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.caption = element_text(size = 14, hjust = 0.5,face = "italic"))+
  theme(legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size
 # geom_line( color = "grey")
#ggsave("3-images/figures_2023-10-20/co2_vs_nova_nag_treh.png")

