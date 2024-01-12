
# run the `processing_nova_co2.R` script first
# you will need `co2_samples` and `nova_samples` dataframes. 
# these include just the samples, no blanks

source("2-code/processing_nova_co2.R")

# STATS - compare each treatment against the "Control"
# we will use the Dunnett Test from the `DescTools` package

fit_dunnett_co2 <- function(dat) {
  
  # the dunnett test is being calculated to determine significant differences from the control
  # and we will plot this in the bar graph
  # where significant, it will be denoted with an asterisk in the plot
  
  d <-DescTools::DunnettTest(co2_ppm ~ Condition, control = "Control", 
                             data = dat)
  
  # create a dataframe and then clean up
  # create a new column "asterisk" that will have an asterisk for significant differences
  dunnett = d$Control %>% 
    as.data.frame() %>% 
    rownames_to_column("Condition") %>% 
    mutate(pval = round(pval, 3),
           Condition = str_remove(Condition, "-Control"),
           asterisk = case_when(pval <= 0.05 ~ "*")) %>% 
    dplyr::select(Condition, pval, asterisk)
  
  # next, we need to determine the y-axis position for the asterisks
  # the bar heights are highly variable, so we customize it
  # we will set the position as 10% above the maximum value for each bar
  height = 
    dat %>% 
    group_by(Condition) %>% 
    dplyr::summarise(max = max(co2_ppm),
                     y = max + (0.10 * max))
  
  # now, combine both dataframes
  # and order the conditions
  height %>% 
    left_join(dunnett) %>% 
    order_conditions()
  
}
co2_dunnett = 
  co2_samples %>%
  group_by(substrate, Hours) %>%
  do(fit_dunnett_co2(.))

# now plot the bar graph, but with the Dunnett asterisks
# modifying the function from `processing_nova_co2.R` script
plot_co2_dunnett = function(co2_samples, co2_dunnett){
  #gg_co2_chitin_no_corr <- 
  co2_samples |> 
    #filter(substrate == "CMC") %>% 
    ggplot(aes(x = Hours, y = co2_ppm, fill = Condition))+
    stat_summary(geom = "bar", position = "dodge")+
    stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
    # if you want to plot the actual data points, use this line below: 
    # geom_point(color = "black", position = position_dodge(width = 0.9))+
    expand_limits(x = 0)+
    scale_x_discrete(drop = F)+
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~substrate, scales = "free_y")+
    labs(title = "CO2",
         #subtitle = "Figure 1",
         x = "Time (hours)",
         y = "CO2 (ppm)")+
    geom_text(data = co2_dunnett, 
              aes(label = asterisk, y = y, group = Condition, color = Condition,
                  ), 
              position = position_dodge(width = 0.9), size = 10)+
    scale_fill_brewer(palette = "Paired", drop = F)+
    scale_color_brewer(palette = "Paired", drop = F)+
    theme(axis.title = element_text(size = 22),
          axis.text = element_text(size = 22),
          strip.text = element_text(size = 22),
          legend.text = element_text(size = 14)
    )
  
}

gg_co2_cmc = plot_co2_dunnett(co2_samples %>% filter(substrate == "CMC"), co2_dunnett %>% filter(substrate == "CMC"))
gg_co2_chitin = plot_co2_dunnett(co2_samples %>% filter(substrate == "Chitin"), co2_dunnett %>% filter(substrate == "Chitin"))
gg_co2_nag = plot_co2_dunnett(co2_samples %>% filter(substrate == "NAG"), co2_dunnett %>% filter(substrate == "NAG"))
gg_co2_trehalose = plot_co2_dunnett(co2_samples %>% filter(substrate == "Trehalose"), co2_dunnett %>% filter(substrate == "Trehalose"))






##Nova_STATS

fit_dunnett_nova <- function(dat) {
  
  # the dunnett test is being calculated to determine significant differences from the control
  # and we will plot this in the bar graph
  # where significant, it will be denoted with an asterisk in the plot
  
  d <-DescTools::DunnettTest(Absorbance ~ Condition, control = "Control", 
                             data = dat)
  
  # create a dataframe and then clean up
  # create a new column "asterisk" that will have an asterisk for significant differences
  dunnett = d$Control %>% 
    as.data.frame() %>% 
    rownames_to_column("Condition") %>% 
    mutate(pval = round(pval, 3),
           Condition = str_remove(Condition, "-Control"),
           asterisk = case_when(pval <= 0.05 ~ "*")) %>% 
    dplyr::select(Condition, pval, asterisk)
  
  # next, we need to determine the y-axis position for the asterisks
  # the bar heights are highly variable, so we customize it
  # we will set the position as 10% above the maximum value for each bar
  height = 
    dat %>% 
    group_by(Condition) %>% 
    dplyr::summarise(max = max(Absorbance),
                     y = max + (0.10 * max))
  
  # now, combine both dataframes
  # and order the conditions
  height %>% 
    left_join(dunnett) %>% 
    order_conditions()
  
}
nova_dunnett = 
  nova_samples %>%
  group_by(substrate, Hours) %>%
  do(fit_dunnett_nova(.))

# now plot the bar graph, but with the Dunnett asterisks
# modifying the function from `processing_nova_co2.R` script
plot_nova_dunnett = function(nova_samples, nova_dunnett){
  #gg_co2_chitin_no_corr <- 
  nova_samples |> 
    #filter(substrate == "CMC") %>% 
    ggplot(aes(x = Hours, y = Absorbance, fill = Condition))+
    stat_summary(geom = "bar", position = "dodge")+
    stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
    # if you want to plot the actual data points, use this line below: 
    # geom_point(color = "black", position = position_dodge(width = 0.9))+
    expand_limits(x = 0)+
    scale_x_discrete(drop = F)+
    scale_y_continuous(labels = scales::comma)+
    facet_wrap(~substrate, scales = "free_y")+
    labs(title = "Biomass",
         #subtitle = "Figure 1",
         x = "Time (hours)",
         y = "Cell Counts")+
    geom_text(data = nova_dunnett, 
              aes(label = asterisk, y = y, group = Condition, color = Condition,
              ), 
              position = position_dodge(width = 0.9), size = 10)+
    scale_fill_brewer(palette = "Paired", drop = F)+
    scale_color_brewer(palette = "Paired", drop = F)+
    theme(axis.title = element_text(size = 22),
          axis.text = element_text(size = 22),
          strip.text = element_text(size = 22),
          legend.text = element_text(size = 14)
    )
  
}

gg_nova_cmc = plot_nova_dunnett(nova_samples %>% filter(substrate == "CMC"), nova_dunnett %>% filter(substrate == "CMC"))
gg_nova_chitin = plot_nova_dunnett(nova_samples %>% filter(substrate == "Chitin"), nova_dunnett %>% filter(substrate == "Chitin"))
gg_nova_nag = plot_nova_dunnett(nova_samples %>% filter(substrate == "NAG"), nova_dunnett %>% filter(substrate == "NAG"))
gg_nova_trehalose = plot_nova_dunnett(nova_samples %>% filter(substrate == "Trehalose"), nova_dunnett %>% filter(substrate == "Trehalose"))








