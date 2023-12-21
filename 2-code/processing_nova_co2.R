

# 1. load packages --------------------------------------------------------
library(tidyverse)
theme_set(theme_bw(base_size = 14)+
            theme(axis.title = element_text(face = "bold")))

# setup -------------------------------------------------------------------

## arrange order of experimental conditions
order_conditions = function(dat){
  dat |> 
    mutate(Condition = factor(Condition, levels = c("15C", "30C",
                                                    "pH 6", "pH 7.4",
                                                    "High Iron", "No Vit.Min.",
                                                    "Control", "Stagnant")))
}
align_hours = function(dat){
  dat %>% 
    mutate(Hours = case_match(Hours, 
                              "21" ~ "24", "45" ~ "48", "46" ~ "48",
                              "68" ~ "72", "94" ~ "96",
                              "115" ~ "120", "119" ~ "120",
                              "139" ~ "144", "141" ~ "144", 
                              "163" ~ "168",
                              .default = Hours))
  
}

#

# import files --------------------------------------------------------------
import_nova_files <- function(FILEPATH){
  
  filepaths = list.files(path=FILEPATH, pattern = c("nova", ".csv"), full.names = TRUE)
  do.call(bind_rows, lapply(filepaths, function(path){
    df <- read_csv(path) %>% 
      mutate(source = basename(path))
    df
  }))
}
import_co2_files <- function(FILEPATH){
  
  filepaths = list.files(path=FILEPATH, pattern = c("CO2", ".csv"), full.names = TRUE)
  do.call(bind_rows, lapply(filepaths, function(path){
    df <- read_csv(path) %>% 
      mutate(source = basename(path),
             `Hrs of samples` = as.character(`Hrs of samples`))
    df
  }))
  
}

data_nova <- import_nova_files(FILEPATH = "1-data/nova_co2") %>% janitor::clean_names()
data_co2 <- import_co2_files(FILEPATH = "1-data/nova_co2") %>% janitor::clean_names()

#

# process CO2 data --------------------------------------------------------

co2_processed <- 
  data_co2 |> 
  rename(Time_hr = hrs_of_samples) |> 
  separate(name, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  separate(Time_hr, sep = ":", into = c("Hours")) |> 
  separate(source, sep = "_", into = "substrate", remove = FALSE) %>% 
  mutate(date_run = str_extract(source, "[0-9]{8}"),
         date_run = lubridate::ymd(date_run),
         Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.numeric(Hours),
         Hours = as.factor(Hours),
         #Hours = case_match(Hours, "45" ~ "46", "115" ~ "119", "141" ~ "139", .default = Hours),
         #Hours = str_sort(Hours, numeric = TRUE)
  ) %>% 
  align_hours() %>% 
  mutate(Hours_num = as.numeric(Hours),
         Hours = fct_reorder(Hours, Hours_num))

co2_blanks = 
  co2_processed |> 
  filter(Replicate == "blank") |> 
  dplyr::select(-Replicate, -name) |> 
  rename(blank_ppm = co2_ppm)

co2_samples = 
  co2_processed |> 
  filter(!Replicate %in% "blank") |> 
  left_join(co2_blanks) |> 
  mutate(CO2_bl_corrected_ppm = co2_ppm - blank_ppm) |> 
  order_conditions() %>% 
  #filter(!Condition %in% "15C") %>%  # removes all 15C
  filter(!((Condition == "15C" & substrate == "Chitin")|(Condition == "15C" & substrate == "CMC"))) %>% 
  force()

# plot CO2 data ----
# create function for CO2 graphs
plot_co2 = function(co2_samples){
  #gg_co2_chitin_no_corr <- 
  co2_samples |> 
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
         x = "Time, hours",
         y = "CO2, ppm")+
    scale_fill_brewer(palette = "Paired", drop = F)+
    scale_color_brewer(palette = "Paired", drop = F)+
    theme(#axis.title = element_text(size = 14),
          #axis.text = element_text(size = 14),
          #strip.text = element_text(size = 14)
          )
  
}

# now, plot
gg_co2_all = plot_co2(co2_samples)
#ggsave("3-images/figures_2023-10-20/co2_bar.png", width = 14, height = 10)
gg_co2_chitin = plot_co2(co2_samples %>% filter(substrate == "Chitin"))
gg_co2_CMC = plot_co2(co2_samples %>% filter(substrate == "CMC"))
gg_co2_NAG = plot_co2(co2_samples %>% filter(substrate == "NAG")) + 
  annotate("text", label = "NA", x = 1, y = 20000, size = 8)
gg_co2_trehalose = plot_co2(co2_samples %>% filter(substrate == "Trehalose"))



### #gg_co2_chitin_bl_corr <- 
### co2_samples |> 
###   ggplot(aes(x = as.character(Hours), y = CO2_bl_corrected_ppm, fill = Condition))+
###   stat_summary(geom = "bar", position = "dodge")+
###   #  geom_bar(stat = "identity", position = position_dodge())+
###   expand_limits(x = 0)+
###   scale_y_continuous(labels = scales::comma)+
###   facet_wrap(~source, scales = "free")+
###   labs(title = "Chitin - CO2",
###        subtitle = "blank-corrected")+
###   scale_fill_brewer(palette = "Paired")


#
# process Novacyte data ---------------------------------------------------

nova_processed = 
  data_nova |> 
  mutate(Time_hr = (plate_id-1) * 24) |> 
  separate(sample, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate)) |> 
  rename(Absorbance = all_abs_count) |> 
  dplyr::select(source, Condition, Replicate, Absorbance, Time_hr) %>% 
  rename(Hours = Time_hr) %>% 
  separate(source, sep = "_", into = "substrate", remove = FALSE) %>% 
  mutate(date_run = str_extract(source, "[0-9]{8}"),
         date_run = lubridate::ymd(date_run),
         Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.factor(Hours),
         #Hours = str_sort(Hours, numeric = TRUE)
  ) %>% 
  # align_hours() %>% 
  mutate(Hours_num = as.numeric(Hours),
         Hours = fct_reorder(Hours, Hours_num))


nova_blank = 
  nova_processed |> 
  filter(Replicate == "blank") |> 
  rename(Blank = Absorbance) |> 
  dplyr::select(-Replicate)

nova_samples = 
  nova_processed |> 
  filter(!Replicate %in% "blank") |> 
  #group_by(Condition, Time_hr) |> 
  #dplyr::summarise(Absorbance = mean(Absorbance)) |> 
  # bring in blanks
  left_join(nova_blank) |> 
  mutate(Absorbance_bl_corrected = Absorbance - Blank) |> 
  order_conditions() |> 
  # remove outliers
  # these outliers are for chitin: DO WE NEED TO REMOVE???
  # we know that the outliers are Control-C and 30C-A
  ###  filter(!(Condition %in% "Control" & Replicate %in% "C")) |> 
  ### filter(!(Condition %in% "30C" & Replicate %in% "A")) %>% 
  #filter(!Condition %in% "15C") %>%  # removes all 15C
   filter(!((Condition == "15C" & substrate == "Chitin")|(Condition == "15C" & substrate == "CMC"))) %>% 
  force()

#
# plot Novacyte data ----
plot_nova = function(nova_samples){
#gg_nova_no_corr <- 
  nova_samples |> 
  ggplot(aes(x = Hours, 
             y = Absorbance, fill = Condition,
             group = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  # if you want to plot the actual data points, use this line below: 
  # geom_point(color = "black", position = position_dodge(width = 0.9))+
  labs(title = "Biomass collection",
       #subtitle = "Figure 2",
       x = "Time, hours",
       y = "Cell counts")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")+
  facet_wrap(~substrate, scales = "free_y")+
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14))
}

gg_nova_all = plot_nova(nova_samples)
#ggsave("3-images/figures_2023-10-20/nova_bar.png", width = 14, height = 10)
gg_nova_chitin = plot_nova(nova_samples %>% filter(substrate == "Chitin"))
gg_nova_CMC = plot_nova(nova_samples %>% filter(substrate == "CMC"))
gg_nova_NAG = plot_nova(nova_samples %>% filter(substrate == "NAG"))
gg_nova_trehalose = plot_nova(nova_samples %>% filter(substrate == "Trehalose"))

