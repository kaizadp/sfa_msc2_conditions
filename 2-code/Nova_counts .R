library(ggplot2).
library(tidyverse)


# setup -------------------------------------------------------------------

## arrange order of experimental conditions
order_conditions = function(dat){
  dat |> 
    mutate(Condition = factor(Condition, levels = c("15C", "30C",
                                                    "pH 6", "pH 7.4",
                                                    "High Iron", "No Vit.Min.",
                                                    "Control", "Stagnant")))
}





# chitin ------------------------------------------------------------------
## CO2 Chitin----

co2_chitin <- read.csv("Chitin_CO2_20230217.csv")

co2_chitin_processed <- 
  co2_chitin |> 
  rename(Time_hr = `Hrs.of.samples`) |> 
  separate(name, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  separate(Time_hr, sep = ":", into = c("Hours")) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.numeric(Hours)) 
  
co2_chitin_blanks = 
  co2_chitin_processed |> 
  filter(Replicate == "blank") |> 
  dplyr::select(-Replicate, -name) |> 
  rename(blank_ppm = CO2_ppm)

co2_chitin_samples = 
  co2_chitin_processed |> 
  filter(!Replicate %in% "blank") |> 
  left_join(co2_chitin_blanks) |> 
  mutate(CO2_bl_corrected_ppm = CO2_ppm - blank_ppm) |> 
  order_conditions()
  # remove outliers
  # we know that the outliers are Control-C and 30C-A
filter(!(Condition %in% "NoVit.Min" & Replicate %in% "B")) |> 
  filter(!(Condition %in% "30C" & Replicate %in% "A"))

gg_co2_chitin_no_corr <- 
  ggplot(co2_chitin_samples |> filter(Replicate != "blank"), 
         aes(x = Hours, y = CO2_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "chitin - CO2",
       subtitle = "not blank-corrected")+
  scale_fill_brewer(palette = "Paired")

gg_co2_chitin_bl_corr <- 
  co2_chitin_samples |> 
  ggplot(aes(x = Hours, y = CO2_bl_corrected_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Chitin - CO2",
       subtitle = "blank-corrected")+
  scale_fill_brewer(palette = "Paired")



#
## Novacyte Chitin ----

nova_chitin <- read.csv("Chitin_novacyte_20230217.csv")

nova_chitin_processed = 
  nova_chitin |> 
  mutate(Time_hr = (`Plate.ID`-1) * 24) |> 
  separate(Sample, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate)) |> 
  rename(Absorbance = `All.Abs..Count`) |> 
  dplyr::select(Condition, Replicate, Absorbance, Time_hr)

nova_chitin_blank = 
  nova_chitin_processed |> 
  filter(Replicate == "blank") |> 
  rename(Blank = Absorbance) |> 
  dplyr::select(-Replicate)

nova_chitin_samples = 
  nova_chitin_processed |> 
  filter(!Replicate %in% "blank") |> 
  #group_by(Condition, Time_hr) |> 
  #dplyr::summarise(Absorbance = mean(Absorbance)) |> 
  # bring in blanks
  left_join(nova_chitin_blank) |> 
  mutate(Absorbance_bl_corrected = Absorbance - Blank) |> 
  order_conditions(.) |> 
  # remove outliers
  # we know that the outliers are Control-C and 30C-A
  filter(!(Condition %in% "Control" & Replicate %in% "C")) |> 
  filter(!(Condition %in% "30C" & Replicate %in% "A"))


gg_nova_Chitin_no_corr <- 
  nova_Chitin_samples |> 
  ggplot(aes(x = Time_hr, 
             y = Absorbance, fill = Condition,
             group = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  labs(title = "Chitin - NovaCyte",
       subtitle = "not blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")


gg_nova_chitin_blanks_only <- 
  nova_chitin_summary |> 
  ggplot(aes(x = Time_hr, 
           y = Blank, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Chitin - NovaCyte",
       subtitle = "blanks only")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

gg_nova_chitin_bl_corr <- 
  nova_chitin_summary |> 
  filter(!Time_hr %in% c(24, 48)) |> 
  ggplot(aes(x = Time_hr, 
           y = Absorbance_bl_corrected, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Chitin - NovaCyte",
       subtitle = "blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")



#
# NAG ---------------------------------------------------------------------

## CO2 NAG ----

co2_nag <- read.csv("NAG_CO2_EXP_R.csv")

co2_nag_processed <- 
  co2_nag |> 
  rename(Time_hr = `Hrs.of.samples`) |> 
  separate(name, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  separate(Time_hr, sep = ":", into = c("Hours")) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.numeric(Hours)) 

co2_nag_blanks = 
  co2_nag_processed |> 
  filter(Replicate == "blank") |> 
  dplyr::select(-Replicate, -name) |> 
  rename(blank_ppm = CO2_ppm)

co2_nag_samples = 
  co2_nag_processed |> 
  filter(!Replicate %in% "blank") |> 
  left_join(co2_nag_blanks) |> 
  mutate(CO2_bl_corrected_ppm = CO2_ppm - blank_ppm) |>
  order_conditions()

gg_co2_nag_no_corr <- 
  ggplot(co2_nag_samples |> filter(Replicate != "blank"), 
         aes(x = Hours, y = CO2_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "nag - CO2",
       subtitle = "not blank-corrected")+
  scale_fill_brewer(palette = "Paired")

gg_co2_nag_bl_corr <- 
  ggplot(co2_nag_samples |> order_conditions(.), 
       aes(x = Hours, y = CO2_bl_corrected_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "nag - CO2",
       subtitle = "blank-corrected")+
  scale_fill_brewer(palette = "Paired")


#
## Novacyte NAG----

nova_nag <- read.csv("NAG conditions_NovaCounts10.14.22 MG.csv")

nova_nag_processed = 
  nova_nag |> 
  mutate(Time_hr = (`Plate.ID`-1) * 24) |> 
  separate(Sample, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate)) |> 
  rename(Absorbance = `All.Abs..Count`) |> 
  dplyr::select(Condition, Replicate, Absorbance, Time_hr)

nova_nag_blank = 
  nova_nag_processed |> 
  filter(Replicate == "blank") |> 
  rename(Blank = Absorbance) |> 
  dplyr::select(-Replicate)

nova_nag_samples = 
  nova_nag_processed |> 
  filter(!Replicate %in% "blank") |> 
  # bring in blanks
  left_join(nova_nag_blank) |> 
  mutate(Absorbance_bl_corrected = Absorbance - Blank) |> 
  order_conditions(.)


gg_nova_nag_no_corr <- 
  ggplot(nova_nag_samples, 
       aes(x = Time_hr, 
           y = Absorbance, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  labs(title = "NAG - NovaCyte",
       subtitle = "not blank-corrected")+
  labs(x = "Hours", y = "Cell counts")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

gg_nova_nag_blanks_only <- 
  ggplot(nova_nag_summary, 
       aes(x = Time_hr, 
           y = Blank, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "NAG - NovaCyte",
       subtitle = "blanks only")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

gg_nova_nag_bl_corr <- 
  ggplot(nova_nag_summary, 
       aes(x = Time_hr, 
           y = Absorbance_bl_corrected, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "NAG - NovaCyte",
       subtitle = "blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

#
# combine graphs ----------------------------------------------------------
# install.packages(c("patchwork", "cowplot"))
library(patchwork)
library(cowplot)

# co2-blank corrected graphs
gg_co2_chitin_bl_corr + gg_co2_nag_bl_corr +
  plot_layout(guides = "collect")+
  plot_annotation(title = "CO2")


# nova blank-corrected graphs
gg_nova_chitin_bl_corr + gg_nova_nag_bl_corr +
  plot_layout(guides = "collect")+
  plot_annotation(title = "NovaCyte")






# Trehalose ------------------------------------------------------------------
## CO2 Tre----

co2_Trehalose <- read.csv("Trehalose_CO2_20221210.csv")

co2_Trehalose_processed <- 
  co2_Trehalose|>
  rename(Time_hr = `Hrs.of.samples`)|>
  separate(name, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  separate(Time_hr, sep = ":", into = c("Hours")) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.numeric(Hours)) 

co2_Trehalose_blanks = 
  co2_Trehalose_processed |> 
  filter(Replicate == "blank") |> 
  dplyr::select(-Replicate, -name) |> 
  rename(blank_ppm = CO2_ppm)

co2_Trehalose_samples = 
  co2_Trehalose_processed |> 
  filter(!Replicate %in% "blank") |> 
  left_join(co2_Trehalose_blanks) |> 
  mutate(CO2_bl_corrected_ppm = CO2_ppm - blank_ppm)

  # remove outliers
  # we know that the outliers are Control-C and 30C-A
 filter(!(Condition %in% "Control" & Replicate %in% "B")) |> 
  filter(!(Condition %in% "pH6" & Replicate %in% "C"))


 gg_co2_Trehalose_no_corr <- 
   ggplot(co2_Trehalose_samples |> filter(Replicate != "blank"), 
          aes(x = Hours, y = CO2_ppm, fill = Condition))+
   stat_summary(geom = "bar", position = "dodge")+
   stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
   #  geom_bar(stat = "identity", position = position_dodge())+
   expand_limits(x = 0)+
   scale_y_continuous(labels = scales::comma)+
   labs(title = "Trehalose - CO2",
        subtitle = "not blank-corrected")+
   scale_fill_brewer(palette = "Paired")
 

gg_co2_Trehalose_bl_corr <- 
  co2_Trehalose_samples |> 
  ggplot(aes(x = Hours, y = CO2_bl_corrected_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Trehalose - CO2",
       subtitle = "blank-corrected")+
  scale_fill_brewer(palette = "Paired")





#
## Novacyte Tre----

nova_Trehalose <- read.csv("Trehelose_conditions_NovaCounts12.10.22.csv")

nova_Trehalose_processed = 
  nova_Trehalose |> 
  mutate(Time_hr = (`Plate.ID`-1) * 24) |> 
  separate(Sample, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate)) |> 
  rename(Absorbance = `All.Abs..Count`) |> 
  dplyr::select(Condition, Replicate, Absorbance, Time_hr)

nova_Trehalose_blank = 
  nova_Trehalose_processed |> 
  filter(Replicate == "blank") |> 
  rename(Blank = Absorbance) |> 
  dplyr::select(-Replicate)

nova_Trehalose_samples = 
  nova_Trehalose_processed |> 
  filter(!Replicate %in% "blank") |> 
  #group_by(Condition, Time_hr) |> 
  #dplyr::summarise(Absorbance = mean(Absorbance)) |> 
  # bring in blanks
  left_join(nova_Trehalose_blank) |> 
  mutate(Absorbance_bl_corrected = Absorbance - Blank) 
  #order_conditions(.)
  # remove outliers
  # we know that the outliers are Control-C and 30C-A
 # filter(!(Condition %in% "Control" & Replicate %in% "C")) |> 
  #filter(!(Condition %in% "30C" & Replicate %in% "A"))


gg_nova_Trehalose_no_corr <- 
  nova_Trehalose_samples |> 
  ggplot(aes(x = Time_hr, 
             y = Absorbance, fill = Condition,
             group = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  labs(title = "Trehalose - NovaCyte",
       subtitle = "not blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")


gg_nova_Trehalose_blanks_only <- 
  nova_chitin_summary |> 
  ggplot(aes(x = Time_hr, 
             y = Blank, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Trehalose - NovaCyte",
       subtitle = "blanks only")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

gg_nova_Trehalose_bl_corr <- 
  nova_Trehalose_samples |>
  ggplot(aes(x = Time_hr, 
             y = Absorbance_bl_corrected, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "Trehalose - NovaCyte",
       subtitle = "blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")


# nova blank-corrected graphs
gg_nova_Trehalose_bl_corr + gg_nova_Trehalose_bl_corr +
  plot_layout(guides = "collect")+
  plot_annotation(title = "NovaCyte")

# Carboxymethylcellulose ------------------------------------------------
## C02 CMC----
co2_CMC <- read.csv("CMC_CO2_20230417.csv")

co2_CMC_processed <- 
  co2_CMC|>
  rename(Time_hr = `Hrs.of.samples`)|>
  separate(name, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  separate(Time_hr, sep = ":", into = c("Hours")) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate),
         Hours = as.numeric(Hours)) 

co2_CMC_blanks = 
  co2_CMC_processed |> 
  filter(Replicate == "blank") |> 
  dplyr::select(-Replicate, -name) |> 
  rename(blank_ppm = CO2_ppm)

co2_CMC_samples = 
  co2_CMC_processed |> 
  filter(!Replicate %in% "blank") |> 
  left_join(co2_CMC_blanks) |> 
  mutate(CO2_bl_corrected_ppm = CO2_ppm - blank_ppm)

# remove outliers
# we know that the outliers are No Vit.Min-A and 15C-A
filter(!(Condition %in% "No Vit.Min" & Replicate %in% "A")) |> 
  filter(!(Condition %in% "15C" & Replicate %in% "A"))



gg_co2_CMC_no_corr <- 
  ggplot(co2_CMC_samples |> filter(Replicate != "blank"), 
         aes(x = Hours, y = CO2_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "CMC - CO2",
       subtitle = "not blank-corrected")+
  scale_fill_brewer(palette = "Paired")

gg_co2_CMC_bl_corr <- 
  co2_CMC_samples |> 
  ggplot(aes(x = Hours, y = CO2_bl_corrected_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  #  geom_bar(stat = "identity", position = position_dodge())+
  expand_limits(x = 0)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "CMC - CO2",
       subtitle = "blank-corrected")+
  scale_fill_brewer(palette = "Paired")


## Novacyte CMC----

nova_CMC <- read.csv("CMC_novacyte_20230417.csv")

nova_CMC_processed = 
  nova_CMC |> 
  mutate(Time_hr = (`Plate.ID`-1) * 24) |> 
  separate(Sample, sep = "_", into = c("Condition", "Replicate"), remove = FALSE) |> 
  mutate(Replicate = if_else(Replicate == "D", "blank", Replicate)) |> 
  rename(Absorbance = `All.Abs..Count`) |> 
  dplyr::select(Condition, Replicate, Absorbance, Time_hr)

nova_CMC_blank = 
  nova_CMC_processed |> 
  filter(Replicate == "blank") |> 
  rename(Blank = Absorbance) |> 
  dplyr::select(-Replicate)

nova_CMC_samples = 
  nova_CMC_processed |> 
  filter(!Replicate %in% "blank") |> 
  #group_by(Condition, Time_hr) |> 
  #dplyr::summarise(Absorbance = mean(Absorbance)) |> 
  # bring in blanks
  left_join(nova_CMC_blank) |> 
  mutate(Absorbance_bl_corrected = Absorbance - Blank) 
#order_conditions(.)
# remove outliers
# we know that the outliers are Control-C and 30C-A
# filter(!(Condition %in% "Control" & Replicate %in% "C")) |> 
#filter(!(Condition %in% "30C" & Replicate %in% "A"))


gg_nova_CMC_no_corr <- 
  nova_CMC_samples |> 
  ggplot(aes(x = Time_hr, 
             y = Absorbance, fill = Condition,
             group = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  stat_summary(geom = "errorbar", position = "dodge", color = "grey40")+
  labs(title = "CMC - NovaCyte",
       subtitle = "not blank-corrected")+
  labs(x = "Hours", y = "Cell counts")
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")


gg_nova_CMC_blanks_only <- 
  nova_CMC_summary |> 
  ggplot(aes(x = Time_hr, 
             y = Blank, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "CMC - NovaCyte",
       subtitle = "blanks only")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

gg_nova_CMC_bl_corr <- 
  nova_CMC_samples |>
  ggplot(aes(x = Time_hr, 
             y = Absorbance_bl_corrected, fill = Condition))+
  geom_bar(stat = "identity", position = position_dodge())+
  labs(title = "CMC - NovaCyte",
       subtitle = "blank-corrected")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_brewer(palette = "Paired")

