source("2-code/processing_nova_co2.R")

# blanks <- dplyr::filter(nova_processed, Hours == 0)|>
#   dplyr::select(substrate, Condition, Replicate, Hours, Absorbance)
# 
final <- dplyr::filter(nova_processed, Hours == 96)|>
  dplyr::select(substrate, Condition, Replicate, Hours, Absorbance)

ggplot(final, aes(Condition, Absorbance, fill = substrate))+
  geom_col(position = "dodge")


stats_function <- function(sub){
  df <- dplyr::filter(final, substrate == sub)
  anova <- aov(Absorbance ~ Condition, data = df)
  broom::tidy(TukeyHSD(anova))
}

chitin_stats <- stats_function("Chitin") 
nag_stats <- stats_function("NAG")
tre_stats <- stats_function("Trehalose")
cmc_stats <- stats_function("CMC")




co2_data <- dplyr::filter(co2_processed, Hours == 96)|>
  dplyr::select(substrate, Condition, Replicate, Hours, co2_ppm)

ggplot(final, aes(Condition, co2_ppm, fill = substrate))+
  geom_col(position = "dodge")


co2_stats_function <- function(sub){
  df <- dplyr::filter(co2_data, substrate == sub)
  anova <- aov(co2_ppm ~ Condition, data = df)
  broom::tidy(TukeyHSD(anova))
}

chitin_co2_stats <- co2_stats_function("Chitin") 
nag_co2_stats <- co2_stats_function("NAG")
tre_co2_stats <- co2_stats_function("Trehalose")
cmc_co2_stats <- co2_stats_function("CMC")

