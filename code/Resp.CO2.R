library(ggplot2)

raw_data <- read.csv("CMC_Condition_CO2_R.EXP.csv")

data <- raw_data
data$Time <- as.numeric(sapply(data$Hrs.of.samples, function(x){unlist(strsplit(x,":"))[1]}, USE.NAMES = FALSE))
data$Condition <- sapply(data$name,
                         function(x){unlist(strsplit(x, "_"))[1]}, USE.NAMES = FALSE)

ggplot(data, aes(x = as.factor (Time), y = CO2_ppm, fill = Condition))+
  stat_summary(geom = "bar", position = "dodge")+
  scale_x_discrete(name = "Time (Hours)"
  theme_bw()
  