library(ggplot2)
library(tidyverse)
library(gplots)
library(viridis)

data <- read.csv("Figure_4B_motif.enrichment.csv")
data$Motif <- factor(data$Motif, levels=rev(c(data$Motif)))

data[data == 0] <- NA
data_long <- pivot_longer(data, -Motif)
data_long$name <- factor(data_long$name, levels = c("CM", "CBS", "CV", "CBS.M", "CBS.V", "CM.V"))
data_long <- drop_na(data_long)

ggplot(data_long, aes(name, Motif)) +
  geom_point(aes(colour = value), size = 3)+
  scale_color_viridis(option="E", direction = -1)+
  theme(axis.text.x = element_text(angle = 45))+
  scale_y_discrete(position = "left")+
  theme_bw()
