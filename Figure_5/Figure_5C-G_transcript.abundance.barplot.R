library(ggplot2)
library(tidyverse)
library(egg)
require(reshape2)
require(Rmisc)

data <- read.csv("Figure_6C-G_transcript.abundance.csv")
data$Symbol = as.character(data$Symbol)
data$Symbol <- factor(data$Symbol, levels = unique(data$Symbol))

ggplot(data, aes(Symbol, TPM, fill = cell, 
                 label = ifelse(is.DE == 1, "*", ""))) +
  geom_bar(stat="identity",position=position_dodge(width=1.1)) +
  scale_y_continuous(position = "left")+
  labs(x = "", y = "TPM") +
  geom_text(hjust = 0.1, size = 4) +
  facet_wrap (~Symbol, scale = "free", ncol=15, labeller = label_wrap_gen(5))+
  labs(title="", x="cell type", y = "TPM")+
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=10,face="bold", colour = "black"), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.x = element_blank(), 
        legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(colour="black", size=15), 
        panel.grid.major = element_line(), panel.grid.minor = element_line(),
        panel.background = element_blank(), 
        strip.background = element_blank(),
        aspect.ratio = 2, 
        axis.line = element_line(colour = "black"),
        panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0.5, "lines"))
