#Figure 1J: visualize correlation using color gradient
library(dplyr)
library(ggplot2)
library(reshape2)

tpm <- read.csv("KitaakeLCM_TPM_15168genes.csv")
row.names(tpm) <- tpm$GENEID
tpm <- tpm[,-1]

cm <- melt(cor(log2(tpm+1), method = "spearman"))
colnames(cm) <- c("Var1", "Var2", "value")
head(cm)
cm$Var1 <- as.factor(cm$Var1)
cm$Var2 <- as.factor(cm$Var2)

ggplot(data=cm, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value)) +
  ggtitle("") +
  theme(text = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 90, color = "black", vjust = 0.5),
        axis.text.y = element_text(size=12, color = "black")) +
  xlab('') +
  ylab('') +
  scale_x_discrete(limits=c("M1","M2","M3", "M4","M5","BS1","BS2","BS3","BS4","BS5", "V1", "V2", "V3", "V4")) +
  scale_y_discrete(limits=rev(c("M1","M2","M3", "M4","M5", "BS1","BS2","BS3","BS4","BS5","V1", "V2", "V3", "V4"))) +
  scale_fill_gradient2(low="cyan", mid="yellow", high="red", midpoint=0.6, 
                       space="Lab", na.value = "red", guide="colorbar", 
                       name = "Spearman's correlation", 
                       limits= c(0.4, 0.8),
                       breaks = seq(0.4, 0.8, 0.1), 
                       labels = seq(0.4, 0.8, 0.1))

