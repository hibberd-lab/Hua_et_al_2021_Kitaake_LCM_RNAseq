data <- read.csv("Supplemental_Figure6B_TF.enrichment.CSV")
data$logFDR<- -log10(data$FDR)
data$Family <- as.character(unique(data$Family))
data$Family <- factor(data$Family, rev(unique(data$Family)))
data

#dot plot
ggplot(data, aes(x=cluster, y=Family,color=logFDR)) +
  geom_point(aes(size = count)) +
  scale_size(breaks=c(1,3,5)) +
  scale_x_discrete(limits = c("TFM", "TFBS", "TFV", "TFBS&M", "TFBS&V", "TFM&V")) +
  scale_color_gradient(low="blue", high="red", limit = c(1, 5), 
                       breaks = c(1, 3, 5), labels = c("1", "3", "5"), na.value="red")+
  theme_light() +
  theme(panel.border=element_rect(fill='transparent', color='black', size=1),
        plot.title = element_text(color="black", size=14, hjust=0.5, face="bold", lineheight=1),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, vjust=1.5, face="bold"),
        axis.text.x = element_text(size=12,color="black",face="bold"),
        axis.text.y = element_text(size=12,color="black",face="bold"),
        legend.text = element_text(color="black", size=10, hjust=0),
        legend.position="right") +
  labs(x="",y="",size="GeneRatio", title="",color=expression('-Log'[10]*'(FDR)'), face="bold")


