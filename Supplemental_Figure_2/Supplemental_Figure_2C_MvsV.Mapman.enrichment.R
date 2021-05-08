setwd("F:/Lei_Owncloud/01 C4 rice Project/01 Kitaake WT LCM manuscript/0001dataandfigures/Figure 1/code")
data <- read.csv("Supplemental_Figure2C_MvsV.Mapman.enrichment.csv", header = TRUE)
data$Category <- as.character(unique(data$Category))
data$Category <- factor(data$Category, rev(unique(data$Category)))

ggplot(data, aes(x=cell_type, y=Category,color=logFDR)) +
  geom_point(aes(size = GeneRatio)) +
  scale_size(breaks=c(0.01,0.03,0.05,0.1, 0.15, 0.2)) +
  scale_color_gradient(low="blue", high="red", limit = c(1, 30), na.value="red")+
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

