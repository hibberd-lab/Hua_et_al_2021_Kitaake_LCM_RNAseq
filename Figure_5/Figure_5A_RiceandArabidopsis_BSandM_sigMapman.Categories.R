library(ggplot2)
# bubble plot
data <- read.delim("Figure_5A_RiceandArabidopsis_BSandM_sigMapman.Categories.txt", header = TRUE)
head(data)
data$logFDR<- -log10(data$FDR)
data$Category <- as.character(unique(data$Category))
data$Category <- factor(data$Category, rev(unique(data$Category)))
data$cell <- factor(data$cell, unique(data$cell))

ggplot(data, aes(x=cell, y=Category,color=logFDR)) +
  geom_point(aes(size = Gene.count)) +
  scale_size(breaks=c(10,50,100)) +
  scale_color_gradient(low="blue", high="red", limit = c(1, 20), 
                       breaks = c(1, 5, 10, 15, 20), labels = c("1", "5", "10", "15", ">20"), na.value="red")+
  theme_light() +
  theme(panel.border=element_rect(fill='transparent', color='black', size=1),
        plot.title = element_text(color="black", size=14, hjust=0.5, lineheight=1),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, vjust=1.5, face="bold"),
        axis.text.x = element_text(size=12,color="black",angle = 45, vjust = 1.05, hjust = 1),
        axis.text.y = element_text(size=12,color="black"),
        legend.text = element_text(color="black", size=10, hjust=0),
        legend.position="right") +
  labs(x="",y="",size="Count", title="",color=expression('-Log'[10]*'(FDR)'), face="bold")
