data <- read.csv("Supplemental_Figure3A_transporters.enrichment.CSV")
head(data)
data$logFDR<- -log10(data$FDR)
data$Category <- as.character(unique(data$Category))
data$Category <- factor(data$Category, rev(unique(data$Category)))

ggplot(data, aes(x=cluster, y=Category,color=logFDR)) +
  geom_point(aes(size = count)) +
  scale_size(breaks=c(5,10)) +
  scale_x_discrete(limits = c("CM", "CBS", "CV", "CBS&M", "CBS&V", "CM&V"), 
                   labels=c("CM\n613", "CBS\n285", "CV\n972", "CBS&M\n1136", "CBS&V\n1015",  "CM&V\n134")) +
  scale_color_gradient(low="blue", high="red", limit = c(1, 5), 
                       breaks = c(0, 3, 5), labels = c("0", "3", "5"), na.value="red")+
  theme_light() +
  theme(panel.border=element_rect(fill='transparent', color='black', size=1),
        plot.title = element_text(color="black", size=14, hjust=0.5, face="bold", lineheight=1),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, vjust=1.5, face="bold"),
        axis.text.x = element_text(size=12,color="black",face="bold"),
        axis.text.y = element_text(size=12,color="black",face="bold"),
        legend.text = element_text(color="black", size=10, hjust=0),
        legend.position="right") +
  labs(x="",y="",size="Count", title="",color=expression('-Log'[10]*'(FDR)'), face="bold")

