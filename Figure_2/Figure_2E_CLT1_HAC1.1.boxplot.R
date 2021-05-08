setwd("")

library(ggplot2)
require(reshape2)


tpm <- read.csv("KitaakeLCM_TPM.csv", header=TRUE)
list <- read.delim("Figure_2E_CLT1_HAC1.1.txt", header = TRUE)
list$Symbol <- as.character(list$Symbol)
list$Symbol <- factor(list$Symbol, levels = list$Symbol)

sub <- merge(tpm, list, by="GENEID")
row.names(sub) <- sub$Symbol
sub <- sub[,2:15]

tv <- t(sub)

cell_type <- c("BS","BS","BS","BS","BS","M","M","M", "M","M","V","V","V", "V")
tv <- as.data.frame(tv)
tv$cell_type <- cell_type
head(tv)

tv$cell_type <- as.factor(tv$cell_type)
levels(tv$cell_type)
tv$cell_type <- factor(tv$cell_type, levels(tv$cell_type)[c(2,1,3)])
levels(tv$cell_type)

melt <- melt(tv)
melt$variable <- factor(melt$variable, levels=list$Symbol)
levels(melt$variable)
head(melt)
melt <- as.data.frame(melt)

p<- ggplot(melt, aes(x=cell_type, y=value, fill=cell_type)) + 
  geom_boxplot()+
  expand_limits(y = 0)+
  facet_wrap (~variable, scale = "free", nrow = 1,labeller = label_wrap_gen(5))+
  labs(title="", x="cell type", y = "TPM")+
  scale_fill_manual(name = "Cell Type", labels = c("M", "BS", "V"),
                    values = c("#00CC33","#FF3333","#3399FF")) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold", colour = "black"), 
        strip.text.x = element_text(size = 10, colour = "black", angle = 0,face = "italic",hjust=0), 
        legend.position="bottom", legend.title = element_blank(),
        legend.text = element_text(colour="black", size=15), 
        panel.grid.major = element_line(), panel.grid.minor = element_line(),
        panel.background = element_blank(), 
        strip.background = element_blank(),
        aspect.ratio = 1, 
        axis.line = element_line(colour = "black"))
p  
