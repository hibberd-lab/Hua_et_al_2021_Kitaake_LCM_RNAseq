#plot PCA using vsd transformed data
vsd <- read.csv("KitaakeLCM_vsd.csv")
row.names(vsd) <- vsd$GENEID
vsd <- vsd[,-1]

PCA <- prcomp(t(vsd),center = TRUE, scale = FALSE)
summary(PCA)
#proportion of variance 
#0.4614  0.1086  0.0747  0.06121  0.04864  0.04687  0.04082  0.03849  0.03528  0.03146  0.02087  0.0178  0.0138 0.000e+00

#Plot PCA using ggplot2
samples_PCA <- c("BS1","BS2","BS3","BS4","BS5",
                 "M1","M2","M3","M4","M5",
                 "V1","V2","V3","V4")

PCA_df <- data.frame(PC1 = PCA$x[,1], PC2 = PCA$x[,2],sample=samples_PCA)
PCA_df$celltype <- c(rep("BS", 5), rep("M", 5), rep("V", 4))
PCA_df$celltype <- factor(PCA_df$celltype, levels = c("M", "BS", "V"))


ggplot(PCA_df, aes(x = PC1, y= PC2, colour=celltype, shape = celltype)) +
  geom_point(size= 4, aes(shape = celltype)) +
  scale_colour_manual(name = "Cell Type", labels = c("M", "BS", "V"),
                      values = c("#009900","#FF3333","#3399FF"))+
  xlab("PC1 (46.1%)") + 
  ylab("PC2 (10.9%)")+ 
  theme(aspect.ratio = 1)+theme_bw() +
  theme(axis.text=element_text(size=14))
