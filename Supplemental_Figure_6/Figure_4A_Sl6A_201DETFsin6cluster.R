setwd("F:/Lei_Owncloud/01 C4 rice Project/01 Kitaake WT LCM manuscript/0001dataandfigures/Figure 2/code")
library(ComplexHeatmap)
library(circlize)
library(dendextend)

data <- read.delim("Figure_4A_201DETFs_in6clusters.txt", header = TRUE)
head(data)
data$TFcluster <- factor(data$TFcluster, levels = c("TFM","TFBS","TFV","TFBS&M", "TFBS&V","TFM&V"))
row.names(data) <- data$GENEID
mat <- data[, 2:4]

ht <- Heatmap(mat, split = data$TFcluster, gap = unit(2, "mm"),
               col = colorRamp2(c(-1.2, 0, 1.2), c("blue","white", "red")),
               cluster_rows = T, cluster_columns = F, show_row_names = T, 
               clustering_distance_rows = "pearson",
               clustering_method_rows = "complete",
               cluster_row_slices = FALSE,
               row_names_gp=gpar(fontsize = 6),
               show_row_dend = F,
               column_names_gp = gpar(fontsize = 15),
               column_names_side = "top", 
               column_names_rot = 0,
               row_names_side = "right",
               row_names_max_width = unit(20, "cm"),
               row_dend_reorder = T,
               row_dend_width = unit(15, "mm"),
               heatmap_legend_param = list(title = expression('Z-score'), 
                                           title_gp = gpar(fontsize = 12),
                                           at = c(-1.2, 0, 1.2),
                                           legend_direction = "horizontal",
                                           title_position = "topcenter",
                                           grid_border = NULL,
                                           border = "black",
                                           grid_width = unit(0.1, "cm"),
                                           legend_height = unit(1, "cm"), 
                                           legend_width = unit(2, "cm"),
                                           labels_gp = gpar(fontsize = 12)))

draw(ht, heatmap_legend_side = "bottom")

#line plot
head(data)
data1 <- data[, 1:5]

m_te_m <- melt(data1, id.vars = c("GENEID", "TFcluster"))
head(m_te_m)
summary(m_te_m)
m_te_m$variable <- factor(m_te_m$variable, levels = c("M","BS","V"))

ggplot(m_te_m, aes(variable, value, group= GENEID))+
  scale_y_discrete(position = "right")+
  geom_line(alpha=0.1)+
  stat_summary(aes(group=TFcluster),
               fun.y=mean, geom="line", size=1, colour="red")+
  facet_wrap(~TFcluster, ncol = 1)+
  xlab("")+
  ylab("Z-score")+
  theme_bw()+
  theme(axis.text=  element_text(colour="black", size=12),
        axis.title = element_text(colour = "black", size=12),
        strip.text.x = element_text(colour="black", size=12), 
        aspect.ratio = 0.9, legend.title = element_blank())


#summarize TF family_SupplementalFigure6A

data2 <- data[, c(5, 6)]
as.data.frame.matrix(t(table(data2)))
write.csv(as.data.frame.matrix(t(table(data2))), "Supplemental_Figure_6A_summaryof_TFfamilies.csv")
















