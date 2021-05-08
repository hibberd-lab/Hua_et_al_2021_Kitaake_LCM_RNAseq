library(ComplexHeatmap)
library(circlize)

norm <- read.csv("KitaakeLCM_normaliseddata.csv", header = TRUE)
list <- read.delim("Supplemental_Figure3B_VandPtypeATPase.genelist.txt", header = TRUE)
data <- merge(norm, list, by = "GENEID")
head(data)
data1 <- data[, c(2,3,4)]
row.names(data1) <- data$Symbol
t_data1 <- t(data1)
scale_t_data1<-scale(t_data1, center = TRUE, scale = TRUE)
data1<-t(scale_t_data1)
data1 <- data.matrix(data1)


row_dend = as.dendrogram(hclust(dist(data1)))
ht <- Heatmap(data1, 
              col = colorRamp2(c(-1.2, 0, 1.2), c("blue","white", "red")),
              cluster_rows =row_dend, cluster_columns = F, show_row_names = T,
              row_names_gp=gpar(fontsize = 14, fontface = "italic"),
              cluster_row_slices = FALSE,
              rect_gp = gpar(col = "black"),
              column_names_gp = gpar(fontsize = 14, fontface =  "bold"),
              column_names_side = "top", 
              column_names_rot = 0,
              row_names_side = "right",
              row_gap = unit(2, "mm"),
              row_title_gp = gpar(fontsize = 14, fontface = "bold"),
              row_title_rot = 0, show_row_dend = T,
              row_names_max_width = unit(500, "mm"),
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

