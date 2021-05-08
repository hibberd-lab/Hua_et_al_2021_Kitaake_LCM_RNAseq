library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)

data <- read.csv("Figure_1N_kmeanclusters.csv")

data$cluster <- factor(data$cluster, levels = c("CM","CBS","CV","CBS&M","CBS&V","CM&V"))
head(data)
row.names(data) <- data$GENEID
mat <- data[, 2:4]

ht <- Heatmap(mat, split = data$cluster, gap = unit(2, "mm"),
        col = colorRamp2(c(-1.2, 0, 1.2), c("blue","white", "red")),
        cluster_rows = T, cluster_columns = F, show_row_names = F, 
        clustering_distance_rows = "pearson",
        clustering_method_rows = "complete",
        cluster_row_slices = FALSE,
        row_names_gp=gpar(fontsize = 1),
        show_row_dend = F,
        column_names_gp = gpar(fontsize = 15, fontface = "bold"),
        column_names_side = "top", 
        column_names_rot = 0,
        row_names_side = "left",
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
