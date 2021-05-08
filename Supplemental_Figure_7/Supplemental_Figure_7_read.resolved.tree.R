library("ape")
library("Biostrings")
library("ggplot2")
library("ggtree")

filename = list.files(pattern = "txt")
trees <- lapply(filename, read.tree)
str(trees)
class(trees) <- "multiPhylo"
names(trees) <- filename

pdf("Rplot_BS interested trees.pdf", width=30, height=60)
ggtree(trees) + 
  facet_wrap(~.id,  scales = "fixed",ncol=4) + 
  geom_treescale()+ 
  geom_tiplab()
dev.off()





