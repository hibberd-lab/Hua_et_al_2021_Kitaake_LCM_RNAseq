# set working folder
setwd("F:/0001owncloud/01 C4 rice Project/01 Kitaake WT LCM manuscript/0001dataandfigures/Figure 1/code")

library(tximport)
library(readr)
#load the file that links transcripts to genes
Os_tx2gene <- read.csv ("Os_tx2gene.csv", header = TRUE)

#import transcript abundance from Salmon quantification result
files = list.files(pattern="*quant.sf")
files

#estimate gene-level abundance and counts
txi <-tximport(files, type = "salmon", tx2gene = Os_tx2gene)
names(txi)

#set sample names
samplename <- c("BS1","BS2","BS3","BS4","BS5","M1","M2","M3","M4","M5","V1","V2","V3","V4")

#export raw counts for DE analysis
counts<- txi$counts
colnames(counts)<- samplename
head(counts)
#export transcript abundance (TPM)
tpm <- txi$abundance
colnames(tpm) <-  samplename

write.csv(counts, "KitaakeLCM_rawcounts.csv")
write.csv(tpm, "KitaakeLCM_TPM.csv")


#Differential gene expression analysis using DESeq2
library(DESeq2)
sampleTable <- data.frame(celltype = c(rep("BS",5), rep("M", 5), rep("V", 4)))
rownames(sampleTable) <- colnames(counts)
sampleTable
dds <- DESeqDataSetFromTximport(txi, sampleTable, ~celltype)
dds <- estimateSizeFactors(dds)

# prefilter the data
idx<-rowSums(tpm>1) >=3
summary(idx) # 15168 genes left
dds <- dds[idx,]

#save TPM of the expressed 15168 genes
expr.tpm <- tpm[idx,]
write.csv(expr.tpm, "KitaakeLCM_TPM_15168genes.csv")

#apply variance stabilizing transformation to the count data
#transformed data was used for principle component analysis
vsd <- vst(dds, blind = TRUE)

write.csv(vsd, "KitaakeLCM_vsd.csv")
#Differential gene expression analysis- pairwise comparisons
dds <- DESeq(dds, betaPrior=FALSE)
bsvsm <-lfcShrink(dds, contrast=c("celltype", "BS", "M"), type = "ashr")
bsvsm$padj <- ifelse(is.na(bsvsm$padj), 1, bsvsm$padj)
summary(bsvsm)
mvsv <-lfcShrink(dds, contrast=c("celltype", "M", "V"), type = "ashr")
mvsv$padj <- ifelse(is.na(mvsv$padj), 1, mvsv$padj)
bsvsv <-lfcShrink(dds, contrast=c("celltype", "BS", "V"), type = "ashr")
bsvsv$padj <- ifelse(is.na(bsvsv$padj), 1, bsvsv$padj)

bsvsm$GENEID <- rownames(bsvsm)
mvsv$GENEID <- rownames(mvsv)
bsvsv$GENEID <- rownames(bsvsv)

#save results into file
library (dplyr)
DESeq.res <- data.frame(bsvsm)[,c(2,5,6)] %>%
  left_join(data.frame(mvsv)[,c(2,5,6)], by="GENEID" )%>%
  left_join(data.frame(bsvsv)[,c(2,5,6)], by="GENEID") 
names(DESeq.res) <- c("log2FoldChange.bsvsm", "padj.bsvsm", "GENEID",
                      "log2FoldChange.mvsv", "padj.mvsv", "log2FoldChange.bsvsv", "padj.bsvsv")
head(DESeq.res)

write.csv(DESeq.res, "KitaakeLCM_DESeq2_result.csv")
