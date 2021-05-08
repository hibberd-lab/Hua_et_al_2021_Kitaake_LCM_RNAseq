#setwd()
#load the count data from file
counts <- read.csv("KitaakeLCM_rawcounts.csv")
row.names(counts)<- counts$X
counts <- counts[,-1]

#store counts data in DGElist data object with grouping factor
library(edgeR)
group = c("BS","BS","BS","BS","BS","M","M","M","M","M","V","V","V","V")
y <- DGEList(counts=counts, group=group)

#filter out lowly expressed genes
tpm <- read.csv("KitaakeLCM_TPM.csv")
row.names(tpm)<- tpm$X
tpm <- tpm[,-1]
keep <- rowSums(tpm > 1) >= 3
summary(keep) #True=15168
y <- y[keep,]

#calculating normalization factors and performing TMM normalization
y <- calcNormFactors(y, method = "TMM")
y$samples

#design matrix
design <- model.matrix(~0+group, data=y$samples)
colnames(design) <- levels(y$samples$group)
design

#Estimate common dispersion
y <- estimateGLMCommonDisp(y, design=design)

#Estimate gene-wise dispersion
y <- estimateGLMTrendedDisp(y, design=design)
y <- estimateGLMTagwiseDisp(y, design=design)

# Fit the linear model
fit <- glmQLFit(y, design)

#Conduct likelihood ratio tests for pairwise comparison and show the top genes
bsvsm <- glmQLFTest(fit, contrast=c(1,-1,0))
res.bsvsm <- topTags(bsvsm, n=20000)$table

mvsv <- glmQLFTest(fit, contrast=c(0,1,-1))
res.mvsv <- topTags(mvsv, n=20000)$table

bsvsv <- glmQLFTest(fit, contrast=c(1,0,-1))
res.bsvsv <- topTags(bsvsv, n=20000)$table

res.bsvsm$GENEID <- row.names(res.bsvsm)
res.mvsv$GENEID <- row.names(res.mvsv)
res.bsvsv$GENEID <- row.names(res.bsvsv)
head(res.bsvsm)

library (dplyr)
edgeR.res <- res.bsvsm[, c(1,5,6)] %>%
  left_join(res.mvsv[, c(1,5,6)], by = "GENEID" ) %>%
  left_join(res.bsvsv[, c(1,5,6)], by = "GENEID" )

names(edgeR.res) <- c("logFC.BSvsM", "FDR.BSvsM", "GENEID", 
                      "logFC.MvsV", "FDR.MvsV","logFC.BSvsV", "FDR.BSvsV")
head(edgeR.res)
write.csv(edgeR.res, "KitaakeLCM_edgeR_result.csv")


