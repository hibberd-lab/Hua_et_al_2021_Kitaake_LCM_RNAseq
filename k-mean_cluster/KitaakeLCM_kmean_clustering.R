setwd("F:/0001owncloud/001 Github/KitaakeLCM/Hua_et_al_Kitaake_LCM/k-mean_cluster")

require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
library(reshape2)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(gplots)
library(ComplexHeatmap)
library(circlize)
library(dendextend)

tpm <- read.csv("KitaakeLCM_TPM.mean.csv", header = T)
list1 <- read.csv("KitaakeLCM_15168expressed.genelist.csv", header = T)
list2 <- read.csv("KitaakeLCM.consensus.union.of.anypair.4155genes.csv", header = T)

# Calculate the mean for each cell type and keep only the mean values
data <- merge(tpm, list1, by = "GENEID")

##before quantile normalization
datm <-melt(data)
ggplot(datm, aes(log(value), group=variable))+
  geom_density()+
  xlim(-10,10)

#normalize the data using quantile normalization
library(preprocessCore)

testn<-as.data.frame(normalize.quantiles(as.matrix(data[,c(2:4)])))
colnames(testn)<-c("M","BS","V")
rownames(testn)<-data$GENEID

ggplot(melt(testn), aes(x=log(value), colour=variable)) +
  geom_density() +
  ggtitle("Quantile Normalization Log Density")

#log2 transformation after quantile normalization, save data for plotting
norm <- log2(testn+1)
write.csv(as.data.frame(norm), "KitaakeLCM_normaliseddata.csv")

#scale the normalised data of the 4155 DE genes
norm$GENEID <- row.names(norm)
data1 <- merge(norm, list2, by = "GENEID")
row.names(data1) <- data1$GENEID
data1 <- data1[, -1]
t_data <-t(data1)
z_data <- scale(t_data, center = TRUE, scale = TRUE)
tz_data <-t(z_data)
##### kmeans
#determining the number of clusters using the 4155 DE genes
wss <- (nrow(tz_data)-1)*sum(apply(tz_data,2,var))
head(wss)
for (i in 2:30) wss[i] <- sum(kmeans(tz_data, centers=i, iter.max = 100000)$withinss)
plot(wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#6 clusters were picked
set.seed(333)
fit <- kmeans(tz_data, 6, iter.max = 10000) 

# get cluster means 
aggregate(tz_data, by=list(fit$cluster),FUN=mean)

# append cluster assignment
m_te <- data.frame(tz_data, fit$cluster)
head(m_te)
m_te <-m_te[order(m_te$fit.cluster),] 
summary(m_te)
m_te$fit.cluster <- as.factor(m_te$fit.cluster)
m_te$GENEID <- rownames(m_te)
m_te$GENEID <-as.factor(m_te$GENEID)
table(m_te$fit.cluster)
m_te$cluster <- m_te$fit.cluster
library(dplyr)
library(stringr)
m_te$cluster <- m_te$cluster %>%
  str_replace_all("1", "CM") %>%
  str_replace_all("2", "CBS&V") %>%
  str_replace_all("3", "CBS") %>%
  str_replace_all("4", "CM&V") %>%
  str_replace_all("5", "CV") %>%
  str_replace_all("6", "CBS&M")


write.csv(m_te, "KitaakeLCM_kmean_clusters.csv")
