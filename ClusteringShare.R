# K-Means Clustering
rm(list=ls(all=TRUE))
library(cluster)
library(fpc)

# Importing the dataset
dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39,61)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset<-scale(dataset)
is.na(dataset)
dataset<-na.omit(dataset)

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i, nstart = 10)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 8, nstart=10)
y_kmeans = kmeans$cluster

plotcluster(dataset,y_kmeans)
# Visualising the clusters
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster plot'))

##########################

#Expectation Maximization in R
rm(list=ls(all=TRUE))
library(mclust)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset$shares=ifelse(dataset$share>3395,1,0)
dataset1<-dataset[,-57]
dataset1<-scale(dataset1)
dataset1<-data.frame(dataset1)
shares1<-as.data.frame(dataset[,-c(1:56)])
names(shares1)<-print("shares")
dataset1<-cbind(dataset1,shares1)
is.na(dataset1)
dataset1<-na.omit(dataset1)

#mb = Mclust(dataset1[,-57])
mb3 = Mclust(dataset1[,-57], 5)

summary(mb3, parameters = TRUE)
table(dataset1$shares, mb3$classification)
plot(mb3, what=c("classification"))
plot(mb3, "density")

##########################

rm(list=ls(all=TRUE))
library(cluster)
library(fpc)
#library(FactoMineR)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39,61)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset<-scale(dataset)
is.na(dataset)
dataset<-na.omit(dataset)

set.seed(123)
pca <- prcomp(dataset)
pca
summary(pca)
# pca1 = PCA(dataset, scale. = TRUE)
# pca1$eig
# pca1$var$coord
# head(pca1$ind$coord)

screeplot(pca,type="lines") # looks like there are 2 principal components
factors <- pca$x[,1:28]
head(factors)
summary(factors)
cov(factors)

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(factors, i, nstart=10)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = factors, centers = 8, nstart=10)
y_kmeans = kmeans$cluster

plotcluster(factors,y_kmeans)
# Visualising the clusters
clusplot(factors,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster Plot'))

##########################

rm(list=ls(all=TRUE))
library(cluster)
library(fpc)
library(fastICA)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39,61)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset<-scale(dataset)
is.na(dataset)
dataset<-na.omit(dataset)

ica<-fastICA(dataset, n.comp=5)
factors <- ica$S
head(factors)
summary(factors)
cov(factors)

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(factors, i, nstart=10)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = factors, centers = 8, nstart=10)
y_kmeans = kmeans$cluster

plotcluster(factors,y_kmeans)
# Visualising the clusters
clusplot(factors,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster Plot'))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls(all=TRUE))
library(cluster)
library(fpc)
library(Boruta)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset[,1:56]<-scale(dataset[,1:56])
is.na(dataset)
dataset<-na.omit(dataset)

FS1 <- Boruta(dataset$shares ~ .,data=dataset, doTrace=2, ntree=50)  
getSelectedAttributes(FS1, withTentative = F)
features_stats <- attStats(FS1)
features_FS1 <- names(dataset)[(which(FS1$finalDecision=="Confirmed"))]
summary(FS1)
print(FS1)

dataafter_FS1 <- subset( dataset, select =c("LDA_01", "LDA_04", "n_non_stop_words", "n_unique_tokens", "self_reference_avg_sharess",	"self_reference_max_shares","n_tokens_content", "n_non_stop_unique_tokens"))

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataafter_FS1, i, nstart=10)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataafter_FS1, centers = 7, nstart=10)
y_kmeans = kmeans$cluster

plotcluster(dataafter_FS1,y_kmeans)
# Visualising the clusters
clusplot(dataafter_FS1,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster Plot'))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls(all=TRUE))
library(cluster)
library(fpc)
library(rpca)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
dataset=dataset[!dataset$n_unique_tokens==701,]
summary(dataset)
is.na(dataset)
dataset<-na.omit(dataset)
dataset[,1:56]<-scale(dataset[,1:56])
set.seed(123)

M <- as.matrix(dataset[,1:56])
Mcent <- sweep(M,2,colMeans(M))
model_RPCA <- rpca(Mcent) 
with(model_RPCA$convergence,list(converged,iterations))
with(model_RPCA$convergence,final.delta)
rpc<-model_RPCA$L.svd$u%*%diag(model_RPCA$L.svd$d)
rpc
factors<-as.data.frame(rpc)
head(factors)
summary(factors)
cov(factors)

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(factors, i, nstart=10)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = factors, centers = 7, nstart=10)
y_kmeans = kmeans$cluster

plotcluster(factors,y_kmeans)
# Visualising the clusters
clusplot(factors,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster Plot'))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


rm(list=ls(all=TRUE))
library(mclust)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset$shares=ifelse(dataset$share>3395,1,0)
dataset1<-dataset[,-57]
dataset1<-scale(dataset1)
dataset1<-data.frame(dataset1)
shares1<-as.data.frame(dataset[,-c(1:56)])
names(shares1)<-print("shares")
dataset1<-cbind(dataset1,shares1)
is.na(dataset1)
dataset1<-na.omit(dataset1)

pca <- prcomp(dataset1[,-57])
pca
summary(pca)
screeplot(pca,type="lines") # looks like there are 2 principal components
factors <- pca$x[,1:28]
head(factors)
summary(factors)
cov(factors)

#mb = Mclust(factors)
mb3 = Mclust(factors, 5)

summary(mb3, parameters = TRUE)
table(dataset1$shares, mb3$classification)
plot(mb3, what=c("classification"))
plot(mb3, "density")

##########################

rm(list=ls(all=TRUE))
library(mclust)
library(fastICA)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset$shares=ifelse(dataset$share>3395,1,0)
dataset1<-dataset[,-57]
dataset1<-scale(dataset1)
dataset1<-data.frame(dataset1)
shares1<-as.data.frame(dataset[,-c(1:56)])
names(shares1)<-print("shares")
dataset1<-cbind(dataset1,shares1)
is.na(dataset1)
dataset1<-na.omit(dataset1)

ica<-fastICA(dataset[,-57], n.comp=8)
factors <- ica$S
head(factors)
summary(factors)
cov(factors)

#mb = Mclust(factors)
mb3 = Mclust(factors, 5)

summary(mb3, parameters = TRUE)
table(dataset1$shares, mb3$classification)
plot(mb3, what=c("classification"))
plot(mb3, "density")

##########################
rm(list=ls(all=TRUE))
library(Boruta)
library(mclust)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset$shares=ifelse(dataset$share>3395,1,0)
dataset1<-dataset[,-57]
dataset1<-scale(dataset1)
dataset1<-data.frame(dataset1)
shares1<-as.data.frame(dataset[,-c(1:56)])
names(shares1)<-print("shares")
dataset1<-cbind(dataset1,shares1)
is.na(dataset1)
dataset1<-na.omit(dataset1)

set.seed(123)
FS1 <- Boruta(shares ~ .,data=dataset1, doTrace=2, ntree=50)  
getSelectedAttributes(FS1, withTentative = F)
features_stats <- attStats(FS1)
features_FS1 <- names(dataset)[(which(FS1$finalDecision=="Confirmed"))]

dataafter_FS1 <- subset( dataset, select =c("LDA_01", "LDA_04", "n_non_stop_words", "n_unique_tokens", "self_reference_avg_sharess",	"self_reference_max_shares","n_tokens_content", "n_non_stop_unique_tokens"))
#mb = Mclust(dataafter_FS1)
mb3 = Mclust(dataafter_FS1, 5)

#summary(mb, parameters = TRUE)
summary(mb3, parameters = TRUE)
table(dataset1$shares, mb3$classification)
plot(mb3, what=c("classification"))
plot(mb3, "density")

##########################

rm(list=ls(all=TRUE))
library(rpca)
library(mclust)

dataset = read.csv('OnlineNewsPopularity.csv')
dataset = dataset[,-c(1,2,38,39)]
summary(dataset)
dataset=dataset[!dataset$n_unique_tokens==701,]
dataset$shares=ifelse(dataset$share>3395,1,0)
dataset1<-dataset[,-57]
dataset1<-scale(dataset1)
dataset1<-data.frame(dataset1)
shares1<-as.data.frame(dataset[,-c(1:56)])
names(shares1)<-print("shares")
dataset1<-cbind(dataset1,shares1)
is.na(dataset1)
dataset1<-na.omit(dataset1)

set.seed(123)
M <- as.matrix(dataset[,1:56])
Mcent <- sweep(M,2,colMeans(M))
model_RPCA <- rpca(Mcent) 
model_RPCA
with(model_RPCA$convergence,list(converged,iterations))
with(model_RPCA$convergence,final.delta)
rpc<-model_RPCA$L.svd$u%*%diag(model_RPCA$L.svd$d)
rpc
factors<-as.data.frame(rpc)
mb = Mclust(factors)
#mb3 = Mclust(dataafter_FS1, 3)

summary(mb, parameters = TRUE)
#summary(mb3, parameters = TRUE)
table(dataset1$shares, mb$classification)
plot(mb, what=c("classification"))
plot(mb, "density")