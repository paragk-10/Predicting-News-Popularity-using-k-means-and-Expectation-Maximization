rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)

#################################################################
#################################################################
#################################################################
#################################################################
#############       ANN for Shares Dataset    ###################
#################################################################
#################################################################
#################################################################
#################################################################

rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)

context = read.csv('OnlineNewsPopularity.csv')
summary(context)
sum(is.na(context))
context = context[,-c(1,2,38,39)]
summary(context)
context=context[!context$n_unique_tokens==701,]

context$shares = ifelse(context$shares>mean(context$shares),1,0)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

context1 <- context[,-57]
context1 <- as.data.frame(lapply(context1, normalize))

pca <- prcomp(context1)
pca
summary(pca)
screeplot(pca,type="lines") # looks like there are 2 principal components
factors <- pca$x[,1:17]
head(factors)
summary(factors)
cov(factors)

factors <- as.data.frame(factors)
shares1<-as.data.frame(context[,-c(1:56)])
names(shares1)<-print("shares")
factors<-cbind(factors,shares1)
set.seed(100)
split = sample.split(factors$shares,SplitRatio = 0.70)
training_set = subset(factors, split == TRUE)
test_set = subset(factors, split == FALSE)

training_set[,1:17] = scale(training_set[,1:17])
test_set[,1:17] = scale(test_set[,1:17])

#I manually ran code for different threshold values and hidden layers as mentoned in report
library(neuralnet)
nn <- neuralnet(shares ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17, data=training_set, hidden=c(3,1),act.fct="logistic",linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

pr.nn <- compute(nn,test_set[,1:17])
pr.nn_ <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)
test.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_set)
print(paste("Mean Square Error rate: ",MSE.nn))
print(paste("Prediction rate: ",1-MSE.nn))

set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  nn <- neuralnet(shares ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17,data=training_set,hidden=c(3,1),linear.output=F,threshold=0.5)   
  pr.nn <- compute(nn,test_set[,1:17])
  pr.nn <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  test.cv.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test_set)    
  pbar$step()
}
mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#################################################################
rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)
library(fastICA)

context = read.csv('OnlineNewsPopularity.csv')
summary(context)
sum(is.na(context))
context = context[,-c(1,2,38,39)]
summary(context)
context=context[!context$n_unique_tokens==701,]

context$shares = ifelse(context$shares>mean(context$shares),1,0)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

context1 <- context[,-57]
context1 <- as.data.frame(lapply(context1, normalize))

ica<-fastICA(context1, n.comp=5)
factors <- ica$S
head(factors)
summary(factors)
cov(factors)

factors <- as.data.frame(factors)
shares1<-as.data.frame(context[,-c(1:56)])
names(shares1)<-print("shares")
factors<-cbind(factors,shares1)
set.seed(100)
split = sample.split(factors$shares,SplitRatio = 0.70)
training_set = subset(factors, split == TRUE)
test_set = subset(factors, split == FALSE)

training_set[,1:5] = scale(training_set[,1:5])
test_set[,1:5] = scale(test_set[,1:5])

#I manually ran code for different threshold values and hidden layers as mentoned in report
library(neuralnet)
nn <- neuralnet(shares ~ V1 + V2 + V3 + V4 + V5, data=training_set, hidden=c(3,1),act.fct="logistic",linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

pr.nn <- compute(nn,test_set[,1:5])
pr.nn_ <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)
test.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_set)
print(paste("Mean Square Error rate: ",MSE.nn))
print(paste("Prediction rate: ",1-MSE.nn))

set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  nn <- neuralnet(shares ~ V1 + V2 + V3 + V4 + V5,data=training_set,hidden=c(3,1),linear.output=F,threshold=0.5)   
  pr.nn <- compute(nn,test_set[,1:5])
  pr.nn <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  test.cv.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test_set)    
  pbar$step()
}
mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#################################################################
rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)
library(rpca)

context = read.csv('OnlineNewsPopularity.csv')
summary(context)
sum(is.na(context))
context = context[,-c(1,2,38,39)]
summary(context)
context=context[!context$n_unique_tokens==701,]

context$shares = ifelse(context$shares>mean(context$shares),1,0)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

context1 <- context[,-57]
context1 <- as.data.frame(lapply(context1, normalize))

M <- as.matrix(context1[,1:56])
Mcent <- sweep(M,2,colMeans(M))
model_RPCA <- rpca(Mcent) 
with(model_RPCA$convergence,list(converged,iterations))
with(model_RPCA$convergence,final.delta)
rpc<-model_RPCA$L.svd$u%*%diag(model_RPCA$L.svd$d)
rpc

factors <- as.data.frame(rpc)
shares1<-as.data.frame(context[,-c(1:56)])
names(shares1)<-print("shares")
factors<-cbind(factors,shares1)
set.seed(100)
split = sample.split(factors$shares,SplitRatio = 0.70)
training_set = subset(factors, split == TRUE)
test_set = subset(factors, split == FALSE)

training_set[,1:27] = scale(training_set[,1:27])
test_set[,1:27] = scale(test_set[,1:27])

#I manually ran code for different threshold values and hidden layers as mentoned in report
library(neuralnet)
nn <- neuralnet(shares ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27, data=training_set, hidden=c(3,1),act.fct="logistic",linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

pr.nn <- compute(nn,test_set[,1:27])
pr.nn_ <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)
test.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_set)
print(paste("Mean Square Error rate: ",MSE.nn))
print(paste("Prediction rate: ",1-MSE.nn))

set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  nn <- neuralnet(shares ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27,data=training_set,hidden=c(3,1),linear.output=F,threshold=0.5)   
  pr.nn <- compute(nn,test_set[,1:27])
  pr.nn <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  test.cv.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test_set)    
  pbar$step()
}
mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#################################################################
rm(list=ls(all=TRUE))
library(data.table) 
library(caTools)
library(neuralnet)
library(Boruta)

context = read.csv('OnlineNewsPopularity.csv')
summary(context)
sum(is.na(context))
context = context[,-c(1,2,38,39)]
summary(context)
context=context[!context$n_unique_tokens==701,]

context$shares = ifelse(context$shares>mean(context$shares),1,0)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

context1 <- context[,-57]
context1 <- as.data.frame(lapply(context1, normalize))
shares1<-as.data.frame(context[,-c(1:56)])
names(shares1)<-print("shares")
context1<-cbind(context1,shares1)

FS1 <- Boruta(shares ~ .,data=context1, doTrace=2, ntree=50)  
getSelectedAttributes(FS1, withTentative = F)
features_stats <- attStats(FS1)
features_FS1 <- names(context)[(which(FS1$finalDecision=="Confirmed"))]
dataafter_FS1 <- subset( context1, select =c("LDA_01", "LDA_04", "n_non_stop_words", "n_unique_tokens", "self_reference_avg_sharess",	"self_reference_max_shares","n_tokens_content", "n_non_stop_unique_tokens"))

factors <- as.data.frame(dataafter_FS1)
shares1<-as.data.frame(context[,-c(1:56)])
names(shares1)<-print("shares")
factors<-cbind(factors,shares1)
set.seed(100)
split = sample.split(factors$shares,SplitRatio = 0.70)
training_set = subset(factors, split == TRUE)
test_set = subset(factors, split == FALSE)

training_set[,1:8] = scale(training_set[,1:8])
test_set[,1:8] = scale(test_set[,1:8])

#I manually ran code for different threshold values and hidden layers as mentoned in report
library(neuralnet)
nn <- neuralnet(shares ~ LDA_01+LDA_04+n_non_stop_words+n_unique_tokens+self_reference_avg_sharess+self_reference_max_shares+n_tokens_content+n_non_stop_unique_tokens, data=training_set, hidden=c(3,1),act.fct="logistic",linear.output=FALSE, threshold=0.5)
nn$result.matrix
plot(nn)

pr.nn <- compute(nn,test_set[,1:8])
pr.nn_ <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)
test.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_set)
print(paste("Mean Square Error rate: ",MSE.nn))
print(paste("Prediction rate: ",1-MSE.nn))

set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  nn <- neuralnet(shares ~ LDA_01+LDA_04+n_non_stop_words+n_unique_tokens+self_reference_avg_sharess+self_reference_max_shares+n_tokens_content+n_non_stop_unique_tokens,data=training_set,hidden=c(3,1),linear.output=F,threshold=0.5)   
  pr.nn <- compute(nn,test_set[,1:8])
  pr.nn <- pr.nn$net.result*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  test.cv.r <- (test_set$shares)*(max(factors$shares)-min(factors$shares))+min(factors$shares)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test_set)    
  pbar$step()
}
mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


#################################################################
#################################################################
